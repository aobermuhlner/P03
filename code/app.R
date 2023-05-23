library(shiny)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(dplyr)
library(shinythemes)

source("data_reader.R")

# Frequency of drugnames
drug_freq <- table(DRUG$drugname)
# Sort by frequency (in decreasing order) and get the names of the drugs
unique_drugs<- names(drug_freq)[order(drug_freq, decreasing = TRUE)]

age_min <- min(DEMO$age, na.rm = TRUE)
age_max <- max(DEMO$age, na.rm = TRUE)

# The data frame doesn't exist, so therefore a handmade list is created
custom_column <- c("primaryid","caseid","drug_seq","drugname","prod_ai", "route","year","quarter","age", "sex","reporter_country","outcome_decoded", "indi_pt", "drug_rec_act")
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Side effects of medicaments"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "drug_select",
        label = "Select a drug",
        choices = NULL, # keep NULL, gets updated in server-side
        multiple = FALSE,  # TODO: make multiple choice compatible
        selected = NULL,
        # custom change in options
        options = list(
          placeholder = "Select a drug",
          maxOptions = 20
        , width = 2
        )
      ),
      selectInput(inputId = "sex_filter",
                  label = "Filter by sex",
                  choices = c("All", "Male" = "M", "Female" = "F"),
                  selected = "All"
      ),
      selectInput(inputId = "year_filter",
                  label = "Filter by year",
                  choices = c("All", "2022", "2023"),
                  selected = "All"
      ),
      sliderInput(inputId = "age_filter",
                  label = "Filter by age",
                  # TODO: set range to min() max() of demo$age
                  min = age_min,
                  max = age_max,
                  value = c(age_min, age_max)
      ),
      conditionalPanel(
        condition = "input.tabPanelId == 'plotTabId' && input.plots_tab == 'therapy_tab'", # JavaScript syntax
        selectInput(inputId = "therapy_filter",
                    label = "Filter by therapy duration",
                    choices = c("All", "Short term","Medium term", "Long term"),
                    selected = "All"
        )
      ),
      conditionalPanel(
        condition = "input.tabPanelId == 'tableTabId' && input.table_tab == 'data_table_tab'", # JavaScript syntax
        checkboxGroupInput(inputId = "df_column_filter",
                           label = "Columns in table to show:",
                           choices = custom_column,
                           selected = custom_column
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabPanelId",
        tabPanel("Plots",
                 value = "plotTabId",
                 conditionalPanel(
                   condition = "input.drug_select", # JavaScript syntax
                   tabsetPanel(
                     id = "plots_tab",
                     tabPanel("Reports per Quarter", plotOutput("reports_per_quarter_plot")),
                     tabPanel("Reports per Sequence", plotOutput("reports_per_sequence_plot")),
                     tabPanel("Therapy Duration", value="therapy_tab", plotOutput("therapy_durations_plot")),
                     tabPanel("Top 10 Indications", plotOutput("top_indications_plot")),
                     tabPanel("Outcome Distribution", plotOutput("outcome_distribution_plot")),
                     tabPanel("Drug Reaction", plotOutput("drug_reaction_plot")),
                     tabPanel("Medication mix", plotOutput("medication_mix_plot")),
                     tabPanel("Top 10 Manufacturers", plotOutput("top_manufacturers_plot"))
                     
                   )
                 )
                 , width = 10
        ),
        tabPanel("Data Table",
                 value = "tableTabId",
                 tabsetPanel(id="table_tab",
                 tabPanel("Filtered Drug Table",value="data_table_tab", dataTableOutput("filtered_drug_table")))
        )
      )
    )
  )
)



# -----------------------------------------------------------------------------
# SERVER-SIDE
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  updateSelectizeInput(session,
                       inputId = 'drug_select',
                       selected = "",
                       choices = unique_drugs,
                       # Handles choices server side, improves performance
                       server = TRUE, 
                       options = list(
                         placeholder = "Select a drug",
                         maxOptions = 20
                       ))
  
  final_data <- reactive({
    data <- join_data(v_drugname = input$drug_select,
              v_sex = input$sex_filter,
              v_age_min = input$age_filter[1],
              v_age_max = input$age_filter[2],
              v_year = input$year_filter)
    return(data)

  })
  
  output$filtered_drug_table <- renderDataTable({
    data <- final_data()
    new_df <- data %>%
      select(input$df_column_filter)
    return(new_df)
    
  })
  
  # Every plot outsourced and handled in data_reader.R
  
  # Render reports per quarter plot
  output$reports_per_quarter_plot <- renderPlot({
    plot_reports_per_quarter(final_data())
  })

  # Render reports per sequence plot
  output$reports_per_sequence_plot <- renderPlot({
    plot_reports_per_sequence(final_data())
  })

  # Render therapy duration plot
  output$therapy_durations_plot <- renderPlot({
    plot_therapy_durations(final_data(), input$therapy_filter)
  })

  # Render top 10 indications plot
  output$top_indications_plot <- renderPlot({
    plot_top_indications(final_data())
  })

  # Render outcome distribution plot
  output$outcome_distribution_plot <- renderPlot({
    plot_outcome_distribution(final_data())
  })
  
  # Render outcome drug_reaction_plot
  output$drug_reaction_plot <- renderPlot({
    plot_drug_reaction(final_data())
  })
  
  # Render outcome Medication mix
  output$medication_mix_plot <- renderPlot({
    plot_prod_ai_distribution(final_data())
  })
  
  # Render outcome Medication mix
  output$ top_manufacturers_plot <- renderPlot({
    plot_manufactorer_distribution(final_data())
  })
 
}

# -----------------------------------------------------------------------------

# Run the app
shinyApp(ui, server)
