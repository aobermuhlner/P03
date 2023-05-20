library(shiny)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(dplyr)

source("data_reader.R")

# Frequency of drugnames
drug_freq <- table(DRUG$drugname)
# Sort by frequency (in decreasing order) and get the names of the drugs
unique_drugs<- names(drug_freq)[order(drug_freq, decreasing = TRUE)]

age_min <- min(DEMO$age, na.rm = TRUE)
age_max <- max(DEMO$age, na.rm = TRUE)


custom_column <- c("primaryid","caseid","drug_seq","drugname","prod_ai", "route","year","quarter","age", "sex","reporter_country","outcome_decoded", "indi_pt", "drug_rec_act")
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
    fluidPage(
      titlePanel("Side effects of medicaments"),
      
      # Create a new Row in the UI for selectInputs
      fluidRow(
        column(4,
               selectizeInput(
                 inputId = "drug_select",
                 label = "Select a drug",
                 choices = NULL, # keep NULL, gets updated in server-side
                 multiple = FALSE,  # TODO: make multiple choice compatible
                 selected = NULL,
                 # custom change in options
                 options = list(
                   placeholder = "Select a drug",
                   maxOptions = 10
                 )
               ),
        ),
        column(4,
               # age filter, slider returns value to server-side
               sliderInput(inputId = "age_filter",
                           label = "Filter by age",
                           # TODO: set range to min() max() of demo$age
                           min = age_min,
                           max = age_max,
                           value = c(age_min, age_max)),
        ),
        column(4,
               # sex filter
               selectInput(inputId = "sex_filter",
                           label = "Filter by sex",
                           choices = c("All", "Male" = "M", "Female" = "F"),
                           selected = "All"),
        ),
        column(4,
               # year filter
               selectInput(inputId = "year_filter",
                           label = "Filter by year",
                           choices = c("All", "2022", "2023"),
                           selected = "All"),
        ),
        conditionalPanel(
          condition = "input.tabPanelId == 'therapy_tab'", # JavaScript syntax
          column(4,
                 selectInput(inputId = "therapy_filter",
                             label = "Filter by therapy duration",
                             choices = c("All", "Short term","Medium term", "Long term"),
                             selected = "All")
          )
        ),
        conditionalPanel(
          condition = "input.tabPanelId == 'table_tab'", # JavaScript syntax
          column(4,
                 checkboxGroupInput(inputId = "df_column_filter",
                                    label = "Columns in table to show:",
                                    choices = custom_column,
                                    selected = custom_column
          )
        )
      ),
      conditionalPanel(
        condition = "input.drug_select", # JavaScript syntax
        mainPanel(
          id = "main-panel",
          tabsetPanel(
            id = "tabPanelId",
            tabPanel("Reports per Quarter", plotOutput("reports_per_quarter_plot")),
            tabPanel("Reports per Sequence", plotOutput("reports_per_sequence_plot")),
            tabPanel("Therapy Duration", value="therapy_tab", plotOutput("therapy_durations_plot")),
            tabPanel("Top 10 Indications", plotOutput("top_indications_plot")),
            tabPanel("Outcome Distribution", plotOutput("outcome_distribution_plot")),
            tabPanel("Drug Reaction", plotOutput("drug_reaction_plot")),
            tabPanel("Medication mix", plotOutput("medication_mix_plot")),
            tabPanel("Top 10 Manufacturers", plotOutput("top_manufacturers_plot")),
            tabPanel("Filtered Drug Table", value="table_tab", dataTableOutput("filtered_drug_table"))
          )
        )
      )
      # ,
      # conditionalPanel(
      #   condition = "!input.drug_select",
      #   textOutput("drug_select_message") # Server will need to output this message.
      # )
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
                         maxOptions = 10
                       ))
  
  final_data <- reactive({
    data <- join_data(v_drugname = input$drug_select,
              v_sex = input$sex_filter,
              v_age_min = input$age_filter[1],
              v_age_max = input$age_filter[2],
              v_year = input$year_filter)
    print(input$drug_select)
    return(data)

  })
  
  output$filtered_drug_table <- renderDataTable({
    data <- final_data()
    new_df <- data %>%
      select(input$df_column_filter)
    return(new_df)
    
  })
  
  # output$drug_select_message <- renderText({
  #   "Select drug"
  # })
  
  # Render reports per quarter plot
  output$reports_per_quarter_plot <- renderPlot({
    reports_per_quarter <- num_reports_per_quarter(final_data())
    ggplot(reports_per_quarter, aes(x = quarter, y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Reports per Quarter", x = "Quarter", y = "Number of Reports") +
      theme_minimal()
  })

  # Render reports per sequence plot
  output$reports_per_sequence_plot <- renderPlot({
    reports_per_sequence <- num_reports_per_sequence(final_data())
    ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Reports per Sequence", x = "Sequence", y = "Number of Reports") +
      theme_minimal()
  })

  # Render therapy duration plot
  output$therapy_durations_plot <- renderPlot({
    therapy_durations <- calc_therapy_duration_relative(final_data(),input$therapy_filter)

    # Create a data frame from the vector of therapy durations
    therapy_df <- data.frame(duration = therapy_durations)

    ggplot(therapy_df, aes(x = duration)) +
      geom_histogram(binwidth = 1) +   # You might need to adjust binwidth
      labs(title = "Distribution of Therapy Duration", x = "Duration", y = "Frequency") +
      theme_minimal()
  })

  # Render top 10 indications plot
  output$top_indications_plot <- renderPlot({
    top_indications_data <- top_indications(final_data())
    ggplot(top_indications_data, aes(x = reorder(indi_pt, -N), y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Indications", x = "Indication", y = "Number of Reports") +
      theme_minimal()
  })

  # Render outcome distribution plot
  output$outcome_distribution_plot <- renderPlot({
    outcome_distribution_data <- outcome_distribution(final_data())
    ggplot(outcome_distribution_data, aes(x = outcome_decoded, y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Outcome Distribution", x = "Outcome", y = "Number of Outcomes") +
      theme_minimal()
  })
  
  # Render outcome drug_reaction_plot
  output$drug_reaction_plot <- renderPlot({
    # ENTER PLOT CODE HERE
  })
  
  # Render outcome Medication mix
  output$medication_mix_plot <- renderPlot({
    # ENTER PLOT CODE HERE
  })
  
  # Render outcome Medication mix
  output$ top_manufacturers_plot <- renderPlot({
    # ENTER PLOT CODE HERE
  })
 
}

# -----------------------------------------------------------------------------

# Run the app
shinyApp(ui, server)
