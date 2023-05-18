library(shiny)
library(shinyWidgets)
library(data.table)
library(ggplot2)

source("data_reader.R")

# UI (OR CLIENT-SIDE, I THINK)
# -----------------------------------------------------------------------------
# Frontend basically
# UI, what button or widget should be displayed and how
# R functions to generate HTML, CSS and JavaScript code
ui <- fluidPage(
    fluidPage(
      titlePanel("Nebenwirkung von Medikamente"),
      
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
                   # Limit available options
                   maxOptions = 5
                 )
               ),
        ),
        column(4,
               # age filter, slider returns value to server-side
               sliderInput(inputId = "age_filter",
                           label = "Filter by age",
                           # TODO: set range to min() max() of demo$age
                           min = 0,
                           max = 120,
                           value = c(0, 120)),
        ),
        column(4,
               # sex filter
               selectInput(inputId = "sex_filter",
                           label = "Filter by sex",
                           # TODO: remove "unknown" aka NA values
                           choices = c("F", "M"),
                           selected = ""),
        )
      ),
      mainPanel(
        id = "main-panel",
        tabsetPanel(
          tabPanel("Reports per Quarter", plotOutput("reports_per_quarter_plot")),
          tabPanel("Reports per Sequence", plotOutput("reports_per_sequence_plot")),
          tabPanel("Therapy Duration", plotOutput("therapy_durations_plot")),
          tabPanel("Top 10 Indications", plotOutput("top_indications_plot")),
          tabPanel("Outcome Distribution", plotOutput("outcome_distribution_plot")),
          tabPanel("Filtered Drug Table", dataTableOutput("filtered_drug_table"))
        )
      )
    ),
)

# -----------------------------------------------------------------------------
# SERVER-SIDE
# -----------------------------------------------------------------------------

# Backend, computes logic, data processing and interactions
# Uses for example reactive expressions, event handlers and other server-side func
server <- function(input, output, session) {
  # input accesses the list of all inputId from UI-side
  # output is a list of outputs that will be displayed in the UI
  # session, current interaction link to client from server-side

  # Updates selection choice server sided for improved performance
  updateSelectizeInput(session,
                       inputId = 'drug_select',
                       selected = NULL,
                       choices = unique_drugs,
                       # Handles choices server side, improves performance
                       server = TRUE)
  
  # "reactive" expressions store intermediate results, perform calculations and filter data
  # if (filter) value changes, the current expression gets invalidated
  # and re-exceuted to output the new value
  final_data <- reactive({
    
    # # TODO: might do the merge outside of server/ ui to improve performance
    # filtered_data <- merge(drug, demo_dt, by = "primaryid")
    # # Filter the merged data by age
    # filtered_data <- filtered_data[age >= input$age_filter[1] & age <= input$age_filter[2], ]
    # # Filter by sex
    # if (input$sex_filter != "") {
    #   filtered_data <- filtered_data[sex == input$sex_filter, ]
    # }
    # 

    join_data(input$drug_select, input$sex_filter, input$age_filter[1], input$age_filter[2])
  })
  
  output$filtered_drug_table <- renderDataTable({
    final_data()
  })
  
  # Render reports per quarter plot
  output$reports_per_quarter_plot <- renderPlot({
    data <- final_data()
    reports_per_quarter <- num_reports_per_quarter(data)
    ggplot(reports_per_quarter, aes(x = YearQuarter, y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Reports per Quarter", x = "Quarter", y = "Number of Reports") +
      theme_minimal()
  })
  
  # Render reports per sequence plot
  output$reports_per_sequence_plot <- renderPlot({
    # data <- final_data()
    reports_per_sequence <- num_reports_per_sequence(final_data())
    ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Reports per Sequence", x = "Sequence", y = "Number of Reports") +
      theme_minimal()
  })
  
  # Render therapy duration plot
  output$therapy_durations_plot <- renderPlot({
    data <- final_data()
    therapy_durations <- calc_therapy_duration(data)
    
    # Create a data frame from the vector of therapy durations
    therapy_df <- data.frame(duration = therapy_durations)
    
    ggplot(therapy_df, aes(x = duration)) +
      geom_histogram(binwidth = 1) +   # You might need to adjust binwidth
      labs(title = "Distribution of Therapy Duration", x = "Duration", y = "Frequency") +
      theme_minimal()
  })
  
  # Render top 10 indications plot
  output$top_indications_plot <- renderPlot({
    data <- final_data()
    top_indications_data <- top_indications(data)
    ggplot(top_indications_data, aes(x = reorder(drug_seq, -N), y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Indications", x = "Indication", y = "Number of Reports") +
      theme_minimal()
  })
  
  # Render outcome distribution plot
  output$outcome_distribution_plot <- renderPlot({
    data <- final_data()
    outcome_distribution_data <- outcome_distribution(data)
    ggplot(outcome_distribution_data, aes(x = outc_cod, y = N)) +
      geom_bar(stat = "identity") +
      labs(title = "Outcome Distribution", x = "Outcome Code", y = "Number of Outcomes") +
      theme_minimal()
  })
}

# -----------------------------------------------------------------------------

# Run the app
shinyApp(ui, server)
