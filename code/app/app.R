library(shiny)
library(shinyWidgets)
library(data.table)



# -----------------------------------------------------------------------------
# THIS GETS LOADED ONCE
# CHANGE WD OR PATH IF NOT WORKING
drug <- fread("DT/DRUG.dt") # drug name
demo_dt <- fread("DT/DEMO.dt") # sex / age

# Every unique drug name saved to a data.table
drug_table <- data.table(drug_names = unique(drug$drugname))

# -----------------------------------------------------------------------------



# UI (OR CLIENT-SIDE, I THINK)
# -----------------------------------------------------------------------------
# Frontend basically
# UI, what button or widget should be displayed and how
# R functions to generate HTML, CSS and JavaScript code
ui <- fluidPage(
    
    # Application title
    titlePanel("Nebenwirkung von Medikamente"),
    
    # age filter, slider returns value to server-side
    sliderInput(inputId = "age_filter",
                label = "Filter by age",
                # TODO: set range to min() max() of demo$age 
                min = 0,
                max = 120,
                value = c(0, 120)),
    # sex filter
    selectInput(inputId = "sex_filter",
                label = "Filter by sex",
                # TODO: remove "unkown" aka NA values
                choices = unique(demo_dt$sex),
                selected = ""),
    
    # drug name filter
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
    # Main Panel
      mainPanel(
        # Display selected drug created in server
        verbatimTextOutput('selected_drug'),
        # Add a table to display the filtered data
        dataTableOutput("filtered_drug_table")
      )
)

# -----------------------------------------------------------------------------

# SERVER-SIDE
# -----------------------------------------------------------------------------

# Backend, computes logic, data processing and interactions
# Uses for example reactive expressions, event handlers and other server-side func
server <- function(input, output, session) {
  # input accesses the list of all inputId from UI-side
  # output is a list of outputs that will be dispalyed in the UI
  # session, current interaction link to client from server-side
  

  

  # Updates selection choice server sided for improved performance
  updateSelectizeInput(session,
                       inputId = 'drug_select',
                       selected = NULL,
                       choices = drug_table$drug_names,
                       # Handles choices server side, improves performance
                       server = TRUE)
  
  # "reactive" expressions store intermediate results, perform calculations and filter data
  # if (filter) value changes, the current expression gets invalidated
  # and re-exceuted to output the new value
  
  # "observe" expressions, on the other hand checks if a reactive value changes and executes functions / logic
  # it's a responder to the reactive exp, useful for plots when value change
  filtered_data <- reactive({
    
    # TODO: might do the merge outside of server/ ui to improve performance
    filtered_data <- merge(drug, demo_dt, by = "primaryid")
    # Filter the merged data by age
    filtered_data <- filtered_data[age >= input$age_filter[1] & age <= input$age_filter[2], ]
    # Filter by sex
    if (input$sex_filter != "") {
      filtered_data <- filtered_data[sex == input$sex_filter, ]
    }
    
    # Filter by drug name
    if (input$drug_select != "") {
      filtered_data <- filtered_data[drugname == input$drug_select, ]
    }
    return(filtered_data)
  })
  
  # Test only, obsolete, delete in the final product
  # and remove in client verbatimTextOutput
  # ---------------------------------------------
  
  # Creates selected text
  # output returns value to UI
  output$selected_drug <- renderText({
    paste('You selected',
          if (input$drug_select == '') 'nothing'
          else input$drug_select,
          'in the drug example.')
    })
  # ---------------------------------------------
  
  
  # Render the filtered data table
  output$filtered_drug_table <- renderDataTable({
    filtered_data()
  })
  
}

# -----------------------------------------------------------------------------

# Run the app
shinyApp(ui, server)
