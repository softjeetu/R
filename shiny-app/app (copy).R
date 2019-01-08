# if (!require("shiny")) install.packages("shiny")
# if (!require("DT")) install.packages('DT')
library(shiny)
library(dplyr)
library(data.table)
library(DT)

# Churn Customers
churn_customers <- fread('data/final_churn.csv', select = c("gluserid","Mode","isdownloaded","company_name","state","total_enq","Total_enq_reply", "Zone"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # Navbar panel
  navbarPage("IM Reactivity App",
             tabPanel("Customer Churn"),
             tabPanel("Upsell")
  ),
  # app title ----
  
  titlePanel("IM Reactivity App"),
  
  # Sidebar layout with intpu & output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      radioButtons("type", "Choose Type:",
                   c("Zone" = "zone","Cust Id" = "gluserid"), 'zone', TRUE
      ),
      
      # Input: Selector for choosing dataset ----
      conditionalPanel(
        condition = "input.type == 'gluserid'",
        selectInput(inputId = "customer",
                    label = "Choose Customer:",
                    choices = unique(churn_customers[['gluserid']]))
      ),
      
      # Input: Selector for choosing dataset ----
      conditionalPanel(
        condition = "input.type == 'zone'",
        selectInput(inputId = "zone",
                  label = "Choose Zone:",
                  choices = unique(churn_customers[['Zone']]))
      ),
      
      # Input: Selector for number of rows to view ----
      
      # conditionalPanel(
      #   condition = "input.type == 'zone'",
      #   selectInput(inputId = "rows",
      #                label = "Row:",
      #                choices = c("All" = "-1", "10" = "10", "25" = "25", "50" = "50", "100" = "100"), '10')
      # ),
      
      # Button: to search ----
      actionButton("search", "Show", class = "btn-primary")
      #submitButton("Show", icon("refresh"))
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      # verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("results")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  observeEvent(input$search, {
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    if(input$type[1] == 'gluserid'){
      if (is.null(input$customer)) {
        return(NULL)
      }
      
      # filetering data
      filtered <- reactive({
        
        churn_customers %>%
          filter(isdownloaded == 1,
                 gluserid == input$customer
          )
      })
    }
    else if(input$type[1] == 'zone'){
      
      if (is.null(input$zone)) {
        return(NULL)
      }
      
      # if (is.null(input$rows)) {
      #   return(NULL)
      # }
      
      # filetering data
      filtered <- reactive({
        
        churn_customers %>%
          filter(isdownloaded == 1,
                 Zone == input$zone
          )
      })
    }
    
    #print(filtered()) 
    
    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    output$caption <- renderText({
      input$caption
    })
    
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    
    #output$summary <- renderPrint({
      #dataset <- datasetInput()
      #summary(dataset)
    #})
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    
    output$results <- renderTable({
      filtered()
    })
    
    # output$results <- DT::renderDataTable({
    #   data <- filtered()
    #   DT::datatable(
    #     data,
    #     rownames = FALSE,
    #     options = list(searching = FALSE, lengthChange = FALSE)
    #   )
    # })
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)
