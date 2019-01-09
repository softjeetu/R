#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# if (!require("shiny")) install.packages("shiny")
# if (!require("DT")) install.packages('DT')
library(shiny)
library(dplyr)
library(data.table)
library(DT)

# Churn Customers
churn_customers <- fread('data/final_churn.csv', select = c("gluserid","company_name","Mode","mapped_mcats","total_bl_pur","last_3m_bl_pur", "isdownloaded", "state", "Zone"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Navbar panel
  navbarPage("IM Customer Churn App",
             tabPanel("Customer Churn")
  ),
  
  # app title ----
  titlePanel("IM Customer Churn App"),
  
  # Sidebar layout with intpu & output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a type ----
      # Note: Changes made to the caption in the radioInput control
      radioButtons("type", "Choose Type:",
                   c("Zone" = "zone","Cust Id" = "gluserid"), 'zone', TRUE
      ),
      
      # Input: Selector for choosing customer ----
      conditionalPanel(
        condition = "input.type == 'gluserid'",
        selectInput(inputId = "customer",
                    label = "Choose Customer:",
                    choices = unique(churn_customers[['gluserid']]))
      ),
      
      # Input: Selector for choosing zone ----
      conditionalPanel(
        condition = "input.type == 'zone'",
        selectInput(inputId = "zone",
                  label = "Choose Zone:",
                  choices = unique(churn_customers[['Zone']]))
      ),
      
      # Button: to search ----
      actionButton("search", "Show", class = "btn-primary")
      #submitButton("Show", icon("refresh"))
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      #Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      dataTableOutput("results")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  observeEvent(input$search, {
    
    # Return the requested dataset ----
    # By declaring dataset as a reactive expression we ensure
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
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    output$caption <- renderText({
      "Summary & Filtered Result(s)"
    })
    
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    
    output$summary <- renderPrint({
      dataset <- filtered()
      summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    
    # output$results <- renderTable({
    #   filtered()
    # })
    
    output$results <- DT::renderDataTable({
      data <- filtered()
      DT::datatable(
        data,
        rownames = FALSE,
        colnames = c('Customer Id', 'Company Name', 'Mode', 'MCAT count', 'BLs purchased', 'BLs purchased ( Lst 3 M)','isdownloaded',"State", "Zone"),
        extensions = 'Buttons',
        options = list(columnDefs = list(list(visible=FALSE, targets=c(6))), 
                       dom = '<"text-center"<"btn-group"B>><"clear"><"row"<"col-md-6"l><"col-md-6 text-right"f>r>t<"row"<"col-md-6"i><"col-md-6"p>><"clear">',
                       buttons = c('excel', 'pdf', 'print')
                  )
      )
    })
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)
