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

# Upsell
upsell <- fread('data/final_upsell.csv', select = c("GLUSR_USR_ID","MDC_TYPE","TOT_BLS_PURCHASED_3", "TTL_CALLS_3", "Queries_CNT_3","Upsl_status", "Zone"), showProgress=TRUE)

# Define UI for app ----
ui <- fluidPage(
  
  # Navbar panel
  navbarPage("IM Upsell App",
             tabPanel("Upsell")
  ),
  
  # app title ----
  titlePanel("IM Upsell App"),
  
  # Sidebar layout with intpu & output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a type ----
      # Note: Changes made to the type in the radioInput control
      radioButtons("type", "Choose Type:",
                   c("Zone" = "zone","Cust Id" = "gluserid"), 'zone', TRUE
      ),
      
      # Input: Selector for choosing customer ----
      conditionalPanel(
        condition = "input.type == 'gluserid'",
        selectInput(inputId = "customer",
                    label = "Choose Customer:",
                    choices = unique(upsell[['GLUSR_USR_ID']]))
      ),
      
      # Input: Selector for choosing zone ----
      conditionalPanel(
        condition = "input.type == 'zone'",
        selectInput(inputId = "zone",
                    label = "Choose Zone:",
                    choices = unique(upsell[['Zone']]))
      ),
      
      # Button: to search ----
      actionButton("search", "Show", class = "btn-primary")
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
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
        
        upsell %>%
          filter(Upsl_status == 1,
                 GLUSR_USR_ID == input$customer
          )
      })
    }
    else if(input$type[1] == 'zone'){
      
      if (is.null(input$zone)) {
        return(NULL)
      }
      
      # filetering data
      filtered <- reactive({
        
        upsell %>%
          filter(Upsl_status == 1,
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
      if(is.null(filtered())){
        return(NULL)
      }
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
        colnames = c('Customer Id', 'Mode', 'BLs purchased ( Lst 3 M)', 'Total Calls (Lst 3 M)','Total Queries (Lst 3 M)',"Upsell Status", "Zone"),
        extensions = 'Buttons',
        options = list(dom = '<"text-center"<"btn-group"B>><"clear"><"row"<"col-md-6"l><"col-md-6 text-right"f>r>t<"row"<"col-md-6"i><"col-md-6"p>><"clear">',
                       buttons = c('excel', 'pdf', 'print')
        )
      )
    })
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)

