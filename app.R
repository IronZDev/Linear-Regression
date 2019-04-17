#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  
  # Application title
  titlePanel("Linear Regression"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file", 
                label = h3("Provide .csv file"),
                multiple = FALSE,
                accept = c(".dat")
      ),
      disabled(
        selectInput("select", label = h3("Choose target category: "), 
                    choices = NULL, 
                    selected = -1),
        actionButton("startBtn", "Calculate!")
      )
      # hr(),
      # fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("fileContent")
      # plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data = reactiveVal(0) # Global variable for content of csv
  
  observeEvent(input$file, {
    if (is.null(input$file)) {
      disable("select")
      disable("startBtn")
      return(NULL)
    }
    
    # Check if file has .dat extension
    extension = strsplit(input$file$name, "\\.")
    if (extension[[1]][2] != "csv") {
      alert("Wrong file format!")
      reset("file")
      disable("select")
      disable("startBtn")
      return(NULL)
    }
    content = read.csv(file = input$file$datapath, header = TRUE, sep=",")
    colnames(content) <- gsub(".", " ", colnames(content), fixed=TRUE) # Replace "." with " " in column names
    updateSelectInput(session, "select", NULL, choices = colnames(content)) # Update options in selectInput
    data(content) # Save to reactive variable
    enable("select")
    enable("startBtn")
  })
  
  observeEvent(input$startBtn, {
    print(data()[[input$select]])
  })
  
  # output$fileContent <- renderTable({
  #   # Check if file exists
  #   if (is.null(input$file))
  #     return(NULL)
  #   
  #   # Check if file has .dat extension
  #   extension = strsplit(input$file$name, "\\.")
  #   if (extension[[1]][2] != "csv") {
  #     print("Wrong file format!")
  #     return(NULL)
  #   }
  #   
  #   return(read.csv(file = input$file$datapath, header = TRUE, sep=","))
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

