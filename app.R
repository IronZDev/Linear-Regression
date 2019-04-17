#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Linear Regression"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file", 
                  label = h3("File input"),
                  multiple = FALSE,
                  accept = c(".dat")
        ),
        selectInput("select", label = h3("Select box"), 
                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                    selected = 1)
        
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
server <- function(input, output) {
  
   output$fileContent <- renderTable({
     # Check if file exists
     if (is.null(input$file))
       return(NULL)
     
     # Check if file has .dat extension
     extension = strsplit(input$file$name, "\\.")
     if (extension[[1]][2] != "csv") {
       print("Wrong file format!")
       return(NULL)
     }
     
     return(read.csv(file = input$file$datapath, header = TRUE, sep=","))
   })
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

