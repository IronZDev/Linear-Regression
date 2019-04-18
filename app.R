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
library(caret)
library(ggplot2)

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
      h3(textOutput("MAE"), align="center"),
      plotOutput("maePlot")
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
    indices = createMultiFolds(y=data()[[input$select]],5,1)
    MAEs = vector() # Vector of MAEs
    for (index in  1:length(indices)) {
      training.data = data()[indices[[index]], ] 
      test.data = data()[-indices[[index]], ]
      target = training.data[[input$select]]
      training.data[[input$select]] = NULL # Delete the column to be predicted from taining data
      model = lm(target~., training.data)
      predictions = predict(model, test.data)
      MAE = mean(abs(predictions - test.data[[input$select]]))
      print(MAE)
      MAEs = c(MAEs, MAE)
    }
    # print(mean(MAEs))
    output$category = renderText({
      return(input$select)
    })
    output$MAE = renderText({
      return(paste0("Overall mean absolute error: ",mean(MAEs)))
    })
    output$maePlot = renderPlot({
      plotData = data.frame(num = 1:length(MAEs), MAE = MAEs)
      ggplot(plotData, aes(x=num, y=MAE))+
        geom_bar(stat="identity", fill="steelblue")+
        geom_text(aes(label=sprintf("%0.6f", round(MAE, digits = 6))), position = position_stack(vjust = 0.5), color="white", size=5)+
        xlab("Number of fold as test data") + ylab("Average mean value")+
        ggtitle("Average mean values for particular folds as test data")+
        theme(plot.title = element_text(hjust = 0.5, size=20, face="bold"),
              axis.title = element_text(size=15, face="bold"),
              axis.text = element_text(size=12))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

