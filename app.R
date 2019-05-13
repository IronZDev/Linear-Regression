#
# Intelligent Systems
# Deusto University
# Maciej Stokfisz, 2019
#

library(shiny)
library(shinyjs)
library(shinyalert)
library(caret)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(), # Using shinyJS for disabling and enabling elements
  useShinyalert(), # For pretty alerts
  
  # Application title
  titlePanel("Linear Regression"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # Upload box
      fileInput("file", 
                label = h3("Provide .csv file"),
                multiple = FALSE,
                accept = c(".dat")
      ),
      # Select category
      disabled(
        selectInput("select", label = h3("Choose target category: "), 
                    choices = NULL, 
                    selected = -1),
        actionButton("startBtn", "Calculate!")
      )
    ),
    
    # Show calculated mean absolute error + graph
    mainPanel(
      h3(textOutput("MAE"), align="center"),
      plotOutput("maePlot")
    )
  )
)
a

server <- function(input, output, session) {
  data = reactiveVal(0) # Global variable for content of csv
  
  # Updating UI
  observeEvent(input$file, {
    # If not input file then disable category select and start button
    if (is.null(input$file)) {
      disable("select")
      disable("startBtn")
      return(NULL)
    }
    
    
    # Check if file has .csv extension
    extension = strsplit(input$file$name, "\\.")
    if (extension[[1]][2] != "csv") {
      reset("select")
      reset("file")
      delay(100, disable("select")) # Delay to make sure that it will be resetted first
      disable("startBtn")
      shinyalert("Wrong file format!", type="error")
      return(NULL)
    }
    
    content = read.csv(file = input$file$datapath, header = TRUE, sep=",")
    colnames(content) <- gsub(".", " ", colnames(content), fixed=TRUE) # Replace "." with " " in column names
    updateSelectInput(session, "select", NULL, choices = colnames(content)) # Update options in selectInput
    data(content) # Save to reactive variable
    enable("select")
    enable("startBtn")
  })
  
  # Start button clicked event
  observeEvent(input$startBtn, {
    # Generate indices for partitioning
    indices = createMultiFolds(y=data()[[input$select]],5,1)
    MAEs = vector() # Vector of MAEs
    for (index in  1:length(indices)) {
      # Do partitioning
      training.data = data()[indices[[index]], ] 
      test.data = data()[-indices[[index]], ]
      target = training.data[[input$select]]
      training.data[[input$select]] = NULL # Delete the column to be predicted from taining data
      # print(cor(training.data))
      model = lm(target~., training.data)
      # print(summary(model))
      predictions = predict(model, test.data)
      # Calcuate MAE and add to vector
      MAE = mean(abs(predictions - test.data[[input$select]]))
      MAEs = c(MAEs, MAE)
    }
    # print(mean(MAEs))
    
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

