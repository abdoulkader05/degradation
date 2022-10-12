source("modif.R")

library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      textOutput("top"),
      plotOutput("dataCurve")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  filecsv <- reactive({
    req(input$file1)
    pathh <- input$file1
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return (df)
  })
  
  increment <- reactive({
    dt <- filecsv()
    n = length(dt)
    l = nrow(dt)
    for (i in 2:n){
      for (j in 2:l){
        dt[i, j] = dt[i, j] - dt[i- 1, j]
      }
    }
    return (dt)
  })
  output$dataCurve<- renderPlot({
    notre_data <- increment()
    l = ncol(notre_data)
    plot(notre_data[,1], notre_data[,2], type = "l")
    for (k in 3:l){
      lines(notre_data[,1], notre_data[,k])
    }
    #ggplot(data = notre_data, aes(notre_data[,1], notre_data[,-1], fill = "blue")) + geom_line()
    #plot(notre_data[,1], notre_data[-1])
  })
output$top <- 
  renderText({
    names(filecsv())
  })
}

# Create Shiny app ----
shinyApp(ui, server)


