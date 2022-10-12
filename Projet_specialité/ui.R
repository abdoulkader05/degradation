#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)

# Define UI
shinyUI(fluidPage(theme = shinytheme("yeti"),
                  useShinyjs(),
                  navbarPage(
                    
                    "",
                    tabPanel("Simulation",
                             sidebarPanel(
                               tags$h1("Parameters :"),
                               selectInput(inputId = "ModelChoice", label = "Select a model", choices = c("Wiener Process", "Gamma Process")),
                               conditionalPanel(condition = 'input.ModelChoice == "Gamma Process"', numericInput("a", "a :", 1), numericInput("b", "b :", 1)),
                               conditionalPanel(condition = 'input.ModelChoice ==  "Wiener Process"',
                                                numericInput("mu", "mu :", 1,min = 0 ),
                                                numericInput("sigma2", "sigma2:", 1, min = 0)),
                               selectInput("dtchoix" , label = "Type of distribution for the time ", choices = c("Periodic" , "Exponential", "Gamma", "Normal" )),
                               conditionalPanel(condition = "input.dtchoix == 'Periodic'", numericInput("deltat", " Period:", 0.1)),
                               conditionalPanel(condition =  "input.dtchoix == 'Exponential'", numericInput("Lambda", "lambda :", 1)),
                               conditionalPanel(condition = "input.dtchoix == 'Gamma'", numericInput("aa", "a :", 1), numericInput("bb", "b :", 1)),
                               conditionalPanel(condition = "input.dtchoix == 'Normal'", numericInput("mean", "mu:", 1), numericInput("sigma_carre", "sigma2:", 1)),
                               numericInput("nb", "Number of paths :", 5, min = 0),
                               checkboxInput("check", "different T ", FALSE),
                               numericInput("n", "Number of points by path:", 1000),
                               numericInput("seuil", "Threshold:", 80),
                               actionButton("buttonParameters", "OK"),
                               
                               
                               
                             ), # sidebarPanel
                             mainPanel(
                               h1("Parameters Estimates :"),
                               h2(textOutput("initial_first_parameter")),
                               h2(textOutput("First_parameter")),
                               textOutput("initial_mu_value"),
                               textOutput("a_simu"),
                               h2(textOutput("initial_second_parameter")),
                               h2(textOutput("Second_parameter")),
                               textOutput("initial_sigma2_value"),
                               textOutput("b_simu"),
                               tableOutput("initial_parameters"),
                               tableOutput("Table_estimator"),
                               h2(textOutput("Title_true_mean")),
                               textOutput("first_instant_exceeding_wiener_theory"),
                               h2(textOutput("Title_estimated_mean")),
                               textOutput("initial_instant_exceeding_wiener"),
                               textOutput("first_instant_exceeding_wiener"),
                               h2("Empirical mean of the time of exceeding the threshold:"),
                               textOutput("first_instant_exceeding_observed"),
                               plotOutput("initialCurve"),
                               plotOutput("WienerCurve"),
                               plotOutput("Graph_of_exceeding"),
                               plotOutput("GammaCurve"),
                               plotOutput("Graph_of_exceeding_gamma"),
                              
                               
                               
                             ) # mainPanel
                             
                             
                             
                    ), 
                    
                    
                    tabPanel("Data",
                             sidebarPanel(
                               tags$h1("Estimations :"),
                               tags$h2("mu :"),
                               textOutput("mu_value"),
                               
                               tags$h2("sigma2 :"),
                               textOutput("sigma2_value"),
                               tags$h2("a :"),
                               textOutput("a_value"),
                               tags$h2("b :"),
                               textOutput("b_value"),
                               tags$h2("Prediction:"),
                               textOutput("prediction"),
                               numericInput("seuildata", "Threshold:", 10),
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
                               
                               radioButtons("disp", "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head")
                           
                              
                               
                             ), # sidebarPanel
                             mainPanel(
                               textOutput("top"),
                               plotOutput("dataCurve"),
                        
                               
                             ) # mainPanel
                             
                    ),
                    tabPanel("Biais ",
                             sidebarPanel(
                               tags$h1("Parameters :"),
                               selectInput(inputId = "ModelChoice2", label = "Select a model", choices = c("Wiener Process", "Gamma Process", "Inverse Gaussian Process")),
                               conditionalPanel(condition = 'input.ModelChoice2 == "Gamma Process"', numericInput("a2", "a :", 1), numericInput("b2", "b :", 1)),
                               conditionalPanel(condition = 'input.ModelChoice2 ==  "Wiener Process"',
                                                numericInput("mu2", "mu :", 1,min = 0 ),
                                                numericInput("sigma22", "sigma2:", 1, min = 0)),
                               selectInput("dtchoix2" , label = "Type of distribution for the time", choices = c("Periodic" , "Exponential", "Gamma", "Normal" )),
                               conditionalPanel(condition = "input.dtchoix2 == 'Periodic'", numericInput("deltat2", " Period:", 0.1)),
                               conditionalPanel(condition =  "input.dtchoix2 == 'Exponential'", numericInput("Lambda2", "lambda :", 1)),
                               conditionalPanel(condition = "input.dtchoix2 == 'Gamma'", numericInput("aa2", "a :", 1), numericInput("bb2", "b :", 1)),
                               conditionalPanel(condition = "input.dtchoix2 == 'Normal'", numericInput("mean2", "m :", 1), numericInput("sigma_carre2", "sigma2 :", 1)),
                               numericInput("nb2", "Number of paths :", 5, min = 0),
                               numericInput("n2", "Number of points by path:", 1000),
                               actionButton("buttonParameters2", "OK"),
                               
                               
                               
                             ), # sidebarPanel
                             mainPanel(
                               h1("Biais  :"),
                               plotOutput("BiaisWiener"),
                               plotOutput("BiaisGamma")
                               
                             ) # mainPanel
                             
                             
                             
                    ), 
                    
                  ), # navbarPage 
                    
) # fluidPage
)
