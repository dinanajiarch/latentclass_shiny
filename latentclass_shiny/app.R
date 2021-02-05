#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
    navbarPage("Latent Class Anlaysis Visualizations",
               tabPanel("Item Probability Plots",
                        sidebarLayout(
                            sidebarPanel("Upload your Mplus output that contains the enumeration analyses only. Do not upload output with covariates.",
                                         fileInput("file1", label = h3("File input")),
                                         
                                         hr(),
                                         fluidRow(column(4, verbatimTextOutput("value")))
                                         
                            ),
                            mainPanel("IPP OUTPUT!",
                                      plotOutput(outputId = "ipp_plot"))
                        ),
               ),
               
               tabPanel("Discrete Covariate Plots", 
                        sidebarLayout(
                                sidebarPanel("Upload Mplus output with categorical covariates. This is the output from the third step in the 3-step analysis.",
                                             fileInput("file2", label = h3("File input")),
                                             
                                             hr(),
                                             fluidRow(column(4, verbatimTextOutput("value")))
                                             
                                ),
                            mainPanel("DCP OUTPUT!",
                                      plotOutput(outputId = "ipp_plot"))
                            ),
               ),
               tabPanel("Continuous Covariate Plots",
                        sidebarLayout(
                            sidebarPanel("Upload Mplus output with continuous covariates. This is the output from the third step in the 3-step analysis.",
                                         fileInput("file3", label = h3("File input")),
                                         hr(),
                                         fluidRow(column(4, verbatimTextOutput("value")))
                                         ),
                            mainPanel("CCP OUTPUT!",
                                      plotOutput(outputId = "ipp_plot"))
                        ),
                        ),
               tabPanel("Logits to Probabilities",
                        sidebarLayout(
                            sidebarPanel("Want to easily convert logits to probabilities? Input the logit below and the app will calculate the probability.",
                                         numericInput("num", label = h3("Numeric input"), value = 1),
                                         
                                         hr(),
                                         fluidRow(column(3, verbatimTextOutput("value")))
                                         ),
                            mainPanel("LTP OUTPUT!",
                                      plotOutput("sw_plot"))
                        )
                    )
                )
            )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$value1 <- renderPrint({
        str(input$file1)
    })
    

    output$value2 <- renderPrint({
        str(input$file2)
    })
    
    output$value3 <- renderPrint({
        str(input$file3)
    })
    
    output$value4 <- renderPrint({ input$num })
    


    output$ipp_plot <- renderPlot({
        ggplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
