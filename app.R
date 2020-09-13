library(RedditExtractoR) 
library(tuber) 
library(rvest) 
library(shiny)
library(shinyWidgets)
library(DT)

Sys.setlocale('LC_ALL','C')


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Social Media Search Tool"),
    

    ui <- fluidPage(
      actionButton("go", "Go"),
      textInput("n", "search"),
      dataTableOutput("table")
      
    ))
    
    server <- function(input, output) {
      
      randomVals <- eventReactive(input$go, {
        reddit_urls(input$n)
      })
      output$table <- DT::renderDataTable({
        (randomVals())
      })
    
    }
    
    
    shinyApp(ui, server)
    
    #Not sure why but it works...