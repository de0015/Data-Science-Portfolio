library(RedditExtractoR) 
library(rvest)
library(rtweet)
library(shiny)
library(httr)
library(shinyWidgets)
library(shinythemes)
library(DT)

Sys.setlocale('LC_ALL','C')

create_token(app = "XXX", consumer_key = "XXXX", consumer_secret = 
                    "XXXXX", access_token = "XXXX", access_secret = "XXXX")
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
    
    # Application title
    titlePanel("Social Media Search Tool"),
    
    
    ui <- fluidPage(
        actionButton("go", "Go"),
        textInput("n", "search"),
        tabsetPanel(type = "tabs",
                    tabPanel("Reddit", dataTableOutput("table")),
                    tabPanel("Twitter", dataTableOutput("table2")))
    ))

server <- function(input, output) {
    
    randomVals <- eventReactive(input$go, {
        reddit_urls(input$n)
    })
        randomVals2 <- eventReactive(input$go, {
            search_tweets(input$n)
    })
    output$table <- DT::renderDataTable({
        (randomVals())
    })
    output$table2 <- DT::renderDataTable({
        (randomVals2())
    
})}


shinyApp(ui, server)

#Not sure why but it works...