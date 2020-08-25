library(RedditExtractoR) # Reddit Search and Extractor
library(tuber) # Youtube Api 
library(rvest) # Web scraper
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Social Media Search Tool"),
    
    

    sidebarPanel(textInput("term", "Enter a Term"),
                   numericInput("cant", "Select Number of Returns",1,0,200),
                   radioButtons("lang", "Select the Language",c(
                       "English"="en",
                       "Castellano"="es",
                       "Deutsch"="de")),
                   submitButton(text = "Run")
                   ))

        # Show a plot of the generated distribution
        mainPanel(
           h4("Last 5 Tweets"),
           tableOutput("table"),
           plotOutput("wordcl"))
           
        


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rawData <- reactive(function(){
    tweets <- get_reddit(search_terms = input$term, n=input$cant,lang=input$lang)
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
