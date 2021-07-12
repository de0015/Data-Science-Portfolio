#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(magrittr)
library(DT)
library(RCurl)
library(formattable)
library(viridis)


PA_Data <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

PA_data <- read.csv(text = PA_Data) 

PA_State <- PA_data %>% filter(state == "Pennsylvania")

WV_State <- PA_data %>% filter(state == "West Virginia")

WV_State$date <- as.Date(WV_State$date)

table_date <- format(Sys.Date()-1,"%Y-%m-%d") %>%
    as.Date()

PA_State$date <- as.Date(PA_State$date)

rawstatenumbers <- PA_State %>% filter(date == table_date)

rawstatenumbers <- rawstatenumbers[,c(2,1,5,6)]

rawstatenumbers <- arrange(rawstatenumbers,desc(rawstatenumbers$cases))

wv_raw <- WV_State %>% filter(date == table_date)

wv_raw <- arrange(wv_raw,desc(wv_raw$cases))

# Change these
wv_county <- c("Kanawha",
               "Berkeley",
               "Jackson",
               "Monongalia",
               "Wayne",
               "Jefferson",
               "Marion",
               "Cabell",
               "Wood",
               "Harrison",
               "Ohio",
               "Putnam",
               "Preston",
               "Logan",
               "Mason",
               "Mineral",
               "Mercer",
               "Fayette",
               "Marshall",
               "Raleigh",
               "Hampshire",
               "Hancock",
               "Morgan",
               "McDowell",
               "Monroe",
               "Nicholas",
               "Taylor",
               "Barbour",
               "Randolph",
               "Roane",
               "Tucker",
               "Upshur",
               "Brooke",
               "Greenbrier",
               "Hardy",
               "Lewis",
               "Pendleton",
               "Tyler",
               "Wetzel",
               "Wirt",
               "Boone",
               "Mingo",
               "Pleasants",
               "Braxton",
               "Grant",
               "Lincoln",
               "Summers",
               "Wyoming")
choices <-c("Adams",
            "Allegheny",
            "Armstrong",
            "Beaver",
            "Bedford",
            "Berks",
            "Blair",
            "Bradford",
            "Bucks",
            "Butler",
            "Cambria",
            "Cameron",
            "Carbon",
            "Centre",
            "Chester",
            "Clarion",
            "Clearfield",
            "Clinton",
            "Columbia",
            "Crawford",
            "Cumberland",
            "Dauphin",
            "Delaware",
            "Elk",
            "Erie",
            "Fayette",
            "Forest",
            "Franklin",
            "Fulton",
            "Greene",
            "Huntingdon",
            "Indiana",
            "Jefferson",
            "Juniata",
            "Lackawanna",
            "Lancaster",
            "Lawrence",
            "Lebanon",
            "Lehigh",
            "Luzerne",
            "Lycoming",
            "McKean",
            "Mercer",
            "Mifflin",
            "Monroe",
            "Montgomery",
            "Montour",
            "Northampton",
            "Northumberland",
            "Perry",
            "Philadelphia",
            "Pike",
            "Potter",
            "Schuylkill",
            "Snyder",
            "Somerset",
            "Sullivan",
            "Susquehanna",
            "Tioga",
            "Union",
            "Venango",
            "Warren",
            "Washington",
            "Wayne",
            "Westmoreland",
            "Wyoming",
            "York")
# Define UI for application that draws a histogram
    ui <- navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
                     "COVID-19 By County",
                     #    fluidPage(
                     #    theme = shinytheme("cosmo"),
                     #    titlePanel("COVID-19 by State by Daniel Efaw"),
                     sidebarLayout(position = "left",
                                   sidebarPanel(selectInput("dataset", "Use this filter selection to view/compare PA counties",
                                                            choices = choices, selected = "Washington", multiple = TRUE),
                                                selectInput("dataset2", "Use this filter selection to view/compare WV counties",
                                                                         choices = wv_county, selected = "Monongalia", multiple = TRUE),
                                                
                         ),  
                             mainPanel(
                                 tabsetPanel(
                                     id = 'datasetInput',
                                     tabPanel("Interactive Penn Cases", plotlyOutput('int', width = "100%", height = "600")),
                                     tabPanel("PA Table", DT::dataTableOutput("table")),
                                     tabPanel("PA Deaths", plotOutput("deathplot",width = "100%", height = "600")),
                                     tabPanel("Interactive WV Cases", plotlyOutput("int_wv", width = "100%", height = "600")),
                                     tabPanel("Interactive WV Deaths", plotlyOutput("int_wv_death", width = "100%", height = "600")),
                                     tabPanel("WV Table", DT::dataTableOutput("table_wv"))
                                
                         )
                     )
                 )
    )
    
    
                  
            
    
    server <- shinyServer(
        
        function(input,output){
            
            datasetInput <- reactive({
                PA_State %>% filter(county == input$dataset)
            })
            datasetInput2 <- reactive({
                WV_State %>% filter(county == input$dataset2)
            })
            # plot time series
            output$ts_plot <- renderPlot({
                
                dataset <- datasetInput()
                ggplot(dataset, aes(x = date, y = cases, group=county, color=county)) +
                    geom_line() +
                    geom_point() +
                    scale_color_viridis(discrete = TRUE) +
                    ggtitle("Total Number of Confirmed COVID-19 Cases") +
                    ylab("Number of cases")+
                    xlab("Date") 
            })
            output$deathplot <- renderPlot({
                dataset2 <- datasetInput()
                ggplot(dataset2, aes(x=date, y=deaths, group=county, color=county)) +
                    geom_line() +
                    geom_point() +
                    scale_color_viridis(discrete = TRUE) +
                    ggtitle("Total Number of COVID-19 Related Deaths") +
                    ylab("Number of deaths")+
                    xlab("Date")
            })
            output$table <- DT::renderDataTable({
                rawstatenumbers %>%
                    datatable(extensions = 'Buttons',
                              options = list(
                                  pageLength = 100,
                                  scrollX=TRUE,
                                  width = '700px',
                                  dom = 'T<"clear">lBfrtip'
                              )
                    )
            })
            output$int <- renderPlotly({
                
                dataset3 <- datasetInput()
                ggplot(dataset3, aes(x = date, y = cases, group=county, color=county)) +
                    geom_line() +
                    geom_point() +
                    scale_color_viridis(discrete = TRUE) +
                    ggtitle("Total Number of Confirmed COVID-19 Cases") +
                    ylab("Number of cases")+
                    xlab("Date") %>%
                    animation_opts(
                        1000, easing = "elastic", redraw = FALSE)
        })
            output$int_wv <- renderPlotly({
            dataset4 <- datasetInput2()
            ggplot(dataset4, aes(x = date, y = cases, group=county, color=county)) +
                geom_line() +
                geom_point() +
                scale_color_viridis(discrete = TRUE) +
                ggtitle("Total Number of Confirmed COVID-19 Cases") +
                ylab("Number of cases")+
                xlab("Date") %>%
                animation_opts(
                    1000, easing = "elastic", redraw = FALSE)
        })
            output$table_wv <- DT::renderDataTable({
                wv_raw %>%
                    datatable(extensions = 'Buttons',
                              options = list(
                                  pageLength = 100,
                                  scrollX=TRUE,
                                  width = '700px',
                                  dom = 'T<"clear">lBfrtip'
                              )
                    )
            })
            output$int_wv_death <- renderPlotly({
                dataset4 <- datasetInput2()
                ggplot(dataset4, aes(x = date, y = deaths, group=county, color=county)) +
                    geom_line() +
                    geom_point() +
                    scale_color_viridis(discrete = TRUE) +
                    ggtitle("Total Number of Confirmed COVID-19 Cases") +
                    ylab("Number of cases")+
                    xlab("Date") %>%
                    animation_opts(
                        1000, easing = "elastic", redraw = FALSE)
            })
        })
    shiny::shinyApp(ui = ui, server = server)
    
