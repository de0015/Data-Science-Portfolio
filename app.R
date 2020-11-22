<<<<<<< HEAD
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
library(ggmap)
library(usmap)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(RCurl)
library(plotly)
library(rvest)
library(tidyverse)
library(widgetframe)
library(htmlwidgets)
library(leaflet)
library(shinydashboard)
library(viridis)
library(DT)

yesterday <- format(Sys.Date()-1,"%m-%d-%Y.csv")

date_eval<- format(Sys.Date()-0,"%Y-%m-%d") %>%
    as.Date()

percent_change<- format(Sys.Date()-2,"%m-%d-%Y.csv") 

percent_change_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()


data_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",yesterday) 

percent_change_data <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",percent_change) 

NY_Times_data <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

NY_Times_data <- read.csv(text = NY_Times_data)


percent_change_url <- getURL(percent_change_data)

percent_change_data_csv<- read.csv(text = percent_change_url)

percent_change_data_csv$Last.Update <- as.Date(percent_change_data_csv$Last_Update)

time_series_confirmed <- getURL(data_url)

time_series<- read.csv(text = time_series_confirmed)

country_sort <- sort(time_series$Country_Region) 

time_series_table <- time_series 

time_series_table$Last_Update <- as.Date(time_series_table$Last_Update)

daily_count <- filter(time_series_table, Last_Update == date_eval) %>%
    group_by(Country_Region)

daily_count_2 <- filter(time_series_table, Last_Update == date_eval)

daily_change <- filter(time_series_table, Last_Update >= percent_change_date)

daily_change_3000 <- daily_change[1:3000,1:13]
daily_change_3000 <- arrange(daily_change_3000,desc(daily_change_3000$Confirmed))

daily_count_3000 <- daily_count_2[1:3000,1:13]
daily_count_3000 <- arrange(daily_count_3000,desc(daily_count_3000$Confirmed))

Daily_US <- filter(daily_change, Country_Region == "US")

Daily_US <- arrange(Daily_US,desc(Daily_US$Confirmed))

Daily_US_2 <- Daily_US %>%
  select(Admin2, Province_State, Last_Update, Confirmed, Deaths, Recovered, Incident_Rate, Case_Fatality_Ratio)

daily_count$location <- paste(daily_count$Province_State,",",daily_count$Country_Region)

time_series_map <- time_series 
time_series_map <- filter(time_series_map, Confirmed > 1)

confirmed_time <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") 

confirmed_time <- read.csv(text = confirmed_time) 

names(confirmed_time) = gsub(pattern = "X*", replacement = "", x = names(confirmed_time))

US_Confirmed <- confirmed_time %>%
    as.matrix()

agg_country <- aggregate(daily_count$Confirmed, by=list(Category=daily_count$Country_Region), FUN=sum)

names(agg_country) <-c("Country","Confirmed Cases")

agg_country <- arrange(agg_country,desc(agg_country$`Confirmed Cases`))

PA_Totals <- filter(Daily_US, Province_State == "Pennsylvania")

WV_Totals <- filter(Daily_US, Province_State == "West Virginia") 

WV_Totals <- as.data.frame(WV_Totals)

names(WV_Totals) <-c("fips","County","State_Province","Country","Last_Update","Lat", "Long_", "Confirmed", "Deaths", "Recovered", "Active", "Combined_Key", "Incident Rate", "Case Fatality Rate")
names(PA_Totals) <-c("fips","County","State_Province","Country","Last_Update","Lat", "Long_", "Confirmed", "Deaths", "Recovered", "Active", "Combined_Key", "Incident Rate", "Case Fatality Rate")



#wv_map <-  plot_usmap("counties", data = WV_Totals , values = "Confirmed",
#                      include = "WV")+
#  theme(legend.position = "right")+
#  labs(title = "COVID-19 Cases by county in WV") + 
#  scale_fill_continuous(
#    low = "white", high = "red", name = "Confirmed COVID-19 Cases in WV")  
## Below is the actual shiny app

ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "Daniel Efaw's Coronavirus Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Global Map", tabName = "Global_Map", icon = icon("globe")),
            menuItem("US_Totals", tabName = "US_Total", icon = icon("clipboard-list")),
            menuItem("Pennsylvania Map", tabName = "PA_Map", icon = icon("globe-americas")),
            menuItem("West Virginia Map", tabName = "WV_Map", icon = icon("globe-americas")))),
    
    dashboardBody(
            tabItems(
            # First tab content
            tabItem(tabName = "Global_Map",
                    fluidRow(leafletOutput("mymap"))),
        
        # Second tab content
        tabItem(tabName = "US_Total",
                dataTableOutput("US_table")),
     
        # Second tab content
        tabItem(tabName = "PA_Map",
                plotlyOutput("pa_map2")),

        # Third tab content
        tabItem(tabName = "WV_Map",
               plotlyOutput("wv_map2")))))
            

server <- function(input, output, session){
  
      output$mymap<- renderLeaflet({
        leaflet(data = daily_change, height = 900, width = 800) %>% 
            addTiles() %>% 
            setView( lng = 33.391, lat = 47.143, zoom = 3 ) %>% 
            addProviderTiles(providers$CartoDB.Voyager,
                             options = providerTileOptions(noWrap = FALSE)
            ) %>%
            addMarkers(~Long_, ~Lat, popup = ~as.character(paste("Confirmed Cases", Confirmed,"<br>", "Number of Deaths", Deaths, "<br>", "Recovered Cases", Recovered)), label = ~as.character(Combined_Key), clusterOptions = markerClusterOptions())
    })
    
    output$US_table <- DT::renderDataTable({
        Daily_US_2%>%
            datatable(extensions = 'Buttons',
                      options = list(
                          pageLength = 100,
                          scrollX=TRUE,
                          width = '700px',
                          dom = 'T<"clear">lBfrtip'))
      
    })
    
    output$wv_map2 <- renderPlotly({
        ggplotly(
          plot_usmap("counties", data = WV_Totals , values = "Confirmed",
                     include = "WV")+
            theme(legend.position = "right")+
            labs(title = "COVID-19 Cases by county in WV") + 
            scale_fill_continuous(
              low = "white", high = "red", name = "Confirmed COVID-19 Cases in WV"))
})
    output$pa_map2 <- renderPlotly({
      ggplotly(
        plot_usmap("counties", data = PA_Totals , values = "Confirmed",
                   include = "PA")+
          theme(legend.position = "right")+
          labs(title = "COVID-19 Cases by county in PA") + 
          scale_fill_continuous(
            low = "white", high = "red", name = "Confirmed COVID-19 Cases in PA")
        )
    })
}

shiny::shinyApp(ui = ui, server = server)
=======
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
>>>>>>> 30510c4009ccc6ac04a50efc79f573d8aa575297
