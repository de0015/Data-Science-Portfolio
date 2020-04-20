
library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(DT)
library(DataCombine)
library(RCurl)
library(formattable)
library(viridis)


## If you post this to github... please remove line below


yesterday <- format(Sys.Date()-1,"%m-%d-%Y.csv")

date_eval<- format(Sys.Date()-1,"%Y-%m-%d") %>%
    as.Date()

percent_change<- format(Sys.Date()-2,"%m-%d-%Y.csv") 

percent_change_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()

three_days_ago <- format(Sys.Date()-3,"%m-%d-%Y.csv") 

three_days_ago_date <- format(Sys.Date()-3,"%Y-%m-%d") %>%
  as.Date()

data_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",yesterday) 

percent_change_data <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",percent_change) 

three_days_ago_data<- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",three_days_ago) 

three_days_ago_get_url <- getURL(three_days_ago_data)

two_days_ago_date <- getURL(percent_change_data)

three_days_ago_data_use <- read.csv(text = three_days_ago_get_url)

compare_date_jh <- read.csv(text = two_days_ago_date)

yesterday_jh <- getURL(data_url)

yesterday_jh_data <- read.csv(text = yesterday_jh)

table_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()

agg_compare <- aggregate(compare_date_jh$Confirmed, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_death <- aggregate(compare_date_jh$Deaths, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_recovery <- aggregate(compare_date_jh$Recovered, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 


###
agg_confirmed_three_days <- aggregate(three_days_ago_data_use$Confirmed, by=list(Category=three_days_ago_data_use$Country_Region), FUN=sum) 
agg_death_three_days <- aggregate(three_days_ago_data_use$Deaths, by=list(Category=three_days_ago_data_use$Country_Region), FUN=sum) 
agg_recovery_three_days <- aggregate(three_days_ago_data_use$Recovered, by=list(Category=three_days_ago_data_use$Country_Region), FUN=sum)
names(agg_confirmed_three_days) <-c("Country","Confirmed Cases")
names(agg_death_three_days) <-c("Country","Deaths")
names(agg_recovery_three_days) <-c("Country","Recoveries")
three_day_data_merge <- merge(agg_confirmed_three_days,agg_death_three_days, by = c("Country"))
three_day_data_merge <- merge(three_day_data_merge, agg_recovery_three_days, by = c("Country"))
###

agg_country <- aggregate(yesterday_jh_data$Confirmed, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum) 

agg_death <- aggregate(yesterday_jh_data$Deaths, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum)

names(agg_compare) <-c("Country","Confirmed Cases")
names(agg_compare_death) <-c("Country","Deaths")
names(agg_compare_recovery) <-c("Country","Recoveries")

comparison_data <- merge(agg_compare,agg_compare_death, by = c("Country"))
comparison_data <- merge(comparison_data, agg_compare_recovery, by = c("Country"))

### The code below is for the merge of the final dataset

agg_recovery <- aggregate(yesterday_jh_data$Recovered, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum)
names(agg_country) <-c("Country","Confirmed Cases")
names(agg_death) <-c("Country","Deaths")
names(agg_recovery) <-c("Country","Recoveries")
total_country <- merge(agg_country,agg_death, by=c("Country"))

total_country <- merge(total_country, agg_recovery, by=c("Country"))

total_country$`Confirmed Cases` <- comma(total_country$`Confirmed Cases`, digits = 0)
total_country$Deaths <- comma(total_country$Deaths, digits = 0)

total_country$Recoveries <- comma(total_country$Recoveries, digits = 0)


choices <-c("Algeria",
            "Australia",
            "Austria",
            "Bahrain",
            "Belarus",
            "Belgium",
            "Bosnia and Herzegovina",
            "Brazil",
            "Bulgaria",
            "Canada",
            "China",
            "Cote d'Ivoire",
            "Croatia",
            "Czechia",
            "Denmark",
            "Egypt",
            "Estonia",
            "Finland",
            "France",
            "Germany",
            "Greece",
            "Hungary",
            "India",
            "Iran",
            "Iraq",
            "Ireland",
            "Italy",
            "Japan",
            "Jordan",
            "Kazakhstan",
            "Kenya",
            "Kuwait",
            "Latvia",
            "Lebanon",
            "Lithuania",
            "Luxembourg",
            "Malaysia",
            "Mexico",
            "Morocco",
            "Netherlands",
            "New Zealand",
            "Norway",
            "Oman",
            "Peru",
            "Philippines",
            "Poland",
            "Portugal",
            "Qatar",
            "Romania",
            "Russia",
            "Saudi Arabia",
            "Serbia",
            "Singapore",
            "Slovakia",
            "Slovenia",
            "South Africa",
            "Korea, South",
            "Spain",
            "Sweden",
            "Switzerland",
            "Taiwan*",
            "Thailand",
            "Turkey",
            "Uganda",
            "Ukraine",
            "United Arab Emirates",
            "United Kingdom",
            "US",
            "Zambia")

### Use these below to calculate

three_days_filter <- three_day_data_merge %>% subset(Country %in% choices)
two_days_filter <- comparison_data %>% subset(Country %in% choices)
one_day_filter <- total_country %>% subset(Country %in% choices)

### Arranging to make sure they are in same order
three_days_filter <- arrange(three_days_filter,desc(three_days_filter$Country))
two_days_filter <- arrange(two_days_filter,desc(two_days_filter$Country))
one_day_filter <- arrange(one_day_filter,desc(one_day_filter$Country))

### Calculator below

two_days_filter$Increase_cases <- two_days_filter$`Confirmed Cases`- three_days_filter$`Confirmed Cases`
two_days_filter$Increase_death <- two_days_filter$Deaths - three_days_filter$Deaths
two_days_filter$Increase_recovery <- two_days_filter$Recoveries - three_days_filter$Recoveries
one_day_filter$Increase_cases <- one_day_filter$`Confirmed Cases` - two_days_filter$`Confirmed Cases`
one_day_filter$Increase_death <- one_day_filter$Deaths - two_days_filter$Deaths
one_day_filter$Increase_recoveries <- one_day_filter$Recoveries - two_days_filter$Recoveries

###

older_data_merge <- merge(three_days_filter, two_days_filter, by = "Country")
older_data_merge <- arrange(older_data_merge,desc(older_data_merge$Country))
older_data_merge$confirmed_difference <- older_data_merge$`Confirmed Cases.y`- older_data_merge$`Confirmed Cases.x` 

###
###
new_merge <- merge(two_days_filter, one_day_filter, by = "Country")
new_merge <- arrange(new_merge,desc(new_merge$Country))
new_merge$Confirmed_rate <- new_merge$Increase_cases.y - new_merge$Increase_cases.x
new_merge$Death_rate <- new_merge$Increase_death.y - new_merge$Increase_death.x
new_merge$Recovery_rate <- new_merge$Increase_recoveries- new_merge$Increase_recovery
###

#total_country <- arrange(total_country,desc(total_country$Country))

#selected_states_current <- total_country %>% subset(Country %in% choices)


#selected_states_compare <- comparison_data %>% subset(Country %in% choices)

#names(selected_states_compare) <-c("Country","Confirmed Cases","Deaths","Recoveries")
#selected_states_compare$`Confirmed Cases` <- comma(selected_states_compare$`Confirmed Cases`, digits = 0)


#compar$`Confirmed Case Increase` <-  (compar$`Confirmed Cases.x` - compar$`Confirmed Cases.y`) 
#compar$`Confirmed Death Increase` <- compar$Deaths.x - compar$Deaths.y 
#compar$`Confirmed Recovery Increase` <- compar$Recoveries.x - compar$Recoveries.y

#compar <- merge(selected_states_current,selected_states_compare, by = "Country") 

#compar$`Confirmed Case Increase` <-  (compar$`Confirmed Cases.x` - compar$`Confirmed Cases.y`) 
#compar$`Confirmed Death Increase` <- compar$Deaths.x - compar$Deaths.y 
#compar$`Confirmed Recovery Increase` <- compar$Recoveries.x - compar$Recoveries.y

#df <- mydata[ -c(1,3:4) ]
### Below sets the columns for the final table

compar <- new_merge[,c(1,8,14,9,15,10,16)] 
compar[,2:7] %>% as.numeric()

names(compar) <-c("Country name","Total Confirmed Cases","Daily Confirmed Increase/Decrease", "Total Deaths","Daily Increase/Decrease in Deaths","Total Recoveries","Daily Increase/Decrease in Recoveries")

compar <- arrange(compar,desc(compar$`Total Confirmed Cases`))

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
    fluidPage(
    theme = shinytheme("cosmo"),
  #  titlePanel(""),
  #  sidebarLayout(
      # sidebarPanel(
           # checkboxGroupInput("dataset", "State",
            #            choices = choices, selected = "North Carolina")
        ),
        mainPanel(
          #  plotOutput("ts_plot", width = "100%", height = "600"),
          #  plotOutput("deathplot", width = "100%", height = "600"),
            dataTableOutput("table", width = "100%", height = "700")
            
        )
    )
#)
    server <- shinyServer(
        
        function(input,output){
            
        #    datasetInput <- reactive({
         #       NY_times_state %>% filter(state == input$dataset)
         #   })
            
            # plot time series
           
            output$table <- DT::renderDataTable({
                compar %>%
                    datatable(extensions = 'Buttons',
                                           options = list(
                                               pageLength = 100,
                                               scrollX=TRUE,
                                               dom = 'T<"clear">lBfrtip'
                                           )
                ) %>%
                    formatCurrency(2:4, currency = "", mark = ",") %>%
                    formatRound(columns=c("Total Confirmed Cases","Daily Confirmed Increase/Decrease", "Total Deaths","Daily Increase/Decrease in Deaths","Total Recoveries","Daily Increase/Decrease in Recoveries"), digits=0)
           ## If the countries disappear, remove Country from the format round above... The damn thing tries to round a string...
            })
        
        })
    
    shiny::shinyApp(ui = ui, server = server)
    