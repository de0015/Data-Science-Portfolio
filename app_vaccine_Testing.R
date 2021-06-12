library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(rvest)
library(scales)
 library(DT)
library(RCurl)
library(formattable)
library(viridis)
library(tidyverse)


## If you post this to github... please remove line below
#rsconnect::setAccountInfo(name='de0105', token='84DE64071296560C73C4B4F8D042A7B1', secret='gDAOFxHUfDxccymL6EHBneo9h0KpuxldahV1wv5k')


yesterday <- format(Sys.Date()-1,"%m-%d-%Y.csv")

date_eval<- format(Sys.Date()-1,"%Y-%m-%d") %>%
    as.Date()

percent_change<- format(Sys.Date()-2,"%m-%d-%Y.csv") 

percent_change_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()

## Vaccine Data

## List of countries with vaccine and the type

Vaccines <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
Vaccines_2 <- read.csv(text=Vaccines) %>% select(1,3,4,5)

names(Vaccines_2) <- c("Location", "Vaccine Name", "Last Report", "Source")

## Global Vaccine (Our World in Data)

global_vaccine <-getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
global_vaccine_2 <- read.csv(text=global_vaccine)
#global_vaccine_2$date <- as.Date(global_vaccine_2$date)
test <- global_vaccine_2 %>% group_by(location) %>% filter(date == max(date)) 
global_vaccinated <- test %>% select(location,date,people_fully_vaccinated_per_hundred) 
global_test <- global_vaccine_2 %>% filter(!is.na(people_fully_vaccinated_per_hundred) )%>% 
  group_by(location) %>%
  slice(which.max(as.Date(date))) %>% select(location,date,people_fully_vaccinated_per_hundred) %>%
  mutate(people_fully_vaccinated_per_hundred/100) 

names(global_test) <- c("Country", "Last Report Date", "People Fully Vaccinated Per 100", "Percentage of Population Vaccinated")

global_test$`Percentage of Population Vaccinated` <- percent(global_test$`Percentage of Population Vaccinated`, digits = 0, format = "f")

### Import the CDC Vaccination Data

US_Vaccine <-getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")
cdc_vaccine_2 <- read.csv(text=US_Vaccine)
test_cdc <- cdc_vaccine_2 %>% group_by(location) %>% filter(date == max(date)) %>% select(location,date,people_fully_vaccinated_per_hundred)  %>%
  mutate(people_fully_vaccinated_per_hundred/100) 
names(test_cdc) <- c("State", "Date", "People Fully Vaccinated Per 100", "Percentage of Population Vaccinated")

test_cdc$`Percentage of Population Vaccinated` <- percent(test_cdc$`Percentage of Population Vaccinated`, digits = 0, format = "f")
## John Hopkins State Data Below

state_compare_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",percent_change) 

state_recent_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",yesterday) 

US_State_2_days <- getURL(state_compare_url)

US_State_2_days <- read.csv(text = US_State_2_days) 

US_STATE_NEW <- getURL(state_recent_url)

US_STATE_NEW <- read.csv(text = US_STATE_NEW)

######


data_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",yesterday) 

percent_change_data <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",percent_change) 

two_days_ago_date <- getURL(percent_change_data)

compare_date_jh <- read.csv(text = two_days_ago_date)

yesterday_jh <- getURL(data_url)

yesterday_jh_data <- read.csv(text = yesterday_jh)

table_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()


agg_compare <- aggregate(compare_date_jh$Confirmed, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_death <- aggregate(compare_date_jh$Deaths, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_recovery <- aggregate(compare_date_jh$Recovered, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 

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

total_country$`Confirmed Cases` <- total_country$`Confirmed Cases` 
total_country$Deaths <- total_country$Deaths

total_country$Recoveries <- total_country$Recoveries

agg_state_confirmed_yesterday <- aggregate(compare_date_jh$Confirmed, by=list(Category=compare_date_jh$Province_State), FUN=sum)
agg_state_deaths_yesterday <- aggregate(compare_date_jh$Deaths, by=list(Category=compare_date_jh$Province_State), FUN=sum)
agg_state_recovery_yesterday <- aggregate(compare_date_jh$Recovered, by=list(Category=compare_date_jh$Province_State), FUN=sum)

compare_data_state <- merge(agg_state_confirmed_yesterday,agg_state_deaths_yesterday, by = "Category")
compare_data_state <- merge(compare_data_state, agg_state_recovery_yesterday, by= "Category")

names(compare_data_state) <-c("State","Confirmed Cases","Deaths","Recoveries")

agg_state_confirmed_today <- aggregate(yesterday_jh_data$Confirmed, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)
agg_state_deaths_today <- aggregate(yesterday_jh_data$Deaths, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)
agg_state_recovery_today <- aggregate(yesterday_jh_data$Recovered, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)

compare_data_state_today <- merge(agg_state_confirmed_today,agg_state_deaths_today, by = "Category")
compare_data_state_today <- merge(compare_data_state_today, agg_state_recovery_today, by= "Category")

names(compare_data_state_today) <-c("State","Confirmed Cases","Deaths","Recoveries")

state_filter <- c("Vermont","New York","Pennsylvania","New Jersey","West Virginia","North Carolina","Texas","Puerto Rico","Texas","California","Illinois","District of Columbia", "Florida", "Michigan")

state_filter_2<- c("Vermont","New York State","Pennsylvania","New Jersey","West Virginia","North Carolina","Texas","Puerto Rico","Texas","California","Illinois","District of Columbia", "Florida", "United States", "Michigan")

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
#df <- mydata[ -c(1,3:4) ]
us_state_today <- US_STATE_NEW %>% filter(Province_State %in% state_filter)
us_state_yesterday <- US_State_2_days%>% filter(Province_State %in% state_filter)

us_state_calc <- merge(us_state_today,us_state_yesterday, by = "Province_State")

us_state_calc$`Confirmed Case Increase` <-  us_state_calc$Confirmed.x - us_state_calc$Confirmed.y 
us_state_calc$`Confirmed Death Increase` <- us_state_calc$Deaths.x - us_state_calc$Deaths.y 
us_state_calc$`Confirmed Recovery Increase` <- us_state_calc$Recovered.x - us_state_calc$Recovered.y

us_state_calc <- us_state_calc[ ,c(1,6,36,7,37,8,38)]

names(us_state_calc) <-c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase","Total Recoveries", "Daily Recoveries Increase")

us_state_calc <- arrange(us_state_calc)
### Changed the cdc state filter to alphabetize

cdc_state_filtered <- filter(test_cdc, State %in% state_filter_2) 
cdc_state_filtered <-  cdc_state_filtered[order(cdc_state_filtered$State),] 

world_vaccine_filtered <- filter(global_test, Country %in% choices) %>%
  arrange(desc(`People Fully Vaccinated Per 100`))


###

total_country <- arrange(total_country,desc(total_country$Country))

selected_states_current <- total_country %>% subset(Country %in% choices)


selected_states_compare <- comparison_data %>% subset(Country %in% choices)

names(selected_states_compare) <-c("Country","Confirmed Cases","Deaths","Recoveries")
selected_states_compare$`Confirmed Cases` <- selected_states_compare$`Confirmed Cases`


compar <- merge(selected_states_current,selected_states_compare, by = "Country") 

compar$`Confirmed Case Increase` <-  compar$`Confirmed Cases.x` - compar$`Confirmed Cases.y` 
compar$`Confirmed Death Increase` <- compar$Deaths.x - compar$Deaths.y 
compar$`Confirmed Recovery Increase` <- compar$Recoveries.x - compar$Recoveries.y

#df <- mydata[ -c(1,3:4) ]
### Below sets the columns for the final table

compar <- compar[,c(1,2,8,3,9,4,10)] 

merge_test <- merge(compar, world_vaccine_filtered, all.x = TRUE)

merge_test <- merge_test[,c(1,2,3,4,5,10)] 

merge_test <- arrange(merge_test,desc(compar$`Confirmed Cases`))

names(merge_test) <- c("Country","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase","Percentage Fully Vaccinated")

### The lines below are to fix John's Hopkins Data move
US_State_2_days$Recovered <- replace_na(US_State_2_days$Recovered,0)
usa_2_days_cases <- aggregate(US_State_2_days$Confirmed, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_days_deaths <- aggregate(US_State_2_days$Deaths, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_days_recovery <- aggregate(US_State_2_days$Recovered, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_day_merge_1 <- merge(usa_2_days_cases,usa_2_days_deaths, by = "Category")
usa_2_day_merge_2 <- merge(usa_2_day_merge_1, usa_2_days_recovery, by = "Category")
### 
 US_STATE_NEW$Recovered <- replace_na(US_STATE_NEW$Recovered,0)
usa_yesterday_cases <- aggregate(US_STATE_NEW$Confirmed, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_deaths <- aggregate(US_STATE_NEW$Deaths, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_recovery <- aggregate(US_STATE_NEW$Recovered, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_merge_1 <- merge(usa_yesterday_cases,usa_yesterday_deaths,  by = "Category")
usa_yesterday_merge_2 <- merge(usa_yesterday_merge_1, usa_yesterday_recovery, by = "Category")
###

### Merge the USA DF's
USA_Totals <- merge(usa_yesterday_merge_2, usa_2_day_merge_2, by = "Category")
names(USA_Totals) <-c("Country", "Yesterday Cases", "Yesterday Deaths", "Yesterday Recoveries", "2 day cases", "2 day deaths", "2 day recoveries")
USA_Totals$CaseIncrease <- USA_Totals$`Yesterday Cases`- USA_Totals$`2 day cases`
USA_Totals$DeathIncrease <- USA_Totals$`Yesterday Deaths` - USA_Totals$`2 day deaths`
USA_Totals$RecoveryIncrease <- USA_Totals$`Yesterday Recoveries` - USA_Totals$`2 day recoveries`
USA_Totals <- USA_Totals[,c(1,2,8,3,9,4,10)]

names(USA_Totals) <- c("Country", "Confirmed Cases", "Case Increase", "Deaths", "Death Increase", "Recoveries", "Recoveries Increase")
names(compar) <-c("Country","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase","Total Recoveries", "Daily Recoveries Increase")

compar <- arrange(compar,desc(compar$`Confirmed Cases`))
compar <- compar[-c(6,7)]
us_state_calc <- us_state_calc[-c(6,7)]
USA_Totals <- USA_Totals[-c(6,7)]


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
    fluidPage(
    theme = shinytheme("cosmo")
  #  titlePanel(""),
  #  sidebarLayout(
      # sidebarPanel(
           # checkboxGroupInput("dataset", "State",
            #            choices = choices, selected = "North Carolina")
        ),
        mainPanel(
          mainPanel(
            tabsetPanel(
              id = 'dataset',
              tabPanel("Country", DT::dataTableOutput("table")),
              tabPanel("State", DT::dataTableOutput("mytable2")),
              tabPanel("US Calculation", DT::dataTableOutput("mytable3")),
              tabPanel("US Vaccination Filtered", DT::dataTableOutput("mytable4")),
              #tabPanel("World Vaccination Filtered", DT::dataTableOutput("mytable5"))
              tabPanel("World Vaccination Filtered", DT::dataTableOutput("mytable6"))
            )
          )
        )
  )
          
        
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
                                               width = '700px',
                                               dom = 'T<"clear">lBfrtip'
                                           )
                ) %>%
                    formatCurrency(2:4, currency = "", mark = ",") %>%
                    formatRound(columns=c("Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase"), digits=0)
           ## If the countries disappear, remove Country from the format round above... The damn thing tries to round a string...
            })
            
            output$mytable2 <- DT::renderDataTable({
              us_state_calc %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",") %>%
               formatRound(columns=c("Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase"), digits=0)
            })           
            
            output$mytable3 <- DT::renderDataTable({
              USA_Totals %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",") %>%
                formatRound(columns=c("Confirmed Cases", "Case Increase", "Deaths", "Death Increase"), digits=0)
            })
            
            output$mytable4 <- DT::renderDataTable({
              cdc_state_filtered %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                 formatPercentage(4, digits = 0)
              #  formatRound(columns=c("State", "Date", "People Fully Vaccinated Per 100"), digits=0)
            })   
            
            output$mytable5 <- DT::renderDataTable({
              world_vaccine_filtered %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatPercentage(4, digits = 0)
              #  formatRound(columns=c("Country", "Last Report Date", "People Fully Vaccinated Per 100"), digits=0)
            })   
            
            output$mytable6 <- DT::renderDataTable({
              merge_test %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",", digits = 0) %>%
                formatPercentage(6, digits = 2)
            })   
        })
    
    shiny::shinyApp(ui = ui, server = server)
    