#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(rsconnect)

jail <- read_csv("https://uofi.box.com/shared/static/b6kuknulot4bqyo7elc8gmm6qzhnk766.dat", 
                 col_types = cols(booking_date = col_date(format = "%m/%d/%Y"), 
                                  booking_time = col_time(format = "%H:%M:%S"), 
                                  released_date = col_date(format = "%m/%d/%Y"), 
                                  released_time = col_time(format = "%H:%M:%S")))

jail2 <- jail %>%
  mutate(city=ifelse(str_detect(city,"^CHA\\w+G"),"CHAMPAIGN",city),city=ifelse(str_detect(city,"^UR\\w+A"),"URBANA",city))
jail2 <- jail2 %>% distinct(jacket_number, .keep_all = TRUE)
jail2$employment_status[is.na(jail2$employment_status)] <- "Unknown"
jail2$race[is.na(jail2$race)] <- "Unknown"
jail2$daysJail <- round((as.numeric(jail2$hours) / 24), 3)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(h1("Time Spent in Champaign County Jails by Various Demographics", align = "center")),
  
  sidebarPanel(
    
    selectInput("checkRace", 
                h4("Race"), choices = list("White" = "White", "Hispanic" = "Hispanic", "White (Hispanic)" = "White (Hispanic)", "Black" = "Black", "Asian/Pacific Islander" = "Asian/Pacific Islander", "Native American" = "Native American"), selected = "White"),
    
    selectInput("checkMarital", 
                h4("Marital Status"), choices = list("Married" =  "Married", "Single" = "Single", "Seperated" = "Separated", "Significant Other" = "Significant Other", "Divorced" = "Divorced", "Widowed" = "Widowed"), selected = "Married"),
    
    selectInput("checkSex", 
                h4("Sex"), choices = list("Female" = "Female", "Male" = "Male"), selected = "Male"),
    
    selectInput("checkEmployment", 
                h4("Employment"), choices = list("Employed - Full Time" = "Employed - Full Time", "Employed - Part Time" = "Employed - Part Time", "Laid Off" = "Laid Off", "Retired" = "Retired", "Self Employed" = "Self Employed", "Student" = "Student", "Unemployed" = "Unemployed", "Unknown" = "Unknown")),
    
    sliderInput("sliderPercentage",
                h4("Percentage of the Sample, Use it to zoom in on the bottom left"), min = 1, max = 100, value = 100), 
    
    materialSwitch("switch", label = h4("Switch to Days"), status = "info"),
    
    width = 4
  ),
  
  mainPanel( 
    # Show a plot of the generated distribution
    h4("Histogram of Time Spent in Jail by Various Demographics", align = "center"),
    plotOutput("Plot"),
    
    # Show some statistics
    h4("Summary Statistics of Time Spent in Jail by Various Demographics", align = "center"),
    
    tableOutput("Statistics")
  )
)

# Define server logic
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    dat = jail2 %>% 
      filter(race %in% input$checkRace) %>%
      filter(maritial_status %in% input$checkMarital) %>%
      filter(sex %in% input$checkSex) %>%
      filter(employment_status %in% input$checkEmployment)
    dat = if(!input$switch) {
      dat %>% 
        select(race, sex, maritial_status, employment_status, hours) %>%
        arrange(hours) %>%
        rename(time_jail = hours)
    } else {
      dat %>%
        select(race, sex, maritial_status, employment_status, daysJail) %>% 
        arrange(daysJail) %>%
        rename(time_jail = daysJail)
    }
    dat = head(dat, input$sliderPercentage/100*nrow(dat))
    
    ggplot(data = dat) +
      geom_histogram(aes(x = time_jail), color = "#619CFF", fill = "lightblue", binwidth = 1) +
      labs(x = "Time Spent in Jail (Default: Hours)") + 
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0.5))
    
  })
  
  output$Statistics <- renderTable({
    dat = jail2 %>% 
      filter(race %in% input$checkRace) %>%
      filter(maritial_status %in% input$checkMarital) %>%
      filter(sex %in% input$checkSex) %>%
      filter(employment_status %in% input$checkEmployment)
    dat = if(!input$switch) {
      dat %>% 
        select(race, sex, maritial_status, employment_status, hours) %>%
        arrange(hours) %>%
        rename(time_jail = hours)
    } else {
      dat %>%
        select(race, sex, maritial_status, employment_status, daysJail) %>% 
        arrange(daysJail) %>%
        rename(time_jail = daysJail)
    }
    dat = head(dat, input$sliderPercentage/100*nrow(dat))
    
    data.frame( "Statistics" = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"), "Time Spent in Jail" = unclass(summary(dat$time_jail)), check.names = FALSE, stringsAsFactors = FALSE)
    
  }, align = "c")
  
}

# Run the app
shinyApp(ui = ui, server = server,options = list(height = 800))