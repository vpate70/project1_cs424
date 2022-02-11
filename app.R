library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
require(scales)

#split screen 2 and 2 mini menus


ridership_halsted <- read.csv(file = 'data/ridership_halsted_updated.csv')

ridership_jefferson <- read.csv(file = 'data/ridership_jefferson_updated.csv')

ridership_ohare <- read.csv(file = 'data/ridership_ohare_updated.csv')


ridership_halsted <- read.csv(file = 'data/ridership_halsted_updated.csv')
head(ridership_halsted)
ridership_halsted$updated_date <- date((parse_date_time(ridership_halsted$date,"mdy")))
ridership_halsted$the_year <- year(ridership_halsted$updated_date)
ridership_halsted$the_month <- month(ridership_halsted$updated_date, abbr = TRUE, label = TRUE)
ridership_halsted$weekday <- wday(ridership_halsted$updated_date, abbr = TRUE, label = TRUE)


ridership_jefferson <- read.csv(file = 'data/ridership_jefferson_updated.csv')
head(ridership_jefferson)
ridership_jefferson$updated_date <- date((parse_date_time(ridership_jefferson$date,"mdy")))
ridership_jefferson$the_year <- year(ridership_jefferson$updated_date)
ridership_jefferson$the_month <- month(ridership_jefferson$updated_date, abbr = TRUE, label = TRUE)
ridership_jefferson$weekday <- wday(ridership_jefferson$updated_date, abbr = TRUE, label = TRUE)

ridership_ohare <- read.csv(file = 'data/ridership_ohare_updated.csv')
head(ridership_ohare)
ridership_ohare$updated_date <- date((parse_date_time(ridership_ohare$date,"mdy")))
ridership_ohare$the_year <- year(ridership_ohare$updated_date)
ridership_ohare$the_month <- month(ridership_ohare$updated_date,abbr = TRUE, label = TRUE)
ridership_ohare$weekday <- wday(ridership_ohare$updated_date, abbr = TRUE, label = TRUE)

years <- sort(unique(year(ridership_ohare$updated_date)), decreasing = TRUE)
x_con <- sort(c("Daily", "Monthly", "Week Day", "All", "Default"))
snams <- sort(c("O'Hare", "UIC-Halsted", "Jefferson Park"))
tflist <- c("FALSE","TRUE")
options(scipen=10000)

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 1"),
  #edit to make mini menu items for both
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   menuItem("Left options",
                            selectInput("station_name", "Select the station name", snams, selected = "UIC-Halsted"),
                            selectInput("Year", "Select the year to visualize", years, selected = 2021),
                            selectInput("type_x", "Select the constraint", x_con, selected = "Default"),
                            selectInput("tableCheck", "Show Table Values",tflist, selected = "FALSE")
                   ),
                   menuItem("Right options",
                            selectInput("rstation_name", "Select the station name", snams, selected = "Jefferson Park"),
                            selectInput("rYear", "Select the year to visualize", years, selected = 2021),
                            selectInput("rtype_x", "Select the constraint", x_con, selected = "Default"),
                            selectInput("rtableCheck", "Show Table Values",tflist, selected = "FALSE")
                   )
                   
  ),
  dashboardBody(
    fluidRow(
      column(6,
             fluidRow(
               box( title = "Left Panel", solidHeader = TRUE, status = "primary", width = 12,
                    verticalLayout(

                        plotOutput("leftbox"),
                        #    dataTableOutput("tab1",  ),
                        conditionalPanel(condition = "input.tableCheck == 'TRUE'", dataTableOutput("tab1")),
                    
                      
                      conditionalPanel(condition = "input.type_x == 'All'", plotOutput("leftall")),
                      conditionalPanel(condition = "input.type_x == 'All'", plotOutput("leftall2"))
                    )
               )
             )
      ),
     
      
      column(6,
             fluidRow(
               box( title = "Right Panel", solidHeader = TRUE, status = "primary", width = 12,
                    verticalLayout(
                        plotOutput("rightbox"),
                        #    dataTableOutput("tab1",  ),
                        conditionalPanel(condition = "input.rtableCheck == 'TRUE'", dataTableOutput("tab3")),
                    
                      
                      conditionalPanel(condition = "input.rtype_x == 'All'", plotOutput("rightall")),
                      conditionalPanel(condition = "input.rtype_x == 'All'", plotOutput("rightall2"))
                    )
               )
             )
      ),
      
    )
  ))

server <- function(input, output) {
  theme_set(theme_grey(base_size = 14)) 
  #Title the graphs and delete year from labels
  justOneYearReactive <- reactive({
    if(input$station_name == "UIC-Halsted"){
      return(subset(ridership_halsted, ridership_halsted$the_year == input$Year))
    }
    else if(input$station_name == "O'Hare"){
      return(subset(ridership_ohare, ridership_ohare$the_year == input$Year))
    }
    else{
      return(subset(ridership_jefferson, ridership_jefferson$the_year == input$Year))
    }
  })
  
  
  justOneYearReactiver <- reactive({
    if(input$rstation_name == "UIC-Halsted"){
      return(subset(ridership_halsted, ridership_halsted$the_year == input$rYear))
    }
    else if(input$rstation_name == "O'Hare"){
      return(subset(ridership_ohare, ridership_ohare$the_year == input$rYear))
    }
    else{
      return(subset(ridership_jefferson, ridership_jefferson$the_year == input$rYear))
    }
  })
  
  
  output$leftbox <- renderPlot({
    if(input$type_x == "Default")
    {
      
      if(input$station_name == "UIC-Halsted"){
        df <- ridership_halsted %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
      else if(input$station_name == "O'Hare"){
        df <- ridership_ohare %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
      else{
        df <- ridership_jefferson %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
    }
    else if(input$type_x == "Week Day"){
      justOneYear <- justOneYearReactive()
      df <- justOneYear %>% 
        group_by(weekday) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)
    }
    else if(input$type_x == "Daily"){
      justOneYear <- justOneYearReactive()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma)
    }
    else if(input$type_x == "Monthly"){
      justOneYear <- justOneYearReactive()
      df <- justOneYear %>% 
        group_by(the_month) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=the_month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Montly", y="Rides") + scale_y_continuous(label=comma)
    }
    else{
      justOneYear <- justOneYearReactive()
      df <- justOneYear %>% 
        group_by(weekday) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)
      
    }
  })
  
  output$leftall <-
    renderPlot({
      if(input$type_x == "All"){
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)
      }
    })
  
  
  output$leftall2 <- renderPlot({
    if(input$type_x == "All"){
      justOneYear <- justOneYearReactive()
      df <- justOneYear %>% 
        group_by(the_month) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=the_month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Montly", y="Rides") + scale_y_continuous(label=comma)
    }
  })
  
  #df for tables
  #sort all data ascending
  df_halsted <- ridership_halsted %>% 
    group_by(the_year) %>% 
    summarise(rides = sum(rides))
  
  df_ohare <- ridership_ohare %>% 
    group_by(the_year) %>% 
    summarise(rides = sum(rides))
  
  df_jefferson <- ridership_jefferson %>% 
    group_by(the_year) %>% 
    summarise(rides = sum(rides))
  
  output$tab1 <- DT::renderDataTable(
    df_halsted, 
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
  )
  
  output$tab2 <- DT::renderDataTable(
    df_ohare, 
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
  )
  
  output$tab3 <- DT::renderDataTable(
    df_jefferson, 
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
  )
  
  output$rightbox <- renderPlot({
    if(input$rtype_x == "Default")
    {
      
      if(input$rstation_name == "UIC-Halsted"){
        df <- ridership_halsted %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
      else if(input$rstation_name == "O'Hare"){
        df <- ridership_ohare %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
      else{
        df <- ridership_jefferson %>% 
          group_by(the_year) %>% 
          summarise(rides = sum(rides))
        ggplot(df, aes(x=the_year, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)
      }
    }
    else if(input$rtype_x == "Week Day"){
      justOneYear <- justOneYearReactiver()
      df <- justOneYear %>% 
        group_by(weekday) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)
    }
    else if(input$rtype_x == "Daily"){
      justOneYear <- justOneYearReactiver()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma)
    }
    else if(input$rtype_x == "Monthly"){
      justOneYear <- justOneYearReactiver()
      df <- justOneYear %>% 
        group_by(the_month) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=the_month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Montly", y="Rides") + scale_y_continuous(label=comma)
    }
    else{
      justOneYear <- justOneYearReactiver()
      df <- justOneYear %>% 
        group_by(weekday) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)
      
    }
  })
  
  output$rightall <-
    renderPlot({
      if(input$rtype_x == "All"){
        justOneYear <- justOneYearReactiver()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)
      }
    })
  
  
  output$rightall2 <- renderPlot({
    if(input$rtype_x == "All"){
      justOneYear <- justOneYearReactiver()
      df <- justOneYear %>% 
        group_by(the_month) %>% 
        summarise(rides = sum(rides))
      ggplot(df, aes(x=the_month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Montly", y="Rides") + scale_y_continuous(label=comma)
    }
  })
}

shinyApp(ui = ui, server = server)
