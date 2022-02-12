library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


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
x_con <- sort(c("Daily", "Monthly", "Week Day", "All For Year", "Default"))
snams <- sort(c("O'Hare", "UIC-Halsted", "Jefferson Park"))
tflist <- c("FALSE","TRUE")
pageList <- c("Data","About")
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
                            selectInput("rstation_name", "Select the station name", snams, selected = "O'Hare"),
                            selectInput("rYear", "Select the year to visualize", years, selected = 2021),
                            selectInput("rtype_x", "Select the constraint", x_con, selected = "Default"),
                            selectInput("rtableCheck", "Show Table Values",tflist, selected = "FALSE")
                   ),
                   menuItem("Page options",
                            selectInput("pageOption", "Select page", pageList, selected = "Data")
                   )

  ),
  
  dashboardBody(
    conditionalPanel(
      condition = "input.pageOption == 'Data'",
      fluidRow(
        column(6,
               conditionalPanel(
                 condition = "input.tableCheck == 'FALSE'",
                 verticalLayout(
                   
                   plotOutput("leftbox",width="100%"),
                   conditionalPanel(condition = "input.type_x == 'All For Year'", plotOutput("leftall")),
                   conditionalPanel(condition = "input.type_x == 'All For Year'", plotOutput("leftall2"))
                 ),
               ),
               conditionalPanel(
                 condition = "input.tableCheck == 'TRUE'",
                 uiOutput("leftWithTable")
               ),
               
        ),
        column(6,
               conditionalPanel(
                 condition = "input.rtableCheck == 'FALSE'",
                 verticalLayout(
                     plotOutput("rightbox",width="100%"),
                   conditionalPanel(condition = "input.rtype_x == 'All For Year'", plotOutput("rightall")),
                   conditionalPanel(condition = "input.rtype_x == 'All For Year'", plotOutput("rightall2"))
                 )
               ),
               conditionalPanel(
                 condition = "input.rtableCheck == 'TRUE'",
                 uiOutput("rightWithTable")
               ),
        )
      )
    ),
    conditionalPanel(
      condition = "input.pageOption == 'About'",
      fluidRow(
        h1("About Page"),
        p("The data is from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
        p("Vivek Patel wrote this application."),
        p("Created: Spring 2022, Feb"),
        p("The application was created for Project 1 of Spring 2022 CS 424 with Dr. Johnson")
      )
    )
  )
)

server <- function(input, output) {
  theme_set(theme_grey(base_size = 14)) 
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
        df <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
      else if(input$station_name == "O'Hare"){
        df <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
      else{
        df <- aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
    }
    else if(input$type_x == "Week Day"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day of the week for",input$Year))
    }
    else if(input$type_x == "Daily"){
      justOneYear <- justOneYearReactive()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day for",input$Year))
    }
    else if(input$type_x == "Monthly"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each month for",input$Year))
    }
    else{
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day of the week for",input$Year))
      
    }
  })
  
  output$leftall <-
    renderPlot({
      if(input$type_x == "All For Year"){
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$station_name,"each day for",input$Year))
      }
    })
  
  
  output$leftall2 <- renderPlot({
    if(input$type_x == "All For Year"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each month for",input$Year))
    }
  })
  
  output$rightbox <- renderPlot({
    if(input$rtype_x == "Default")
    {
      
      if(input$rstation_name == "UIC-Halsted"){
        df <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
      else if(input$rstation_name == "O'Hare"){
        df <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
      else{
        df <- aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
    }
    else if(input$rtype_x == "Week Day"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
    }
    else if(input$rtype_x == "Daily"){
      justOneYear <- justOneYearReactiver()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day for",input$rYear))
    }
    else if(input$rtype_x == "Monthly"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each month for",input$rYear))
    }
    else{
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
      
    }
  })
  
  output$rightall <-
    renderPlot({
      if(input$rtype_x == "All For Year"){
        justOneYear <- justOneYearReactiver()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day for",input$rYear))
      }
    })
  
  
  output$rightall2 <- renderPlot({
    if(input$rtype_x == "All For Year"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month for",input$rYear))
    }
  })
  
  
  output$leftboxT <- renderPlot({
    if(input$type_x == "Default")
    {
      
      if(input$station_name == "UIC-Halsted"){
        df <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
      else if(input$station_name == "O'Hare"){
        df <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
      else{
        df <- aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$station_name,"Station"))
      }
    }
    else if(input$type_x == "Week Day"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day of the week for",input$Year))
    }
    else if(input$type_x == "Daily"){
      justOneYear <- justOneYearReactive()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day for",input$Year))
    }
    else if(input$type_x == "Monthly"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each month for",input$Year))
    }
    else{
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each day of the week for",input$Year))
      
    }
  })
  
  output$leftallT <-
    renderPlot({
      if(input$type_x == "All For Year"){
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$station_name,"each day for",input$Year))
      }
    })
  
  
  output$leftall2T <- renderPlot({
    if(input$type_x == "All For Year"){
      justOneYear <- justOneYearReactive()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$station_name,"each month for",input$Year))
    }
  })
  
  
  output$leftWithTable <- renderUI({
    if(input$type_x == "Default")
    {
      verticalLayout(
         if(input$station_name == "UIC-Halsted"){
           df_halsted <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
           colnames(df_halsted) = c("Year","Rides")
           splitLayout(
             plotOutput("leftboxT"),
             output$tabLeftHalsted <- DT::renderDataTable(
               df_halsted, 
               options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
               ), rownames = FALSE 
             )
           )
         }
         else if(input$station_name == "O'Hare"){
           df_ohare <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
           colnames(df_ohare) = c("Year","Rides")
           splitLayout(
             plotOutput("leftboxT"),
             output$df_ohare <- DT::renderDataTable(
               df_ohare, 
               options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
               ), rownames = FALSE 
             )
           )
         }
         else{
           df_jefferson <-aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
           colnames(df_jefferson) = c("Year","Rides")
           splitLayout(
             plotOutput("leftboxT"),
             output$df_jefferson <- DT::renderDataTable(
               df_jefferson, 
               options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
               ), rownames = FALSE 
             )
           )
         }
      )
    }
  else if(input$type_x == "Week Day"){
    justOneYear <- justOneYearReactive()
    df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
    colnames(df) = c("week day","rides")
    verticalLayout(
      splitLayout(
        plotOutput("leftboxT",width="100%"),
        DT::renderDataTable(
          df, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      ),
    )
  }
  else if(input$type_x == "Daily"){
    justOneYear <- justOneYearReactive()
    df <- data.frame(justOneYear$updated_date, justOneYear$rides)
    colnames(df) = c("date","rides")
    verticalLayout(
      splitLayout(
        plotOutput("leftboxT",width="100%"),
        DT::renderDataTable(
          df, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      ),
    )
  }
  else if(input$type_x == "Monthly"){
    justOneYear <- justOneYearReactive()
    df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
    colnames(df) = c("month","rides")
    verticalLayout(
      splitLayout(
        plotOutput("leftboxT",width="100%"),
        DT::renderDataTable(
          df, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      ),
    )
  }
  else{
    justOneYear <- justOneYearReactive()
    df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
    colnames(df) = c("week day","rides")
    df2 <- data.frame(justOneYear$updated_date, justOneYear$rides)
    colnames(df2) = c("date","rides")
    df3 <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
    colnames(df3) = c("month","rides")
    verticalLayout(
      splitLayout(
        plotOutput("leftboxT",width="100%"),
        DT::renderDataTable(
          df, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      ),
      splitLayout(
        plotOutput("leftallT",width="100%"),
        DT::renderDataTable(
          df2, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      ),
      splitLayout(
        plotOutput("leftall2T",width="100%"),
        DT::renderDataTable(
          df3, 
          options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
          ), rownames = FALSE 
        )
      )
    )
    
  }
  })
  
  output$rightboxT <- renderPlot({
    if(input$rtype_x == "Default")
    {
      
      if(input$rstation_name == "UIC-Halsted"){
        df <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
      else if(input$rstation_name == "O'Hare"){
        df <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
      else{
        df <- aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
        ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
      }
    }
    else if(input$rtype_x == "Week Day"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
    }
    else if(input$rtype_x == "Daily"){
      justOneYear <- justOneYearReactiver()
      ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day for",input$rYear))
    }
    else if(input$rtype_x == "Monthly"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each month for",input$rYear))
    }
    else{
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Day", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
      
    }
  })
  
  output$rightallT <-
    renderPlot({
      if(input$rtype_x == "All For Year"){
        justOneYear <- justOneYearReactiver()
        ggplot(justOneYear, aes(x=updated_date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
          labs(x="Date", y="Rides") + scale_y_continuous(label=comma)+ ggtitle(paste(input$rstation_name,"each day for",input$rYear))
      }
    })
  
  
  output$rightall2T <- renderPlot({
    if(input$rtype_x == "All For Year"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
        labs(x="Monthly", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month for",input$rYear))
    }
  })
  
  
  output$rightWithTable <- renderUI({
    if(input$rtype_x == "Default")
    {
      verticalLayout(
        if(input$rstation_name == "UIC-Halsted"){
          df_halsted <- aggregate(ridership_halsted$rides, by=list(Category=ridership_halsted$the_year), FUN=sum)
          colnames(df_halsted) = c("Year","Rides")
          splitLayout(
            plotOutput("rightboxT"),
            output$tabLeftHalsted <- DT::renderDataTable(
              df_halsted, 
              options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE 
            )
          )
        }
        else if(input$rstation_name == "O'Hare"){
          df_ohare <- aggregate(ridership_ohare$rides, by=list(Category=ridership_ohare$the_year), FUN=sum)
          colnames(df_ohare) = c("Year","Rides")
          splitLayout(
            plotOutput("rightboxT"),
            output$df_ohare <- DT::renderDataTable(
              df_ohare, 
              options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE 
            )
          )
        }
        else{
          df_jefferson <-aggregate(ridership_jefferson$rides, by=list(Category=ridership_jefferson$the_year), FUN=sum)
          colnames(df_jefferson) = c("Year","Rides")
          splitLayout(
            plotOutput("rightboxT"),
            output$df_jefferson <- DT::renderDataTable(
              df_jefferson, 
              options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE 
            )
          )
        }
      )
    }
    else if(input$rtype_x == "Week Day"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      colnames(df) = c("week day","rides")
      verticalLayout(
        splitLayout(
          plotOutput("rightboxT",width="100%"),
          DT::renderDataTable(
            df, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        ),
      )
    }
    else if(input$rtype_x == "Daily"){
      justOneYear <- justOneYearReactiver()
      df <- data.frame(justOneYear$updated_date, justOneYear$rides)
      colnames(df) = c("date","rides")
      verticalLayout(
        splitLayout(
          plotOutput("rightboxT",width="100%"),
          DT::renderDataTable(
            df, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        ),
      )
    }
    else if(input$rtype_x == "Monthly"){
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      colnames(df) = c("month","rides")
      verticalLayout(
        splitLayout(
          plotOutput("rightboxT",width="100%"),
          DT::renderDataTable(
            df, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        ),
      )
    }
    else{
      justOneYear <- justOneYearReactiver()
      df <- aggregate(justOneYear$rides, by=list(Category=justOneYear$weekday), FUN=sum)
      colnames(df) = c("Week day","rides")
      df2 <- data.frame(justOneYear$updated_date, justOneYear$rides)
      colnames(df2) = c("date","rides")
      df3 <- aggregate(justOneYear$rides, by=list(Category=justOneYear$the_month), FUN=sum)
      colnames(df3) = c("month","rides")
      verticalLayout(
        splitLayout(
          plotOutput("rightboxT",width="100%"),
          DT::renderDataTable(
            df, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        ),
        splitLayout(
          plotOutput("rightallT",width="100%"),
          DT::renderDataTable(
            df2, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        ),
        splitLayout(
          plotOutput("rightall2T",width="100%"),
          DT::renderDataTable(
            df3, 
            options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
            ), rownames = FALSE 
          )
        )
      )
      
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

#TODO LIST
#Finish Adding tables
#Remove Years from label 
#ohare random white spaces
