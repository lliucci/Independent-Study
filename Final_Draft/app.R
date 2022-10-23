setwd('C:/Users/eliot/Desktop/Fall 2022/Independent Study/Independent-Study/Final_Draft')
library(shiny)
library(tidyverse)
library(lubridate)

# Variable Creatio 
Unfiltered_Data <- read_csv("../data/ShareMSCUsage2022-01-19_2022-05-27.csv")


# Grouping In/Out Times
Unfiltered_Data <- Unfiltered_Data %>%
  mutate(
    Time.In.Grouped = case_when(
      MinIn < 15 ~ '0',
      MinIn >= 15 & MinIn < 30 ~ '15',
      MinIn >= 30 & MinIn < 45 ~ '30',
      MinIn >= 45 ~ '45'),
    Time.Out.Grouped = case_when(
      MinOut < 15 ~ '0',
      MinOut >= 15 & MinOut < 30 ~ '15',
      MinOut >= 30 & MinOut < 45 ~ '30',
      MinOut >= 45 ~ '45'
    ),
    Course = factor(Course),
    WeekDay = fct_relevel(WeekDay, c("Mon", "Tue", "Wed", "Thu", "Fri"))
  )

Courses <- c("M105", "M121", "M151", "M161", "M165", "M171", "M172", "M182", "M221", "M273", "M274", "STAT 216", "STAT 217")

# Density Function

Density <- function(DataSet){
  Density.In <- DataSet %>%
    mutate(
      Date = as.factor(Date),
      HourIn = as.factor(HourIn),
      Time.In.Grouped = as.factor(Time.In.Grouped)
    ) %>%
    group_by(WeekDay, HourIn, Time.In.Grouped, .drop = F) %>%
    count() %>%
    mutate(Total.In = n)
  Density.Out <- DataSet %>%
    mutate(
      Date = as.factor(Date),
      HourOut = as.factor(HourOut),
      Time.Out.Grouped = as.factor(Time.Out.Grouped)
    ) %>%
    group_by(WeekDay, HourOut, Time.Out.Grouped, .drop = F) %>%
    count() %>%
    mutate(Total.Out = n)
  Density.In <- Density.In %>%
    select(HourIn, Time.In.Grouped, Total.In, WeekDay) %>%
    subset(WeekDay %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))
  Density.Out <- Density.Out %>%
    select(HourOut, Time.Out.Grouped, Total.Out, WeekDay) %>%
    subset(WeekDay %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))
  Density.Out <- Density.Out %>%
    filter(HourOut != 19)
  Density.Out <- Density.Out %>%
    filter(HourOut != 8)
  Density <- cbind(Density.In, Density.Out)
  Density <- Density %>%
    mutate(Net = Total.In - Total.Out)
  Density <- Density %>%
    group_by(WeekDay...4) %>%
    mutate(Population = cumsum(Net))
  Density <- Density %>%
    unite(Time.Frame, HourIn, Time.In.Grouped, sep = ':') %>%
    select(Time.Frame, Population, WeekDay...4) %>%
    rename(WeekDay = WeekDay...4)
  Density <- Density %>%
    mutate(Time = as.numeric(hm(Time.Frame))/3600)
  Mondays <- count(DataSet %>% group_by(WeekNum) %>% filter(WeekDay == 'Mon'))
  Num.Mondays <- nrow(Mondays)
  Tuesdays <- count(DataSet %>% group_by(WeekNum) %>% filter(WeekDay == 'Tue'))
  Num.Tuesdays <- nrow(Tuesdays)
  Wednesdays <- count(DataSet %>% group_by(WeekNum) %>% filter(WeekDay == 'Wed'))
  Num.Wednesdays <- nrow(Wednesdays)
  Thursdays <- count(DataSet %>% group_by(WeekNum) %>% filter(WeekDay == 'Thu'))
  Num.Thursdays <- nrow(Thursdays)
  Fridays <- count(DataSet %>% group_by(WeekNum) %>% filter(WeekDay == 'Fri'))
  Num.Fridays <- nrow(Fridays)
  Density <- Density %>%
    mutate(Avg.Pop = case_when(
      WeekDay == 'Mon' ~ Population / Num.Mondays,
      WeekDay == 'Tue' ~ Population / Num.Tuesdays,
      WeekDay == 'Wed' ~ Population / Num.Wednesdays,
      WeekDay == 'Thu' ~ Population / Num.Thursdays,
      WeekDay == 'Fri' ~ Population / Num.Fridays
    ))
  Density <- Density %>%
    select(Time, WeekDay, Avg.Pop) %>%
    mutate(Hour = hour(hm(Time)))
  Density <- Density %>%
    group_by(Hour, WeekDay) %>%
    mutate(Avg.Hourly.Pop = mean(Avg.Pop))
}

ui <- fluidPage(

  titlePanel("MSC Usage Analysis"),
  
  fluidRow(column(4,
            wellPanel(
              actionButton("generate", "Create Outputs", color = "white"),
                dateRangeInput("dates", 
                                       "Date range",
                                       start = "2022-01-24", 
                                       end = as.character(Sys.Date())),
                        htmlOutput = "DateUI",
                        checkboxGroupInput("courses", 
                                           "Courses of Interest",
                                           choices = Courses),
            actionLink("selectall","Select All"))),
           column(6,
                  tabsetPanel(
                    tabPanel("Student Usage",
                             selectInput("explanatory_scatter", 
                                         "Average density by ______", 
                                         choices = c("Day of Week" = "WeekDay", "Hour", "15 Minute Intervals" = "Time")),
                             plotOutput("scatterplot")),
                    tabPanel("Distribution of Courses",
                             plotOutput("distribution")),
                    tabPanel("Tables",
                             fluidRow(column(7,
                             h6("Number of Students"),
                             tableOutput("table1"),
                             h6("Number of Tutors"),
                             tableOutput("table2")),
                             column(5,
                             h6("Course Contributions"),
                             tableOutput("contributions"),
                             h6("Distinct vs. Non-Distinct Students"),
                             tableOutput("distinct"))))
                  )))

)

server <- function(input, output, session) {

  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"courses", 
                               "Courses of Interest",
                               choices = Courses)
    }
    else
    {
      updateCheckboxGroupInput(session,"courses", 
                               "Courses of Interest",
                               choices = Courses,
                               selected = Courses)
    }
  })
  
  
  ## Interactive Data Filtering
  
  Avg_Density <- eventReactive(input$generate, {
    Filtered_Data <- Unfiltered_Data %>%
      filter(Course %in% input$courses,
             Date >= input$dates[1],
             Date <= input$dates[2])
    Density(Filtered_Data)
  })
  
  Filt_Data <- eventReactive(input$generate, {
    Unfiltered_Data %>%
      filter(Course %in% input$courses,
             Date >= input$dates[1],
             Date <= input$dates[2])
  })
  
  
  ## Interactive Plot Display
  
  observe({
    if(input$explanatory_scatter == 'WeekDay') {
      output$scatterplot <- renderPlot({
        Avg_Density() %>%
          ggplot(
            mapping = aes(
              x = WeekDay,
              y = Avg.Pop
            )
          ) +
          geom_boxplot() +
          ggtitle('Average Number of Students by Day of Week') +
          labs(x = 'Day', y = 'Average Number of Students')
        
      })
    } else if(input$explanatory_scatter == 'Hour') {
      output$scatterplot <- renderPlot({
        Avg_Density() %>%
          group_by(Hour) %>%
          ggplot(
            mapping = aes(
              x = Hour,
              y = Avg.Hourly.Pop,
              color = WeekDay
            )
          ) +
          geom_vline(mapping=NULL, xintercept=seq(9,18, 1),colour='white') +
          geom_line(size = 1.5) +
          ggtitle('Average Number of Students by Hour') +
          labs(x = 'Time', y = 'Average Number of Students') +
          xlim(c(9,17))
      })
    } else if(input$explanatory_scatter == 'Time') {
      output$scatterplot <- renderPlot({
        Avg_Density() %>%
          ggplot(
            mapping = aes(
              x = as.numeric(Time),
              y = Avg.Pop,
              color = WeekDay
            )
          ) +
          geom_vline(mapping=NULL, xintercept=seq(9,18, 0.25),colour='white') +
          geom_line(size = 1.5) +
          ggtitle('Average Number of Students by Time of Day') +
          labs(x = 'Time', y = 'Average Number of Students')
      })
    }
  })
  
  output$distribution <- renderPlot({
    if(length(input$courses) == 1){
      Filt_Data() %>%
        ggplot(aes(
          x = WeekDay,
          y = (..count..)/sum(..count..),
          fill = Course)) +
        geom_bar(color = 'black') +
        ggtitle('Course Density by Day of the Week') +
        labs(x = 'Day', y = 'Relative Frequency', subtitle = 'for Selected Courses')
    } else {
      Filt_Data() %>%
        ggplot(aes(
          x = WeekDay,
          y = (..count..)/sum(..count..),
          fill = Course)) +
        geom_bar(color = 'black', position = "fill") +
        ggtitle('Course Density by Day of the Week') +
        labs(x = 'Day', y = 'Relative Frequency', subtitle = 'for Selected Courses')
    }})
  
      output$table1 <- renderTable({
        Avg_Density() %>%
          group_by(Hour, WeekDay) %>%
          summarize(Avg_Pop = mean(Avg.Pop)) %>%
          select(Hour, Avg_Pop, WeekDay) %>%
          mutate(Avg_Pop = round(Avg_Pop)) %>%
          pivot_wider(
            names_from = WeekDay,
            values_from = Avg_Pop)
      },
      digits = 0)
  
      output$table2 <- renderTable({
        Avg_Density() %>%
          group_by(Hour, WeekDay) %>%
          summarize(Avg_Pop = mean(Avg.Pop)) %>%
          select(Hour, Avg_Pop, WeekDay) %>%
          mutate(Avg_Pop = round(Avg_Pop/5)) %>%
          pivot_wider(
            names_from = WeekDay,
            values_from = Avg_Pop)
      },
      digits = 0)
  
  output$contributions <- renderTable({
    Filt_Data() %>%
      group_by(Course) %>%
      summarize(Total_Hours = sum(Length)/60)
  })
  
  output$distinct <- renderTable({
    Filt_Data() %>%
      group_by(WeekDay) %>%
      summarize(Distinct = n_distinct(IDNum),
                Non_Distinct = n())
  })
 
}

shinyApp(ui = ui, server = server)
