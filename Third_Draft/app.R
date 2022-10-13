setwd("/Users/eliot/Desktop/Fall 2022/Independent Study/Independent-Study/Third_Draft")
library(shiny)
library(tidyverse)
library(lubridate)

# Variable Creatio 
Unfiltered_Data <- read_csv("ShareMSCUsage2022-01-19_2022-05-27.csv")


# Grouping In/Out Times
Unfiltered_Data <- Unfiltered_Data %>%
  mutate(
    Time.In.Grouped = case_when(
      MinIn < 15 ~ '0',
      MinIn >= 15 & MinIn < 30 ~ '15',
      MinIn >= 30 & MinIn < 45 ~ '30',
      MinIn >= 45 ~ '45'
    ),
    Time.Out.Grouped = case_when(
      MinOut < 15 ~ '0',
      MinOut >= 15 & MinOut < 30 ~ '15',
      MinOut >= 30 & MinOut < 45 ~ '30',
      MinOut >= 45 ~ '45'
    )
  )

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

## DATA CREATION MIGHT NEED TO BE IN SERVER

Avg_Density <- Density(Data)

# Creating Vectors for Choices

Courses <- c("M105Q", "M121Q", "M151Q", "M161Q", "M165Q", "M171Q", "M172Q", "M182Q", "M221", "M273Q", "M274", "STAT 216Q", "STAT 217Q")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("MSC Usage"),
  tabPanel("Date Range",
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("dates", 
                              "Date range",
                              start = "2022-01-24", 
                              end = as.character(Sys.Date()))),
             mainPanel()
           )),
  
  ## Course Filter
  
  tabPanel("Courses of Interest",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("courses", 
                              "Courses of Interest",
                              choices = Courses,
                              selected = Courses)),
             mainPanel()
           )),
  
  ## Average Density
  
  tabPanel("Average Density",
           sidebarLayout(
             sidebarPanel(
               selectInput("explanatory_scatter", "Average density by ______", choices = c("WeekDay", "Hour", "Time"))
             ),
             mainPanel(
               plotOutput("scatterplot")
             )
           )),
  
  ## Tables
  
  tabPanel("Tables",
           sidebarLayout(
             sidebarPanel(
               selectInput("table", "What would you like the table to display?", choices = c("Average Students by Hour", "Average Tutors Needed by Hour"))),
             mainPanel(
               
             )
           ))
  
)

# Define server logic
server <- function(input, output) {
  
  
  Data <- reactive({
    Unfiltered_Data %>%
      filter(Date >= input$dateRange[1],
             Date <= input$dateRange[2],
             Courses %in% input$courses)
  })

  
  observe({
    if(input$explanatory_scatter == 'WeekDay') {
      output$scatterplot <- renderPlot({
        Avg_Density %>%
          ggplot(
            mapping = aes(
              x = WeekDay,
              y = Avg.Pop
            )
          ) +
          geom_boxplot() +
          ggtitle('Average Population for Mondays and Tuesdays') +
          labs(x = 'Time', y = 'Average Population') +
          theme_dark() +
          scale_color_brewer() +
          ylim(0, 80)
        
      })
    } else if(input$explanatory_scatter == 'Hour') {
      output$scatterplot <- renderPlot({
        Avg_Density %>%
          group_by(Hour) %>%
          ggplot(
            mapping = aes(
              x = Hour,
              y = Avg.Hourly.Pop,
              color = WeekDay
            )
          ) +
          geom_vline(mapping=NULL, xintercept=seq(9,18, 1),colour='grey45') +
          geom_line(size = 1.5) +
          ggtitle('Average Population for Mondays and Tuesdays') +
          labs(x = 'Time', y = 'Average Population') +
          theme_dark() +
          scale_color_brewer()
      })
    } else if(input$explanatory_scatter == 'Time') {
      output$scatterplot <- renderPlot({
        Avg_Density %>%
          ggplot(
            mapping = aes(
              x = as.numeric(Time),
              y = Avg.Pop,
              color = WeekDay
            )
          ) +
          geom_vline(mapping=NULL, xintercept=seq(9,18, 0.25),colour='grey45') +
          geom_line(size = 1.5) +
          ggtitle('Average Population for Mondays and Tuesdays') +
          labs(x = 'Time', y = 'Average Population') +
          theme_dark() +
          scale_color_brewer()
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
