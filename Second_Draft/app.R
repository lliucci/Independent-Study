setwd("/Users/eliot/Desktop/Fall 2022/Independent Study/Independent-Study/Second_Draft")
library(shiny)
library(tidyverse)
library(lubridate)

# Variable Creation
Data <- read_csv("ShareMSCUsage2022-01-19_2022-05-27.csv")

# Grouping In/Out Times
Data <- Data %>%
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

# Creating Density Data

Density.In <- Data %>%
  mutate(
    Date = as.factor(Date),
    HourIn = as.factor(HourIn),
    Time.In.Grouped = as.factor(Time.In.Grouped)
  ) %>%
  group_by(WeekDay, HourIn, Time.In.Grouped, .drop = F) %>%
  count() %>%
  mutate(Total.In = n)
Density.Out <- Data %>%
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
Mondays <- count(Data %>% group_by(WeekNum) %>% filter(WeekDay == 'Mon'))
Num.Mondays <- nrow(Mondays)
Tuesdays <- count(Data %>% group_by(WeekNum) %>% filter(WeekDay == 'Tue'))
Num.Tuesdays <- nrow(Tuesdays)
Wednesdays <- count(Data %>% group_by(WeekNum) %>% filter(WeekDay == 'Wed'))
Num.Wednesdays <- nrow(Wednesdays)
Thursdays <- count(Data %>% group_by(WeekNum) %>% filter(WeekDay == 'Thu'))
Num.Thursdays <- nrow(Thursdays)
Fridays <- count(Data %>% group_by(WeekNum) %>% filter(WeekDay == 'Fri'))
Num.Fridays <- nrow(Fridays)
Avg_Density <- Density %>%
  mutate(Avg.Pop = case_when(
    WeekDay == 'Mon' ~ Population / Num.Mondays,
    WeekDay == 'Tue' ~ Population / Num.Tuesdays,
    WeekDay == 'Wed' ~ Population / Num.Wednesdays,
    WeekDay == 'Thu' ~ Population / Num.Thursdays,
    WeekDay == 'Fri' ~ Population / Num.Fridays
  ))
Avg_Density <- Avg_Density %>%
  select(Time, WeekDay, Avg.Pop) %>%
  mutate(Hour = hour(hm(Time)))

Courses <- c("M105Q", "M121Q", "M151Q", "M161Q", "M165Q", "M171Q", "M172Q", "M182Q", "M221", "M273Q", "M274", "STAT 216Q", "STAT 217Q")

Questions <- c("Trends by Major",
               "Busiest Times by Class")

Density_Minute_Plot <- Avg_Density %>%
  ggplot(
    mapping = aes(
      x = as.numeric(Time),
      y = Avg.Pop,
      color = WeekDay
    )
  ) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  ggtitle('Average Population for Mondays and Tuesdays') +
  labs(x = 'Time', y = 'Average Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80)

Density_Hourly_Plot <- Avg_Density %>%
  ggplot(
    mapping = aes(
      x = factor(Hour),
      y = Avg.Pop
    )
  ) +
  geom_boxplot() +
  ggtitle('Average Population for Mondays and Tuesdays') +
  labs(x = 'Time', y = 'Average Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80)

Density_Daily_Plot <- Avg_Density %>%
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

# Define UI for application that draws a histogram
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
    
## SCATTERPLOT
    
    tabPanel("Scatterplot",
           sidebarLayout(
             sidebarPanel(
               selectInput("response_scatter", "What is your Y variable?", choices = c("Avg.Pop")),
               selectInput("explanatory_scatter", "What is your X variable?", choices = c("WeekDay", "Hour", "Time")),
               selectInput("grouping_scatter", "How would you like to group the data?", choices = c("None", "College", "Class"))
             ),
             mainPanel(
               plotOutput("scatterplot")
             )
           )),
    
## BOXPLOT  
    
    tabPanel("Boxplot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("explanatory_box", "What is your X variable?", choices = c("WeekDay", "Hour", "Time")),
                 selectInput("grouping_box", "How would you like to group the data?", choices = c("None", "College", "Class"))
               ),
               mainPanel(
                 plotOutput("boxplot")
               )
             ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
      Avg_Density %>%
        ggplot(aes(
          x = input$explanatory_scatter,
          y = input$response_scatter,
          color = input$grouping_scatter
        )) +
        geom_point()
      })
    
    output$boxplot <- renderPlot({
      Avg_Density %>%
        ggplot(aes(
          x = input$explanatory_boxplot,
          color = input$grouping_boxplot
        ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
