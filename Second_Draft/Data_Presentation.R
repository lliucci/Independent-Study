setwd("/Users/eliot/Desktop/Fall 2022/Independent Study/Independent-Study/Second_Draft")
library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)

# Variable Creation
Data <- read_csv("../Third_Draft/ShareMSCUsage2022-01-19_2022-05-27.csv")

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
Data <- Data %>%
  mutate(Math_or_Stats = ifelse(Course == 'STAT 217' | Course == 'STAT 216', 'Stats', 'Math'),
         Week = strftime(Date, format = '%V'))

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


Courses <- c("M105Q", "M121Q", "M151Q", "M161Q", "M165Q", "M171Q", "M172Q", "M182Q", "M221", "M273Q", "M274", "STAT 216", "STAT 217")

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
  ggtitle('Average Population') +
  labs(x = 'Time', y = 'Average Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80)

## Hourly Population
Avg_Density %>%
  ggplot(
    mapping = aes(
      x = factor(Hour),
      y = Avg.Pop
    )
  ) +
  geom_boxplot() +
  ggtitle('Hourly Population Distribution') +
  labs(x = 'Hour', y = 'Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80)

## Daily Average Population
Avg_Density %>%
  ggplot(
    mapping = aes(
      x = WeekDay,
      y = Avg.Pop
    )
  ) +
  geom_boxplot() +
  ggtitle('Population Distributions by Day of Week') +
  labs(x = 'Day', y = 'Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80)

## Density Function
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
  Mondays <- count(Data %>% group_by(Week) %>% filter(WeekDay == 'Mon'))
  Num.Mondays <- nrow(Mondays)
  Tuesdays <- count(Data %>% group_by(Week) %>% filter(WeekDay == 'Tue'))
  Num.Tuesdays <- nrow(Tuesdays)
  Wednesdays <- count(Data %>% group_by(Week) %>% filter(WeekDay == 'Wed'))
  Num.Wednesdays <- nrow(Wednesdays)
  Thursdays <- count(Data %>% group_by(Week) %>% filter(WeekDay == 'Thu'))
  Num.Thursdays <- nrow(Thursdays)
  Fridays <- count(Data %>% group_by(Week) %>% filter(WeekDay == 'Fri'))
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
    select(Time, WeekDay, Avg.Pop)
}



## MSC Density
Avg_Density %>%
  ggplot(
    mapping = aes(
      x = as.numeric(Time),
      y = Avg.Pop,
      color = WeekDay
    )
  ) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  ggtitle('Average Population Throughout the Day') +
  labs(x = 'Time', y = 'Average Population') +
  theme_dark() +
  scale_color_brewer() +
  ylim(0, 80) +
  theme(legend.position = "top")

## Density over Time Tables
Density.Pivoted <- Avg_Density %>%
  select(Time, Avg.Pop, WeekDay) %>%
  mutate(Avg.Pop = ceiling(Avg.Pop)) %>%
  pivot_wider(
    names_from = WeekDay,
    values_from = Avg.Pop)
Density.Pivoted %>%
  kable(caption = 'Average Population Throughout Each Weekday') %>%
  kable_material(c('striped', 'hover'))


## CLS Recommended Tutors
Stats <- Data %>%
  filter(Math_or_Stats == 'Stats')
Math <- Data %>%
  filter(Math_or_Stats == 'Math') %>%
  filter(HourOut != 19,
         HourOut != 8,
         HourIn != 8)
Stat_Density <- Density(Stats)
Math_Density <- Density(Math)
Math_Density <- Math_Density %>%
  mutate(Math_Tutors_Needed = as.integer(Avg.Pop / 5)) %>%
  rename(Avg_Math_Population = Avg.Pop)
Stat_Density <- Stat_Density %>%
  mutate(Stat_Tutors_Needed = as.integer(Avg.Pop / 5)) %>%
  rename(Avg_Stat_Population = Avg.Pop)
Total_Density <- cbind(Stat_Density, Math_Density[,3:4])
Total_Hourly_Density <- Total_Density %>%
  mutate(Hour = as.integer(Time)) %>%
  group_by(Hour, WeekDay) %>%
  mutate(Hourly_Math_Population = as.integer(mean(Avg_Math_Population)),
         Hourly_Stat_Population = as.integer(mean(Avg_Stat_Population))) %>%
  select(Hour, Hourly_Math_Population, Hourly_Stat_Population) %>%
  distinct(Hour, Hourly_Math_Population, Hourly_Stat_Population) %>%
  mutate(Math_Tutors_Needed = as.integer(Hourly_Math_Population / 5),
         Stat_Tutors_Needed = as.integer(Hourly_Stat_Population / 5),
         Stat_Tutors_Needed = str_replace(Stat_Tutors_Needed, "0", "1"),
         Math_Tutors_Needed = str_replace(Math_Tutors_Needed, "0", "1"))

Math.Tutor.Needs <- Total_Hourly_Density %>%
  select(Hour, Math_Tutors_Needed, WeekDay) %>%
  pivot_wider(
    names_from = WeekDay,
    values_from = Math_Tutors_Needed)
Math.Tutor.Needs %>%
  kable(caption = 'Average Number of Math Tutors Needed') %>%
  kable_material(c('striped', 'hover'))

Stat.Tutor.Needs <- Total_Hourly_Density %>%
  select(Hour, Stat_Tutors_Needed, WeekDay) %>%
  pivot_wider(
    names_from = WeekDay,
    values_from = Stat_Tutors_Needed)
Stat.Tutor.Needs %>%
  kable(caption = 'Average Number of Stat Tutors Needed') %>%
  kable_material(c('striped', 'hover'))


Avg_Density %>%
  group_by(Hour, WeekDay) %>%
  summarize(mean(Avg.Pop))
