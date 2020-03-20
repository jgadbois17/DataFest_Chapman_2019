#--------------------------------------------------

#Wellness Dataset
#More for the players

#--------------------------------------------------

#Libraries

#General Purposes
library(tidyverse)
library(lubridate)
library(psych)

#Missing Values
library(mice)
library(VIM)

#--------------------------------------------------

#Variables 
#----------
#Date
#PlayerID
#Fatigue - Degree of fatigue (1-7)
#Soreness - Degree pf Soreness (1-7)
#Desire - Degree of motivation (1-7)
#Irritability - Degree of iorritability (1-7)
#BedTime - time player went to bed 
#WakeTime - time player woke up 
#Sleephours - number of hours of sleep the player got 
#SleepQuality - Quality of sleep (1-7)
#MonitoringScore - The sum of the five scale variables 
#Pain - Is the player in pain?
#Illness - Is the player feeling ill?
#Menstruation - Is the player on her period?
#Nutrition - Hows is the players nutrition?
#NutritionAdjustment - Has the player made a nutrition adjustment that day?
#USGMeasurement - Was hydration tested?
#USG - Urine Specific gravity 
###above 1.025 indicated mild dehydration
#TrainingReadiness - How ready is the player to play?
#----------

#Data
wellness<- read_csv('data/wellness.csv')

str(wellness)
wellness %>% print(width = Inf)

#Missing Data Function
pm<- function(x){sum(is.na(x)/length(x)*100)}

apply(wellness, 2, pm)

#Formatting Data
wellness$TrainingReadiness<- str_split(wellness$TrainingReadiness, '%', 
                                       simplify = T)[,1]

wellness$TrainingReadiness<- as.numeric(wellness$TrainingReadiness)/100

wellness %>% count(PlayerID)

wellness %>% group_by(PlayerID) %>%
  count(USGMeasurement) %>%
  print(n=Inf)

#Adding Variables For Year, Month, Day
wellness$year<- as.factor(year(wellness$Date))
wellness$month<- as.factor(month(wellness$Date))
wellness$mday<- mday(wellness$Date)

#--------------------------------------------------

str(wellness)

#Dubai Tournament 

#Game Day 1 - Games 1-3
wellness %>% 
  filter(year=='2017', month=='11', mday==30) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 4-6
wellness %>% 
  filter(year=='2017', month=='12', mday==1) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#Sydney Tournament 

#Game Day 1 - Games 7-9
wellness %>% 
  filter(year=='2018', month=='1', mday==26) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 10-11
wellness %>% 
  filter(year=='2018', month=='1', mday==27) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 3 - Game 12
wellness %>% 
  filter(year=='2018', month=='1', mday==28) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#Commonwealth Tournament

#Game Day 1 - Games 13-14
wellness %>% 
  filter(year=='2018', month=='4', mday==13) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Game 15
wellness %>% 
  filter(year=='2018', month=='4', mday==14) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 3 - Games 16-17
wellness %>% 
  filter(year=='2018', month=='4', mday==15) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#Kitakyushu Tournament 

#Game Day 1 - Games 18-20
wellness %>% 
  filter(year=='2018', month=='4', mday==21) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 21-22
wellness %>% 
  filter(year=='2018', month=='4', mday==22) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#Langford Tournament 

#Game Day 1 - Games 23-25
wellness %>% 
  filter(year=='2018', month=='5', mday==12) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 26-28
wellness %>% 
  filter(year=='2018', month=='5', mday==13) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#Paris Tournament 

#Game Day 1 - Games 29-31
wellness %>% 
  filter(year=='2018', month=='6', mday==8) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 32-33
wellness %>% 
  filter(year=='2018', month=='6', mday==9) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 3 - Game 34
wellness %>% 
  filter(year=='2018', month=='6', mday==10) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

#World Cup

#Game Day 1 - Games 35-36
wellness %>% 
  filter(year=='2018', month=='7', mday==20) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game Day 2 - Games 37-38
wellness %>% 
  filter(year=='2018', month=='7', mday==21) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#--------------------------------------------------

str(wellness)

#Game 9
g9players<- c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14)

game9<- wellness %>% 
  filter(year=='2018', month=='1', mday==26, 
         PlayerID==2|PlayerID==3|PlayerID==4|PlayerID==7|PlayerID==8|
           PlayerID==9|PlayerID==10|PlayerID==11|PlayerID==12|PlayerID==13|
           PlayerID==14) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

game9<- wellness %>% 
  filter(year=='2018', month=='1', mday==26) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

#Game 38
game38<- wellness %>% 
  filter(year=='2018', month=='7', mday==21) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)

pgame38<- wellness %>% 
  filter(year=='2018', month=='7', mday==21,
         PlayerID==2|PlayerID==3|PlayerID==4|PlayerID==7|PlayerID==10|
           PlayerID==11|PlayerID==13|PlayerID==14) %>%
  select(PlayerID, Fatigue, Soreness, Desire, Irritability,
         SleepHours, SleepQuality, MonitoringScore, Pain,
         Illness, Menstruation, TrainingReadiness) %>%
  print(width=Inf)





