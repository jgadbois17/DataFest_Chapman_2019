#--------------------------------------------------

#Wellness Dataset
#More for the players

#--------------------------------------------------

#Libraries

#General Purposes
library(tidyverse)
library(lubridate)
library(psych)
library(caret)

#For Regression
library(rcompanion)
library(MASS)

#Missing Values
library(mice)
library(VIM)

#--------------------------------------------------

wellness<- read_csv('data/wellness.csv')
str(wellness)

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

wellness %>% print(width=Inf)

#Dropping Variables
wellness<- wellness %>% select(-BedTime, -WakeTime, -USG, -USGMeasurement)
wellness<- wellness %>% select(-Menstruation, -Nutrition, -NutritionAdjustment)

#Converting some variables to factors
wellness$Pain<- as.factor(wellness$Pain)
wellness$Illness<- as.factor(wellness$Illness)
wellness$PlayerID<- as.factor(wellness$PlayerID)

#Converting all variables to a 1-10 scale
wellness$Fatigue<- (wellness$Fatigue/7)*10
wellness$Soreness<- (wellness$Soreness/7)*10
wellness$Desire<- (wellness$Desire/7)*10
wellness$Irritability<- (wellness$Irritability/7)*10
wellness$SleepQuality<- (wellness$SleepQuality/7)*10
wellness$MonitoringScore<- (wellness$MonitoringScore/35)*10
wellness$TrainingReadiness<- wellness$TrainingReadiness*10

#--------------------------------------------------

#Subsets
#Dubai
wdubai<- wellness %>% filter(year==2017)
wdubai<- wdubai %>% filter(month==11&mday==30|month==12&mday==1)
str(wdubai)
wdubai %>% count(Date)

#Sydney
wsydney<- wellness %>% filter(year==2018, month==1)
wsydney<- wsydney %>% filter(mday==26|mday==27|mday==28)
wsydney %>% count(Date)

#Commonwealth
common<- wellness %>% filter(year==2018, month==4, mday==13|mday==14|mday==15)
common %>% count(Date)

#Kitakyushu
kita<- wellness %>% filter(year==2018, month==4, mday==21|mday==22)
kita %>% count(Date)

#Langford 
langford<- wellness %>% filter(year==2018, month==5, mday==12|mday==13)
langford %>% count(Date)

#Paris
paris<- wellness %>% filter(year==2018, month==6, mday==8|mday==9|mday==10)
paris %>% count(Date)

#World Cup 
world<- wellness %>% filter(year==2018, month==7, mday==20|mday==21)
world %>% count(Date)

#--------------------------------------------------

#Game Day Dataset from Wellness
gameday<- rbind(wdubai, wsydney, common, kita, langford, paris, world)
gameday %>% count(Date)
gameday<- gameday %>% select(-year, -month, -mday)

gameday %>% count(PlayerID)

gameday %>% print(width=Inf)
str(gameday)

#Regression on Monitoring Score
plotNormalHistogram(gameday$TrainingReadiness)
shapiro.test(gameday$TrainingReadiness)
plotNormalHistogram(gameday$MonitoringScore)
shapiro.test(gameday$MonitoringScore)

fit<- glm(MonitoringScore ~ .,
          data = gameday,
          family = gaussian(identity))
summary(fit)

stepfit<- stepAIC(fit, direction = 'both')
summary(stepfit)
plot(stepfit)

#Model Fit
null<- glm(MonitoringScore~1, data = gameday, family = gaussian(identity))
deviance<- -2*(logLik(null)-logLik(stepfit))
(pval<- pchisq(deviance, df=8, lower.tail = F))

RMSE(predict(stepfit, gameday), gameday$MonitoringScore)

#Visualization
gameday %>% print(width=Inf)

#--------------------------------------------------

gameday %>% print(width=Inf)
str(gameday)

#GPS Dataset
gps<- read_csv('data/gps.csv')
str(gps)
gps %>% print(width=Inf)

wp<- c(1:17)

g1<- gps %>% filter(PlayerID==1|PlayerID==2|PlayerID==3|PlayerID==4|PlayerID==5)
g2<- gps %>% filter(PlayerID==6|PlayerID==7|PlayerID==8|PlayerID==9|PlayerID==10)
g3<- gps %>% filter(PlayerID==11|PlayerID==12|PlayerID==13|PlayerID==14)
g4<- gps %>% filter(PlayerID==15|PlayerID==16|PlayerID==17)

wgps<- rbind(g1,g2,g3,g4)

str(wgps)
wgps %>% print(width=Inf)

wgps %>% 
  distinct(GameID, keep_all=F) %>%
  group_by(PlayerID)

wgps$PlayerID<- as.factor(wgps$PlayerID)

gameday1<- wgps %>% 
  group_by(GameID, PlayerID) %>%
  summarise(avg.speed = mean(Speed),
            avg.accel.load = mean(AccelLoad),
            avg.accelX = mean(AccelX),
            avg.accelY = mean(AccelY),
            avg.accelZ = mean(AccelZ))

gameday1 %>% print(width=Inf)

#gameday<- gameday %>% filter(PlayerID==2|PlayerID==3|PlayerID==4|PlayerID==6|
#                     PlayerID==7|PlayerID==8|PlayerID==9|PlayerID==10|
#                     PlayerID==11|PlayerID==12|PlayerID==13|PlayerID==17) 
gameday %>% count(PlayerID)
gameday %>% print(width=Inf)
gameday1 %>% count(PlayerID) %>% print(n=12)

#--------------------------------------------------

wgps %>% print(width=Inf)
gameday1 %>% print(width=Inf)
gameday %>% print(width=Inf)
gps %>% print(width=Inf)
gps$PlayerID<- as.factor(gps$PlayerID)

#Played Spain 3 Times - Dubai, Langford, World Cup 
#Langford
gameday %>% filter(Date=='2018-05-12') %>% print(width=Inf)

gameday %>% filter(Date=='2018-05-12') %>%
  ggplot(aes(PlayerID, MonitoringScore, fill=SleepHours)) +
  geom_col() +
  theme_bw()

landgps<- gps %>% filter(GameID==24) %>%
  group_by(PlayerID) %>%
  summarise(avg.speed = mean(Speed),
            avg.accel.load = mean(AccelLoad)) 
landgps

#Dubai
gameday %>% filter(Date=='2017-11-30') %>% print(width=Inf)

gameday %>% filter(Date=='2017-11-30') %>%
  ggplot(aes(PlayerID, MonitoringScore, fill=SleepHours)) +
  geom_col() +
  theme_bw()

dubaigps<- gps %>% filter(GameID==1) %>%
  group_by(PlayerID) %>%
  summarise(avg.speed = mean(Speed),
            avg.accel.load = mean(AccelLoad))
dubaigps

#World Cup 
gameday %>% filter(Date=='2018-07-20') %>% print(width=Inf)

gameday %>% filter(Date=='2018-07-20') %>%
  ggplot(aes(PlayerID, MonitoringScore, fill=SleepHours)) +
  geom_col() +
  theme_bw()

wcgps<- gps %>% filter(GameID==37) %>%
  group_by(PlayerID) %>%
  summarise(avg.speed = mean(Speed),
            avg.accel.load = mean(AccelLoad))
wcgps

#--------------------------------------------------

rpe<- read_csv('data/rpe.csv')
str(rpe)

rpe$year<- year(rpe$Date)
rpe$month<- month(rpe$Date)
rpe$mday<- mday(rpe$Date)

rpe<- mtrainyes
apply(rpe, 2, pm)

#Subsets
#Dubai
wdubai<- rpe %>% filter(year==2017)
wdubai<- wdubai %>% filter(month==11&mday==30|month==12&mday==1)
str(wdubai)
wdubai %>% count(Date)

#Sydney
wsydney<- rpe %>% filter(year==2018, month==1)
wsydney<- wsydney %>% filter(mday==26|mday==27|mday==28)
wsydney %>% count(Date)

#Commonwealth
common<- rpe %>% filter(year==2018, month==4, mday==13|mday==14|mday==15)
common %>% count(Date)

#Kitakyushu
kita<- rpe %>% filter(year==2018, month==4, mday==21|mday==22)
kita %>% count(Date)

#Langford 
langford<- rpe %>% filter(year==2018, month==5, mday==12|mday==13)
langford %>% count(Date)

#Paris
paris<- rpe %>% filter(year==2018, month==6, mday==8|mday==9|mday==10)
paris %>% count(Date)

#World Cup 
world<- rpe %>% filter(year==2018, month==7, mday==20|mday==21)
world %>% count(Date)

gameday2<- rbind(wdubai, wsydney, common, kita, langford, paris, world)
gameday2 %>% count(Date)
gameday2<- gameday2 %>% dplyr::select(-year, -month, -mday)

gameday2 %>% count(PlayerID)
gameday %>% count(PlayerID)

gameday2 %>% print(width=Inf)
gameday %>% print(width=Inf)
str(gameday2)
str(gameday)

gameday2<- gameday2 %>% filter(SessionType=='Game')
distinct(gameday2, Date)





