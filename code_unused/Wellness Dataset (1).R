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

wellness %>% print(width=Inf)

#Function To Normalize Specific Player Observations 
normplayer <- function(x){
  z<- wellness %>%
        filter(PlayerID==x) %>%
        select(Fatigue, Soreness, Desire, Irritability, SleepHours, SleepQuality,
               MonitoringScore)
  m<- apply(z, 2, mean)
  s<- apply(z, 2, sd)
  z<- scale(z,m,s)
}
z1<- as.data.frame(normplayer(x=1))
z2<- as.data.frame(normplayer(x=2))
z3<- as.data.frame(normplayer(x=3))
z4<- as.data.frame(normplayer(x=4))
z5<- as.data.frame(normplayer(x=5))
z6<- as.data.frame(normplayer(x=6))
z7<- as.data.frame(normplayer(x=7))
z8<- as.data.frame(normplayer(x=8))
z9<- as.data.frame(normplayer(x=9))
z10<- as.data.frame(normplayer(x=10))
z11<- as.data.frame(normplayer(x=11))
z12<- as.data.frame(normplayer(x=12))
z13<- as.data.frame(normplayer(x=13))
z14<- as.data.frame(normplayer(x=14))
z15<- as.data.frame(normplayer(x=15))
z16<- as.data.frame(normplayer(x=16))
z17<- as.data.frame(normplayer(x=17))

z<- rbind.data.frame(z1, z2, z3, z4, z5, z6, z7, z8, z9,
                     z10, z11, z12, z13, z14, z15, z16, z17)

#New Dataset with Normalized values for each specific player
nwellness<- wellness
str(nwellness)

nwellness$Fatigue<- z$Fatigue
nwellness$Soreness<- z$Soreness
nwellness$Desire<- z$Desire
nwellness$Irritability<- z$Irritability
nwellness$SleepHours<- z$SleepHours
nwellness$SleepQuality<- z$SleepQuality
nwellness$MonitoringScore<- z$MonitoringScore

nwellness %>% print(width=Inf)
nwellness<- nwellness %>%
  select(-BedTime, -WakeTime, -USG, -USGMeasurement)

apply(nwellness,2,pm)

#New Normalized Dataset without variables with missing values
nwellness1<- nwellness %>%
  select(-Menstruation, -Nutrition, -NutritionAdjustment)

nwellness1$Pain<- as.factor(nwellness1$Pain)
nwellness1$Illness<- as.factor(nwellness1$Illness)
nwellness1$PlayerID<- as.factor(nwellness1$PlayerID)

str(nwellness1)
apply(nwellness1, 2, pm)

#--------------------------------------------------

#Subsets
#Dubai
wdubai<- nwellness1 %>% filter(year==2017)
wdubai<- wdubai %>% filter(month==11&mday==30|month==12&mday==1)
str(wdubai)
wdubai %>% count(Date)

#Sydney
wsydney<- nwellness1 %>% filter(year==2018, month==1)
wsydney<- wsydney %>% filter(mday==26|mday==27|mday==28)
wsydney %>% count(Date)

#Commonwealth
common<- nwellness1 %>% filter(year==2018, month==4, mday==13|mday==14|mday==15)
common %>% count(Date)

#Kitakyushu
kita<- nwellness1 %>% filter(year==2018, month==4, mday==21|mday==22)
kita %>% count(Date)

#Langford 
langford<- nwellness1 %>% filter(year==2018, month==5, mday==12|mday==13)
langford %>% count(Date)

#Paris
paris<- nwellness1 %>% filter(year==2018, month==6, mday==8|mday==9|mday==10)
paris %>% count(Date)

#World Cup 
world<- nwellness1 %>% filter(year==2018, month==7, mday==20|mday==21)
world %>% count(Date)


#Game Day Dataset
gameday<- rbind(wdubai, wsydney, common, kita, langford, paris, world)
gameday %>% count(Date)
str(gameday)
gameday %>% print(width=Inf)

gameday<- gameday %>% select(-year, -month, -mday)

#--------------------------------------------------

gameday %>% print(width=Inf)
str(gameday)

#Player Averages
playeraverages<- gameday %>% 
  group_by(PlayerID, Date) %>%
  summarise(avg.fatigue = mean(Fatigue),
            avg.soreness = mean(Soreness),
            avg.desire = mean(Desire),
            avg.irrit = mean(Irritability),
            avg.sleep = mean(SleepHours),
            avg.sleep.quality = mean(SleepQuality),
            avg.monit.score = mean(MonitoringScore),
            avg.readiness = mean(TrainingReadiness))

playeraverages %>% print(width=Inf)
str(playeraverages)
pairs.panels(playeraverages)




























