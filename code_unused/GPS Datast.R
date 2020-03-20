#--------------------------------------------------

#GPS Dataset
#More for the players

#--------------------------------------------------

#Libraries

#General Purposes
library(tidyverse)

#Missing Values
library(mice)
library(VIM)

#--------------------------------------------------

#Variables
#----------
#GameID 
#Half - the half of the game 
#PlayerID 
#FrameID - ordered unit of time within a half, ten frames per second 
###Example: FrameID = 100 implies we are 10 seconds into the half 
#Time - the current time 
#GameClock - the time on the game clock 
###First half goes till 7:00, Second goes till 14:00 
#Speed - Movement speed of the player, in meters per second 
#AccelImpulse - The absolute value of change in speed divided by change in time 
#AccelLoad - Load detected by the accelerometer, in arbitrary units 
#AccelX - Acceleration in anterioposterior axis direction 
###Forward/Backwards 
#AccelY - Acceleration in medial axis direction 
#AccelZ - Acceleration in vertical direction 
#AccelX,Y,Z measures in meters per second squared 
#----------

#Data 
fullgps<- read_csv('data/gps.csv')
str(fullgps)

fullgps %>% count(GameID)

fullgps$Half<- as.factor(fullgps$Half)

#Normalizing Variables
normalize<- function(x) {
  (x-min(x))/(max(x)-min(x))
}

fullgps$Speed<- normalize(fullgps$Speed)
fullgps$AccelImpulse<- normalize(fullgps$AccelImpulse)
fullgps$AccelLoad<- normalize(fullgps$AccelLoad)
fullgps$AccelX<- normalize(fullgps$AccelX)
fullgps$AccelY<- normalize(fullgps$AccelY)
fullgps$AccelZ<- normalize(fullgps$AccelZ)

fullgps %>% print(width=Inf)

#--------------------------------------------------

#Missing Value Function
pm<- function(x){sum(is.na(x)/length(x)*100)}

fullgps %>% count(PlayerID) %>% print(n=Inf)

str(fullgps)

#Game 1
game1<- fullgps %>%
  filter(GameID==1)
apply(game1, 2, pm)

#Game 38
game38<- fullgps %>%
  filter(GameID==38)
apply(game38, 2, pm)

#Game 37
game37<- fullgps %>%
  filter(GameID==37)
apply(game37, 2, pm)

#Game 36
game36<- fullgps %>% filter(GameID==36)
apply(game36, 2, pm)

#Game 7
game7<- fullgps %>% filter(GameID==7)
apply(game7, 2, pm)

#Game 29
game29<- fullgps %>% filter(GameID==29)
apply(game29, 2, pm)

#--------------------------------------------------

fullgps %>% filter(GameID==1, Half==1) %>%
  group_by(PlayerID) %>%
  arrange(desc(Speed)) %>%
  print(width=Inf)

fullgps %>% filter(GameID==1, Half==1, Speed > 0) %>%
  group_by(PlayerID) %>%
  summarise(med.speed = median(Speed),
            med.accel.imp = median(AccelImpulse),
            med.accel.load = median(AccelLoad),
            med.accel.x = median(AccelX),
            med.accel.y = median(AccelY),
            med.accel.z = median(AccelZ)) %>%
  arrange(desc(med.speed))

fullgps %>% filter(GameID==1, Half==2, Speed > 0) %>%
  group_by(PlayerID) %>%
  summarise(med.speed = median(Speed),
            med.accel.imp = median(AccelImpulse),
            med.accel.load = median(AccelLoad),
            med.accel.x = median(AccelX),
            med.accel.y = median(AccelY),
            med.accel.z = median(AccelZ)) %>%
  arrange(desc(med.speed))

#--------------------------------------------------

fullgps %>% print(width=Inf)
fullgps$PlayerID<- as.factor(fullgps$PlayerID)
fullgps$Half<- as.factor(fullgps$Half)

wcgps<- fullgps %>% filter(GameID==38|GameID==35)
wcgps %>% print(width=Inf)

grf<- randomForest(Speed ~ AccelImpulse+AccelX+AccelY+AccelZ+Half+PlayerID,
                   data = wcgps)









