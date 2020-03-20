#--------------------------------------------------

#GPS Dataset
#More for the players

#--------------------------------------------------

#Libraries

#General Purposes
library(tidyverse)
library(psych)

#Missing Values
library(mice)
library(VIM)

#--------------------------------------------------

#Variables
#--------------------------------------------------
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
#--------------------------------------------------

#Data 
gps<- read_csv('data/gps.csv')
str(gps)

gps %>% print(width=Inf)

#Counts of Observations at specific times in the game 
gps %>% filter(GameClock==420) %>% count(PlayerID)
gps %>% filter(GameClock==240) %>% count(PlayerID)

#--------------------------------------------------

#Visualizations

#Histogram of FrameID
ggplot(gps, aes(FrameID)) +
  geom_histogram(fill='cyan') +
  theme_bw()

#Histogram of GameClock
ggplot(gps, aes(GameClock)) +
  geom_histogram(fill='blue') +
  theme_bw()

#Histogram of Time
ggplot(gps, aes(Time)) +
  geom_histogram(fill='red') +
  theme_bw()

str(gps)

#Covariation
pairs.panels(gps[1:1000, 7:12])

#--------------------------------------------------

#Game Players Speed Function
GamePlayers<- function(x){
  gps %>% filter(GameID==x) %>%
    ggplot(aes(Speed, GameClock)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~PlayerID, nrow = 3) 
}

#Game Players AccelImpulse Function
gpaccellimpulse<- function(x){
  gps %>% filter(GameID==x) %>%
    ggplot(aes(AccelImpulse, GameClock)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~PlayerID, nrow = 3) 
}

#Game Players AccelLoad Function
gpaccellload<- function(x){
  gps %>% filter(GameID==x) %>%
    ggplot(aes(AccelLoad, GameClock)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~PlayerID, nrow = 3) 
}

#Looking At Game 9 vs Russia
GamePlayers(x=9)
gpaccellimpulse(x=9)
gpaccellload(x=9)

#Dubai Tournament Games (1-6)
GamePlayers(x=1)
GamePlayers(x=2)
GamePlayers(x=3)
GamePlayers(x=4)
GamePlayers(x=5)
GamePlayers(x=6)

#Sydney Tournament Games (7-12)
GamePlayers(x=7)
GamePlayers(x=8)
GamePlayers(x=9)
GamePlayers(x=10)
GamePlayers(x=11)
GamePlayers(x=12)

#Commonwealth Tournament Games (13-17)
GamePlayers(x=13)
GamePlayers(x=14)
GamePlayers(x=15)
GamePlayers(x=16)
GamePlayers(x=17)

#Kitakyushu Tournament Games (18-22)
GamePlayers(x=18)
GamePlayers(x=19)
GamePlayers(x=20)
GamePlayers(x=21)
GamePlayers(x=22)

#Langford Tournament Games (23-28)
GamePlayers(x=23)
GamePlayers(x=24)
GamePlayers(x=25)
GamePlayers(x=26)
GamePlayers(x=27)
GamePlayers(x=28)

#Paris Tournament Games (29-34)
GamePlayers(x=29)
GamePlayers(x=30)
GamePlayers(x=31)
GamePlayers(x=32)
GamePlayers(x=33)
GamePlayers(x=34)

#World Cup Games (35-38)
GamePlayers(x=35)
GamePlayers(x=36)
GamePlayers(x=37)
GamePlayers(x=38)

#--------------------------------------------------

gps %>% print(width=Inf)

#Player Positions in Game 7 during the first few seconds of the game 
#in the first half 
gps %>% filter(GameClock==540) %>%
  select(GameClock, FrameID)

gps %>% filter(GameClock==1, GameID==7, PlayerID==3) %>%
  select(GameClock, FrameID, PlayerID, Speed, Longitude, Latitude) %>%
  print(n=Inf)

gps %>% filter(GameID==7,  FrameID==1, PlayerID==3) %>%
  select(PlayerID, GameClock, FrameID, Speed, Longitude, Latitude) %>%
  print(n=Inf)

gps %>% filter(GameID==7, PlayerID==2) %>%
  ggplot(aes(GameClock, FrameID)) +
  geom_point()

#Time by Mean Accel
gps %>% filter(GameID==7) %>%
  ggplot(aes(FrameID, AccelLoad)) +
  geom_point()

gps %>% filter(GameID==8, PlayerID==3) %>% print(width=Inf)

str(gps)

gps %>% filter(GameClock==0, GameID==7, PlayerID!= c(3, 4, 5, 16)) %>%
  ggplot(aes(Latitude, Longitude, color = PlayerID, label = PlayerID)) +
  geom_text(check_overlap = T)



gps %>% filter(FrameID==1, GameID==7) %>%
  ggplot(aes(Latitude, Longitude, color = PlayerID, label = PlayerID)) +
  geom_text(check_overlap = T)

gps %>% filter(FrameID==1, GameID==8) %>%
  ggplot(aes(Latitude, Longitude, color = PlayerID, label = PlayerID)) +
  geom_text(check_overlap = T)



gps %>% filter(FrameID==1, GameID==7) %>%
  ggplot(aes(Latitude, Longitude, color = PlayerID, label = PlayerID)) +
  geom_text(check_overlap = T)

gps %>% filter(FrameID==1200, GameID==7) %>%
  ggplot(aes(Latitude, Longitude, color = PlayerID, label = PlayerID)) +
  geom_text(check_overlap = T)

gps %>% filter(FrameID==1, GameID==7, GameClock==0) %>%
  ggplot(aes(round(Latitude, 4), round(Longitude, 4), label=PlayerID)) +
  geom_text(check_overlap = T)

gps %>% filter(FrameID==1, GameID==7, GameClock==420) %>%
  ggplot(aes(Latitude, Longitude, label=PlayerID)) +
  geom_text(check_overlap = T)


#--------------------------------------------------

str(gps)

gps %>% 
  filter(GameID==9) %>%
  group_by(PlayerID)%>%
  summarise(Avg.Speed = mean(Speed),
            Avg.Accel.Impulse = mean(AccelImpulse),
            Avg.Accel.Load = mean(AccelLoad),
            Avg.AccelX = mean(AccelX),
            Avg.AccelY = mean(AccelY),
            Avg.AccelZ = mean(AccelZ))









