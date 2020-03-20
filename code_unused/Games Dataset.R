#--------------------------------------------------

#Games Dataset
#More for the players

#--------------------------------------------------

#Libraries

#General Purposes
library(tidyverse)

#--------------------------------------------------

#Variables
#-----
#GameID
#Date
#Tournament - Tournament that hosts game
#TournamentGame - The game number of the tournament
#Team - Canada
#Opponent - The country that Canada Played 
#Outcome - W if Canada won, L is Canada Lost 
#TeamnPoins - Number of points that Canada Scored
#TeamPointsAllowed - Number of points the opposing team scored
#-----

#Data
games<- read_csv('data/games.csv')
str(games)

#Formatting Data
games$GameID<- as.factor(games$GameID)
games$Tournament<- as.factor(games$Tournament)
games$TournamentGame<- as.factor(games$TournamentGame)
games$Team<- as.factor(games$Team)
games$Opponent<- as.factor(games$Opponent)
games$Outcome<- as.factor(games$Outcome)

str(games)

#--------------------------------------------------

#Explore

games %>% print(width=Inf)

games %>% count(Date)
games %>% count(GameID) %>% print(n=Inf)

games %>% count(Tournament)

games %>% count(Opponent)

#--------------------------------------------------

#Dubai Tournament 
#Date Range: 2017-11-30 to 2017-12-01
games %>%
  filter(Tournament=='Dubai') %>%
  print(width=Inf)

#--------------------------------------------------

#Sydney Tournament 
#Date Range: 2018-01-26 to 2018-01-28
games %>%
  filter(Tournament=='Sydney') %>%
  print(width = Inf)

#--------------------------------------------------

#Commonwealth Tournament
#Date Range: 2018-04-13 to 2018-04-15
games %>%
  filter(Tournament=='Commonwealth') %>%
  print(width=Inf)

#--------------------------------------------------

#Kitakyushu Tournament 
#Date Range: 2018-04-21 to 2018-04-22
games %>%
  filter(Tournament=='Kitakyushu') %>%
  print(width=Inf)

#--------------------------------------------------

#Langford Tournament 
#Date Range: 2018-05-12 to 2018-05-13
games %>%
  filter(Tournament=='Langford') %>%
  print(width=Inf)

#--------------------------------------------------

#Paris Tournament 
#Date Range: 2018-06-08 to 2018-06-10
games %>%
  filter(Tournament=='Paris') %>%
  print(width=Inf)

#--------------------------------------------------

#World Cup 
#Date Range: 2018-07-20 to 2018-07-21
games %>%
  filter(Tournament=='World Cup') %>%
  print(width = Inf)














