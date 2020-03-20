# DataFest Chapman 2019

DataFest is an event put on by the American Statistics Association for undergraduate students. 
Students sign up in small teams and are given a complex dataset and 48 hours to produce a two slide presentation. At the end of the time period, all teams present their findings and the ones that stand out the most are recognized. 

My team won the best use of statistical software for our efforts at tackling the given problem. 


## Data

The dataset was from the International Women's Rugby 7's team. It consisted of four csv files all containing different data. 

1. Games
  * Information about all of the games the team played in a season 
2. Wellness 
  * A self reported wellness report each player reported each morning 
3. RPE (Rate of Perceived Effort) 
  * Self reported workloads from each session 
  * A session is a workout or a game 
4. GPS 
  * The players wore gps trackers recording their position at 100 Hz (every 0.01 seconds)

**Note:** The GPS dataset was very large, so it is left out of the data folder 

## Our Approach 

Our approach with this data was to look into the players morning evaluation reports to determine if the players game performance corresponded with how they reported they felt. 


## Procedures 

### Self Evaluations 

We created a random forest to predict their overall rating on how they felt. We looked into the feature importance to see what features were playing the biggest role in how the players felt each morning. 

### Looking into Performance 

* We chose one game to work with and first plotted the first ten seconds of the GPS data to track player movements. 
* We then went and found a video of the beginning of the game to identify which players corresponded to the arbitrary ID numbers given in the dataset. 
* We then found what players scored the goals for that given game to compare with how they reported they felt