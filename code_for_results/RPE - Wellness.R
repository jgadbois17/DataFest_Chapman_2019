#--------------------------------------------------

#General Purposes
library(tidyverse)
library(psych)

#Missing Values
library(mice)
library(VIM)

#Random Forest
library(randomForest)
library(caret)

#--------------------------------------------------

wellness<- read_csv('data/wellness.csv')
rpe<- read_csv('data/rpe.csv')

str(wellness)
str(rpe)

#--------------------------------------------------

#Functions

#Missing Value Function
pm<- function(x){sum(is.na(x)/length(x)*100)}

#Normalization Function
normalize<- function(x){
  (x-min(x))/(max(x)-min(x))
}

#--------------------------------------------------

#RPE Dataset

#SessionType - Type of training session
#Duration - duration of session in minutes
#RPE - Rate of perceived exertion (1-10)
#SessionLoad - Duration*RPE
#DailyLoad - Sum of SessionLoad for a given day

apply(rpe, 2, pm)
md.pairs(rpe)

rpe$ObjectiveRating<- ifelse(rpe$ObjectiveRating <= 5, 1, rpe$ObjectiveRating)
rpe$ObjectiveRating<- ifelse(rpe$ObjectiveRating==7|rpe$ObjectiveRating==6,
                             2, rpe$ObjectiveRating)

rpe$ObjectiveRating<- ifelse(rpe$ObjectiveRating==8, 3, rpe$ObjectiveRating)
rpe$ObjectiveRating<- ifelse(rpe$ObjectiveRating==9, 4, rpe$ObjectiveRating)
rpe$ObjectiveRating<- ifelse(rpe$ObjectiveRating==10, 5, rpe$ObjectiveRating)

rpe %>% count(ObjectiveRating)

rpe$ObjectiveRating<- as.factor(rpe$ObjectiveRating)
rpe$RPE<- as.factor(rpe$RPE)
rpe$FocusRating<- as.factor(rpe$FocusRating)
rpe$BestOutOfMyself<- as.factor(rpe$BestOutOfMyself)
rpe$SessionType<- as.factor(rpe$SessionType)

str(rpe)

#RPE dataset with training = yes
trainyes<- rpe %>%
  filter(Training=='Yes') %>%
  select(-Training)

#Covariations
pairs.panels(trainyes)

apply(trainyes, 2, pm)
md.pairs(trainyes)

trainyes<- trainyes %>% 
  select(-AcuteLoad, -ChronicLoad, -AcuteChronicRatio, -BestOutOfMyself)

trainyes %>% print(width=Inf)
str(trainyes)
rpe %>% count(FocusRating)
rpe %>% count(ObjectiveRating)

#--------------------------------------------------

#Missing Values for RPE Dataset

#Imputation - mice
md.pairs(trainyes)
impute<- mice(trainyes[,-8], m=3, seed = 123)
impute

#New Dataset - mice imputation
mtrainyes<- complete(impute, 1)
mtrainyes$ObjectiveRating<- trainyes$ObjectiveRating

str(mtrainyes)
apply(mtrainyes, 2, pm)

#Imputation - KNN
kvar<- c('Date', 'PlayerID', 'SessionType', 'Duration', 'RPE',
         'SessionLoad', 'DailyLoad', 'FocusRating')
ktrainyes<- kNN(trainyes, variable = kvar, k=5)
str(ktrainyes)
ktrainyes<- subset(ktrainyes, select = Date:FocusRating)
apply(ktrainyes, 2, pm)

str(ktrainyes)
apply(ktrainyes, 2, pm)

#Dataset with NA's Omitted
otrainyes<- na.omit(trainyes)

#Comparing Datasets
otrainyes %>% count(FocusRating)
mtrainyes %>% count(FocusRating)
ktrainyes %>% count(FocusRating)

#Use Mice Imputed Dataset
apply(mtrainyes, 2, pm)

#--------------------------------------------------

str(mtrainyes)
mtrainyes$PlayerID<- as.factor(mtrainyes$PlayerID)
mtrainyes$SessionType<- as.factor(mtrainyes$SessionType)

#Normalizing Numeric Variables
mtrainyes$Duration<- normalize(mtrainyes$Duration)
mtrainyes$SessionLoad<- normalize(mtrainyes$SessionLoad)
mtrainyes$DailyLoad<- normalize(mtrainyes$DailyLoad)

#Splitting Data - one with missing values one without
fulldata<- na.omit(mtrainyes)
classdata<- subset(mtrainyes, is.na(mtrainyes$ObjectiveRating)==T)

str(fulldata)
str(classdata)
classdata<- classdata[,-9]

#--------------------------------------------------

#Partitioning Data
set.seed(1)
pd<- sample(2, nrow(fulldata), replace = T, prob = c(0.7, 0.3))

train.rf<- fulldata[pd==1,]
test.rf<- fulldata[pd==2,]

#Random Forest
set.seed(2)
rf<- randomForest(ObjectiveRating ~ .,
                  data = train.rf,
                  ntree = 300,
                  mtry = 8,
                  importance = T,
                  proximity = T)
rf

plot(rf)

#Confusion Matrix & Accuracy - train.rf
pred.rf<- predict(rf, train.rf)
confusionMatrix(pred.rf, train.rf$ObjectiveRating)

#Confusion Matrix & Accuracy - test.rf
pred.rf1<- predict(rf, test.rf)
confusionMatrix(pred.rf1, test.rf$ObjectiveRating)

#Variable Importance 
varImpPlot(rf, main = 'Variable Importance Plot', col = 'blue')
importance(rf)
varUsed(rf)

#Tuning Random Forest
t<- tuneRF(train.rf[,-9], train.rf[,9],
           stepFactor = 0.5,
           plot = T,
           ntreeTry = 300,
           trace = T,
           improve = 0.05)

#Number of Nodes Per Tree
hist(treesize(rf),
     main = "Number of Nodes for the Trees",
     col = 'cyan')

#Partial Dependence Plot
partialPlot(rf, train.rf, FocusRating, '1')
partialPlot(rf, train.rf, FocusRating, '5')

#Extracting Single Trees
getTree(rf, 1, labelVar = T)

#Classifying Missing Observations
pclass<- predict(rf, classdata)
pclass 

classdata$ObjectiveRating<- pclass

#Confusion Matrix & Accuracy for Full Data
pfull<- predict(rf, fulldata)
confusionMatrix(pfull, fulldata$ObjectiveRating)

#Combining Full Data and Class Data
df<- rbind(fulldata, classdata)
str(df)

#Confusion Matrix & Accuracy for Complete Data
pcomplete<- predict(rf, df)
confusionMatrix(pcomplete, df$ObjectiveRating)

#--------------------------------------------------


































