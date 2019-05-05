#### This is the code containing all statistical models for grooming, locomotion and stopping behavior seen in 
#### fruit flies in the presence of spiders or mantids
#### You must run each chunk of code prior to running the model code. Best to follow along line by line
#### Code for coefficients plots shown in S2 File also provided following code for model. 

#### Please find the CSV titled Spider_Mantid.csv
dataset <- read.csv("../../Datasets/Spider_mantid.csv", header = T)

## Some packages you will need
head(dataset)
require(lme4)
require(MuMIn)
require(MCMCglmm) 
require(coefplot2)
require(sciplot)


## Preprocessing of dataset as described in other scripts (please see Spider_Ethogram.R for more details)
dataset$sec.timestamp <- (dataset$time.stamp/2)/1000
dataset$state.event <- as.character(dataset$state.event)
dataset$file.num <- factor(dataset$file.num)
dataset$pred.state <- as.character(dataset$pred.state)
states <- dataset$state.event 
states <- as.character(states) 
timestamp <- dataset$sec.timestamp
pred.state <- dataset$pred.state
code <- dataset$code


## Getting duration spent in each behavioral state 
duration <- rep(NA, length(states))
for (i in 1:length(timestamp)) { duration[i] <- (timestamp[i+1] - timestamp[i])}
dataset$duration <- duration

## Removing negative durations at EOF
for (i in 1:length(dataset$state.event)) { 
  if (dataset$state.event[i] == "end_trial") {dataset$duration[i] <- 0.00}}

## Removing inching from the state and turning it to walking. Please see Spider_Ethogram.R for more details.
for (i in 1: length(dataset$state)) {
  if (dataset$state[i] == "inch") {dataset$state[i] <- "walk"}}

## If not done: gives a negative duration at EOF. Please see Spider_Ethogram.R for more details.
for (i in 1:length(dataset$state.event)) { 
  if (dataset$state.event[i] == "end_trial") {dataset$pred.state[i] <- "DONE"}}

## This makes sure that the time we spent putting in the spider is taken out of the analysis. Please see Spider_Ethogram.R for more details.
for (i in 1:length(dataset$code)) { 
  if (dataset$code[i] == "1") {dataset$pred.state[i] <- "in"}}

dataset$state <- as.character(dataset$state)
for (i in 1:length(dataset$code)) {
  if (dataset$code[i] == "EOF") {dataset$state[i] <- "end_trial"}}


## Combine walking and running into "locomotion". Model locomotion and grooming in the presence and absence of a predator

for( i in 1: length(dataset$state)) {
  if (dataset$state [i] == "run") {dataset$state[i] <- "walk"}}
dataset$state <- factor(dataset$state)


## For each indvidual fly, get total time spent grooming with and without predator 

## First reshape the dataset to work for our needs. I've written a cool function that takes two arguments, h == the behavior you are interested in
## and g is the predator you are interested in. Don't be fooled by the word grooming in the function. It works across behaviors.

# source(../../Scripts/"Time_perState.R")


## Modeling the grooming behavior in the presence and absence of spider predator

spider_groom <- model_dataset("groom", "spider")

str(spider_groom)
spider_groom$file.num <- factor(spider_groom$file.num)
spider_groom$rec.time <- factor(as.character(spider_groom$rec.time))
spider_groom$Humidity <- as.numeric(as.character(spider_groom$Humidity))
str(spider_groom)


#### MCMCglmm package for statistical modeling and anysis #####

## Preprocessing
spider_groom$file.num <- factor(spider_groom$file.num)
spider_groom$rec.time <- factor(as.character(spider_groom$rec.time))
spider_groom$Humidity <- as.numeric(as.character(spider_groom$Humidity))
str(spider_groom)

## Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
spider_groom$rec.time <- as.numeric(as.character(spider_groom$rec.time))
spider_groom$start.time <- cut(spider_groom$rec.time, breaks = 2, labels=c("9.0","10.30"))

## Preprocessing to convert data into mins
spider_groom$time <- spider_groom$time/60
## Scaling time by the minimum
spider_groom$time.min <- spider_groom$time - (min(spider_groom$time))
## Centering and scaling the predictor variable "time"
spider_groom$time.cent <- scale(spider_groom$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
spider_groom$Age.min <- spider_groom$Age - (min(spider_groom$Age))
spider_groom$Age.cent <- scale(spider_groom$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
spider_groom$Temp.min <- spider_groom$Temp - (min(spider_groom$Temp))
spider_groom$Temp.cent <- scale(spider_groom$Temp, scale = F)


## Prior for MCMC modeling 
prior1.3 <- list(
  R = list(V = 1, n = 0.002),
  G = list(
    G1 = list(V = diag(3), n = rep(0.002, 1)), ## change this to 1 to 3 on this line if needed
    G2 = list(V = 1, n = 0.002)))


## Model ##
model1.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_groom,
                            nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)
summary(model1.MCMCglmm)

## Visualizing the coefficients of the model
coefplot2(model1.MCMCglmm)


## Modeling the locomotion behavior in the presence and absence of spider predator
spider_walk <- model_dataset("walk", "spider")
str(spider_walk)

## Preprocessing like before
spider_walk$file.num <- factor(spider_walk$file.num)
## Scaling time by the minimum
spider_walk$rec.time <- factor(as.character(spider_walk$rec.time))
## Centering and scaling the predictor variable "time"
spider_walk$Humidity <- as.numeric(as.character(spider_walk$Humidity))
str(spider_walk)

### Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
spider_walk$rec.time <- as.numeric(as.character(spider_walk$rec.time))
spider_walk$start.time <- cut(spider_walk$rec.time, breaks = 2, labels=c("9.0","10.30"))

## Preprocessing to convert data into mins
spider_walk$time <- spider_walk$time/60
## Centering and scaling the predictor variable "Time"
spider_walk$time.min <- spider_walk$time - (min(spider_walk$time))
spider_walk$time.cent <- scale(spider_walk$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
spider_walk$Age.min <- spider_walk$Age - (min(spider_walk$Age))
spider_walk$Age.cent <- scale(spider_walk$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
spider_walk$Temp.min <- spider_walk$Temp - (min(spider_walk$Temp))
spider_walk$Temp.cent <- scale(spider_walk$Temp, scale = F)


## Model ##
model2.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_walk,
                            nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)
summary(model2.MCMCglmm)

## Visualizing the coefficients of the model
coefplot2(model2.MCMCglmm)



### Modeling fly grooming behaviors in the presence and absence of a juvenile mantid

## Preprocessing like before
mantid_groom <- model_dataset("groom", "mantid")
mantid_groom$file.num <- factor(mantid_groom$file.num)
mantid_groom$rec.time <- factor(as.character(mantid_groom$rec.time))
mantid_groom$Humidity <- as.numeric(as.character(mantid_groom$Humidity))

str(mantid_groom)

## Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
mantid_groom$rec.time <- as.numeric(as.character(mantid_groom$rec.time))
mantid_groom$start.time <- cut(mantid_groom$rec.time, breaks = 2, labels=c("9.0","10.30"))

## Centering and scaling the predictor variable "Time"
mantid_groom$time <- mantid_groom$time/60
mantid_groom$time.min <- mantid_groom$time - (min(mantid_groom$time))
mantid_groom$time.cent <- scale(mantid_groom$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
mantid_groom$Age.min <- mantid_groom$Age - (min(mantid_groom$Age))
mantid_groom$Age.cent <- scale(mantid_groom$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
mantid_groom$Temp.min <- mantid_groom$Temp - (min(mantid_groom$Temp))
mantid_groom$Temp.cent <- scale(mantid_groom$Temp, scale = F)

## Model ##
model3.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_groom,
                            nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)
summary(model3.MCMCglmm)
coefplot2(model3.MCMCglmm)


### Modeling fly walking behaviors in the presence and absence of a juvenile mantid

mantid_walk <- model_dataset("walk", "mantid")

## Preprocessing
mantid_walk$file.num <- factor(mantid_walk$file.num)
mantid_walk$rec.time <- factor(as.character(mantid_walk$rec.time))
mantid_walk$Humidity <- as.numeric(as.character(mantid_walk$Humidity))

## Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
mantid_walk$rec.time <- as.numeric(as.character(mantid_walk$rec.time))
mantid_walk$start.time <- cut(mantid_walk$rec.time, breaks = 2, labels=c("9.0","10.30"))
str(mantid_walk)

## Centering and scaling the predictor variable "Time"
mantid_walk$time <- mantid_walk$time/60
mantid_walk$time.min <- mantid_walk$time - (min(mantid_walk$time))
mantid_walk$time.cent <- scale(mantid_walk$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
mantid_walk$Age.min <- mantid_walk$Age - (min(mantid_walk$Age))
mantid_walk$Age.cent <- scale(mantid_walk$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
mantid_walk$Temp.min <- mantid_walk$Temp - (min(mantid_walk$Temp))
mantid_walk$Temp.cent <- scale(mantid_walk$Temp, scale = F)

## Model ##
model4.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_walk,
                            nitt=500000, thin=20, burnin=20000, verbose=FALSE, prior = prior1.3)
summary(model4.MCMCglmm)



### Modeling fly stopping behaviors in the presence and absence of a spider

## Spider
spider_stop <- model_dataset("stop", "spider")
spider_stop$file.num <- factor(spider_stop$file.num)
spider_stop$rec.time <- factor(as.character(spider_stop$rec.time))
spider_stop$Humidity <- as.numeric(as.character(spider_stop$Humidity))

## Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
spider_stop$rec.time <- as.numeric(as.character(spider_stop$rec.time))
spider_stop$start.time <- cut(spider_stop$rec.time, breaks = 2, labels=c("9.0","10.30"))

## Centering and scaling the predictor variable "Time"
spider_stop$time <- spider_stop$time/60
spider_stop$time.min <- spider_stop$time - (min(spider_stop$time))
spider_stop$time.cent <- scale(spider_stop$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
spider_stop$Age.min <- spider_stop$Age - (min(spider_stop$Age))
spider_stop$Age.cent <- scale(spider_stop$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
spider_stop$Temp.min <- spider_stop$Temp - (min(spider_stop$Temp))
spider_stop$Temp.cent <- scale(spider_stop$Temp, scale = F)


model5.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_stop,
                            nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)
summary(model5.MCMCglmm)

# Wanted to confirm results with lmer. I did some checking. Because there is no variance in the observations before adding the predator, the interaction terms are important. Indeed, probably we should only model the subset of the data after addition of the predator. However the basic results are the same



### Modeling fly stopping behaviors in the presence and absence of a juvenile mantid

mantid_stop <- model_dataset("stop", "mantid")
mantid_stop$file.num <- factor(mantid_stop$file.num)
mantid_stop$rec.time <- factor(as.character(mantid_stop$rec.time))
mantid_stop$Humidity <- as.numeric(as.character(mantid_stop$Humidity))

## Breaking up rec.time (recording time) into 2 levels and assigning to a new descriptive variable called start.time.
mantid_stop$rec.time <- as.numeric(as.character(mantid_stop$rec.time))
mantid_stop$start.time <- cut(mantid_stop$rec.time, breaks = 2, labels=c("9.0","10.30"))

## Centering and scaling the predictor variable "Time"
mantid_stop$time <- mantid_stop$time/60
mantid_stop$time.min <- mantid_stop$time - (min(mantid_stop$time))
mantid_stop$time.cent <- scale(mantid_stop$time, scale = FALSE)

## Centering and scaling the predictor variable "Age of fly"
mantid_stop$Age.min <- mantid_stop$Age - (min(mantid_stop$Age))
mantid_stop$Age.cent <- scale(mantid_stop$Age, scale = FALSE)

## Centering and scaling the predictor variable "Temperature at which assay was conducted"
mantid_stop$Temp.min <- mantid_stop$Temp - (min(mantid_stop$Temp))
mantid_stop$Temp.cent <- scale(mantid_stop$Temp, scale = F)


model6.MCMCglmm <- MCMCglmm(fixed = total.dur ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                            random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_stop,
                            nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)
summary(model6.MCMCglmm)






#####################################################################

### Here is the code to model the change in frequency of behavioral events in the presence of a predator 
### The CSV you need is called Spider_Mantid.csv

dataset <- read.csv("../CSV/Spider_Mantid.csv", header = T)


## Some package you will need (in case you didn't already get them)
head(dataset)
require(sciplot)
require(MCMCglmm)
require(lme4)
require(MuMIn)

## Preprocessing. Refer to Spider_Ethogram.R for more details
dataset$sec.timestamp <- (dataset$time.stamp/2)/1000 
dataset$state.event <- as.character(dataset$state.event)
dataset$file.num <- factor(dataset$file.num)
dataset$pred.state <- as.character(dataset$pred.state)
states <- dataset$state.event 
states <- as.character(states)
timestamp <- dataset$sec.timestamp
pred.state <- dataset$pred.state
code <- dataset$code

## Getting duration spent in each behavioral state 
duration <- rep(NA, length(states))
for (i in 1:length(timestamp)) { duration[i] <- (timestamp[i+1] - timestamp[i])}
dataset$duration <- duration

## Removing negative durations at EOF
for (i in 1:length(dataset$state.event)) { 
  if (dataset$state.event[i] == "end_trial") {dataset$duration[i] <- 0.00}
}

## Removing inching from the state and turning it to walking. Refer to Spider_Ethogram.R for more details
for (i in 1: length(dataset$state)) {
  if (dataset$state[i] == "inch") {dataset$state[i] <- "walk"}
}

# If not done: gives a negative duration at EOF. Refer to Spider_Ethogram.R for more details
for (i in 1:length(dataset$state.event)) { 
  if (dataset$state.event[i] == "end_trial") {dataset$pred.state[i] <- "DONE"}
}


## Chainging all stopping states to events called pausing. Refer to Mantid_Ethogram.R for more details
dataset$event <- as.character(dataset$event)
dataset$state <- as.character(dataset$state) 

for (i in 1:length(dataset$state)) {
  if (dataset$predator[i] == "mantid") {
    if (dataset$state[i] == "stop" & (dataset$sec.timestamp[i+1] - dataset$sec.timestamp[i]) < 1
        & (dataset$event[i] == "0")) {dataset$event[i] <- "pause"}}}


## Making state corresponding to pause, the previous state it was in.Refer to Mantid_Ethogram.R for more details
for (i in 1:length(dataset$state)){
  if (dataset$state[i] == "stop" & dataset$event[i] == "pause" & dataset$predator[i] == "mantid") 
    {dataset$state[i] <- dataset$state [i-1]}}



# This makes sure that the time we spent putting in the spider is taken out of the analysis. Refer to Mantid_Ethogram.R for more details
for (i in 1:length(dataset$code)) { 
  if (dataset$code[i] == "1") {dataset$pred.state[i] <- "in"}}

dataset$state <- as.character(dataset$state)

## Making some text changes for graphs
for (i in 1:length(dataset$code)) {
  if (dataset$code[i] == "EOF") {dataset$state[i] <- "end_trial"}
}
dataset$state <- factor(dataset$state)


### This is a function that takes the original data and makes it managable for this analysis.
### The name of the function is model_dataset_event("name_of_event", "predator")

## The code is in a file called Freq_perEvent.R in the foler Scripts
source("/Users/abhijnaparigi/Dropbox/Abhijna/Ethogram/Code/Modelling_Stats/Freq_perEvent.R")

## Linear mixed effects models ##

## Spider data

#ablift - centering and scaling the variable
spider_ablift <- model_dataset_event("ab_lift", "spider")
spider_ablift$file.num <- factor(spider_ablift$file.num)
spider_ablift$rec.time <- as.numeric(as.character(spider_ablift$rec.time))
spider_ablift$start.time <- cut(spider_ablift$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_ablift$Humidity <- as.numeric(as.character(spider_ablift$Humidity))
spider_ablift$time <- spider_ablift$time/60
spider_ablift$time.min <- spider_ablift$time - (min(spider_ablift$time))
spider_ablift$time.cent <- scale(spider_ablift$time, scale=FALSE)
spider_ablift$Age.min <- spider_ablift$Age - (min(spider_ablift$Age))
spider_ablift$Age.cent <- scale(spider_ablift$Age, scale=FALSE)
spider_ablift$Temp.min <- spider_ablift$Temp - (min(spider_ablift$Temp))
spider_ablift$Temp.cent <- scale(spider_ablift$Temp, scale = F)

## Prior
prior1.3 <- list(
  R = list(V = 1, n = 0.002),
  G = list(
    G1 = list(V = diag(3), n = rep(0.002, 1)),
    G2 = list(V = 1, n = 0.002)
  ))

## Model ## 

modelE1.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, 
                             data = spider_ablift, family = "poisson",
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3)

summary(modelE1.MCMCglmm)


# Flying -- preprocessing, centering and scaling the variables
spider_fly <- model_dataset_event("fly", "spider")
spider_fly$file.num <- factor(spider_fly$file.num)
spider_fly$rec.time <- as.numeric(as.character(spider_fly$rec.time))
spider_fly$start.time <- cut(spider_fly$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_fly$Humidity <- as.numeric(as.character(spider_fly$Humidity))
spider_fly$time <- spider_fly$time/60
spider_fly$time.min <- spider_fly$time - (min(spider_fly$time))
spider_fly$time.cent <- scale(spider_fly$time, scale=FALSE)
spider_fly$Age.min <- spider_fly$Age - (min(spider_fly$Age))
spider_fly$Age.cent <- scale(spider_fly$Age, scale=FALSE)
spider_fly$Temp.min <- spider_fly$Temp - (min(spider_fly$Temp))
spider_fly$Temp.cent <- scale(spider_fly$Temp, scale = F)

## Model ##
modelE2.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_fly,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")

summary(modelE2.MCMCglmm)



## Pausing -- preprocessing, centering and scaling the variables
spider_pause <- model_dataset_event("pause", "spider")
spider_pause$file.num <- factor(spider_pause$file.num)
spider_pause$rec.time <- as.numeric(as.character(spider_pause$rec.time))
spider_pause$start.time <- cut(spider_pause$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_pause$Humidity <- as.numeric(as.character(spider_pause$Humidity))
spider_pause$time <- spider_pause$time/60
spider_pause$time.min <- spider_pause$time - (min(spider_pause$time))
spider_pause$time.cent <- scale(spider_pause$time, scale=FALSE)
spider_pause$Age.min <- spider_pause$Age - (min(spider_pause$Age))
spider_pause$Age.cent <- scale(spider_pause$Age, scale=FALSE)
spider_pause$Temp.min <- spider_pause$Temp - (min(spider_pause$Temp))
spider_pause$Temp.cent <- scale(spider_pause$Temp, scale = F)

## Model ##
modelE3.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_pause,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE3.MCMCglmm)


## wd -- preprocessing, centering and scaling the variables
spider_wd <- model_dataset_event("wing_disp", "spider")
spider_wd$file.num <- factor(spider_wd$file.num)
spider_wd$rec.time <- as.numeric(as.character(spider_wd$rec.time))
spider_wd$start.time <- cut(spider_wd$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_wd$Humidity <- as.numeric(as.character(spider_wd$Humidity))
spider_wd$time <- spider_wd$time/60
spider_wd$time.min <- spider_wd$time - (min(spider_wd$time))
spider_wd$time.cent <- scale(spider_wd$time, scale=FALSE)
spider_wd$Age.min <- spider_wd$Age - (min(spider_wd$Age))
spider_wd$Age.cent <- scale(spider_wd$Age, scale=FALSE)
spider_wd$Temp.min <- spider_wd$Temp - (min(spider_wd$Temp))
spider_wd$Temp.cent <- scale(spider_wd$Temp, scale = F)

## Model ##
modelE4.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_wd,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE4.MCMCglmm)


## turn -- preprocessing, centering and scaling the variables
spider_turn <- model_dataset_event("turn", "spider")
spider_turn$file.num <- factor(spider_turn$file.num)
spider_turn$rec.time <- as.numeric(as.character(spider_turn$rec.time))
spider_turn$start.time <- cut(spider_turn$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_turn$Humidity <- as.numeric(as.character(spider_turn$Humidity))
spider_turn$time <- spider_turn$time/60
spider_turn$time.min <- spider_turn$time - (min(spider_turn$time))
spider_turn$time.cent <- scale(spider_turn$time, scale=FALSE)
spider_turn$Age.min <- spider_turn$Age - (min(spider_turn$Age))
spider_turn$Age.cent <- scale(spider_turn$Age, scale=FALSE)
spider_turn$Temp.min <- spider_turn$Temp - (min(spider_turn$Temp))
spider_turn$Temp.cent <- scale(spider_turn$Temp, scale = F)


## Model ## 
modelE5.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = spider_turn,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE5.MCMCglmm)


## jump -- preprocessing, centering and scaling the variables
spider_jump <- model_dataset_event("jump", "spider")
spider_jump$file.num <- factor(spider_jump$file.num)
spider_jump$rec.time <- as.numeric(as.character(spider_jump$rec.time))
spider_jump$start.time <- cut(spider_jump$rec.time, breaks = 2, labels=c("9.0","10.30"))
spider_jump$Humidity <- as.numeric(as.character(spider_jump$Humidity))
spider_jump$time <- spider_jump$time/60
spider_jump$time.min <- spider_jump$time - (min(spider_jump$time))
spider_jump$time.cent <- scale(spider_jump$time, scale=FALSE)
spider_jump$Age.min <- spider_jump$Age - (min(spider_jump$Age))
spider_jump$Age.cent <- scale(spider_jump$Age, scale=FALSE)
spider_jump$Temp.min <- spider_jump$Temp - (min(spider_jump$Temp))
spider_jump$Temp.cent <- scale(spider_jump$Temp, scale = F)

## Model ##
modelE6.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num + Date, 
                             data = spider_jump,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, 
                             prior = prior1.3, family = "poisson")
summary(modelE6.MCMCglmm)




### Same deal for manid data ###


## Ablift -- preprocessing, centering and scaling the variables
mantid_ab <- model_dataset_event("ab_lift", "mantid")
mantid_ab$file.num <- factor(mantid_ab$file.num)
mantid_ab$rec.time <- as.numeric(as.character(mantid_ab$rec.time))
mantid_ab$start.time <- cut(mantid_ab$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_ab$Humidity <- as.numeric(as.character(mantid_ab$Humidity))
mantid_ab$time <- mantid_ab$time/60
mantid_ab$time.min <- mantid_ab$time - (min(mantid_ab$time))
mantid_ab$time.cent <- scale(mantid_ab$time, scale=FALSE)
mantid_ab$Age.min <- mantid_ab$Age - (min(mantid_ab$Age))
mantid_ab$Age.cent <- scale(mantid_ab$Age, scale=FALSE)
mantid_ab$Temp.min <- mantid_ab$Temp - (min(mantid_ab$Temp))
mantid_ab$Temp.cent <- scale(mantid_ab$Temp, scale = F)

## Model ##
modelE7.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, 
                             data = mantid_ab, nitt=500000, thin=100, burnin=100000, verbose=FALSE, 
                             prior = prior1.3, family = "poisson")
summary(modelE7.MCMCglmm)

# Flying -- preprocessing, centering and scaling the variables
mantid_fly <- model_dataset_event("fly", "mantid")
mantid_fly$file.num <- factor(mantid_fly$file.num)
mantid_fly$rec.time <- as.numeric(as.character(mantid_fly$rec.time))
mantid_fly$start.time <- cut(mantid_fly$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_fly$Humidity <- as.numeric(as.character(mantid_fly$Humidity))
mantid_fly$time <- mantid_fly$time/60
mantid_fly$time.min <- mantid_fly$time - (min(mantid_fly$time))
mantid_fly$time.cent <- scale(mantid_fly$time, scale=FALSE)
mantid_fly$Age.min <- mantid_fly$Age - (min(mantid_fly$Age))
mantid_fly$Age.cent <- scale(mantid_fly$Age, scale=FALSE)
mantid_fly$Temp.min <- mantid_fly$Temp - (min(mantid_fly$Temp))
mantid_fly$Temp.cent <- scale(mantid_fly$Temp, scale = F)

## Model ##
modelE8.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_fly,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE8.MCMCglmm)


### pausing -- preprocessing, centering and scaling the variables
mantid_pause <- model_dataset_event("pause", "mantid")
mantid_pause$file.num <- factor(mantid_pause$file.num)
mantid_pause$rec.time <- as.numeric(as.character(mantid_pause$rec.time))
mantid_pause$start.time <- cut(mantid_pause$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_pause$Humidity <- as.numeric(as.character(mantid_pause$Humidity))
mantid_pause$time <- mantid_pause$time/60
mantid_pause$time.min <- mantid_pause$time - (min(mantid_pause$time))
mantid_pause$time.cent <- scale(mantid_pause$time, scale=FALSE)
mantid_pause$Age.min <- mantid_pause$Age - (min(mantid_pause$Age))
mantid_pause$Age.cent <- scale(mantid_pause$Age, scale=FALSE)
mantid_pause$Temp.min <- mantid_pause$Temp - (min(mantid_pause$Temp))
mantid_pause$Temp.cent <- scale(mantid_pause$Temp, scale = F)

## Model ##

modelE9.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                             random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_pause,
                             nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE9.MCMCglmm)


## wd -- preprocessing, centering and scaling the variables
mantid_wd <- model_dataset_event("wing_disp", "mantid")
mantid_wd$file.num <- factor(mantid_wd$file.num)
mantid_wd$rec.time <- as.numeric(as.character(mantid_wd$rec.time))
mantid_wd$start.time <- cut(mantid_wd$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_wd$Humidity <- as.numeric(as.character(mantid_wd$Humidity))
mantid_wd$time <- mantid_wd$time/60
mantid_wd$time.min <- mantid_wd$time - (min(mantid_wd$time))
mantid_wd$time.cent <- scale(mantid_wd$time, scale=FALSE)
mantid_wd$Age.min <- mantid_wd$Age - (min(mantid_wd$Age))
mantid_wd$Age.cent <- scale(mantid_wd$Age, scale=FALSE)
mantid_wd$Temp.min <- mantid_wd$Temp - (min(mantid_wd$Temp))
mantid_wd$Temp.cent <- scale(mantid_wd$Temp, scale = F)

## Model ##
modelE10.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                              random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_wd,
                              nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE10.MCMCglmm)


## turn -- preprocessing, centering and scaling the variables
mantid_turn <- model_dataset_event("turn", "mantid")
mantid_turn$file.num <- factor(mantid_turn$file.num)
mantid_turn$rec.time <- as.numeric(as.character(mantid_turn$rec.time))
mantid_turn$start.time <- cut(mantid_turn$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_turn$Humidity <- as.numeric(as.character(mantid_turn$Humidity))
mantid_turn$time <- mantid_turn$time/60
mantid_turn$time.min <- mantid_turn$time - (min(mantid_turn$time))
mantid_turn$time.cent <- scale(mantid_turn$time, scale=FALSE)
mantid_turn$Age.min <- mantid_turn$Age - (min(mantid_turn$Age))
mantid_turn$Age.cent <- scale(mantid_turn$Age, scale=FALSE)
mantid_turn$Temp.min <- mantid_turn$Temp - (min(mantid_turn$Temp))
mantid_turn$Temp.cent <- scale(mantid_turn$Temp, scale = F)


## Model ##

modelE11.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                              random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_turn,
                              nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE11.MCMCglmm)


## jump -- preprocessing, centering and scaling the variables
mantid_jump <- model_dataset_event("jump", "mantid")
mantid_jump$file.num <- factor(mantid_jump$file.num)
mantid_jump$rec.time <- as.numeric(as.character(mantid_jump$rec.time))
mantid_jump$start.time <- cut(mantid_jump$rec.time, breaks = 2, labels=c("9.0","10.30"))
mantid_jump$Humidity <- as.numeric(as.character(mantid_jump$Humidity))
mantid_jump$time <- mantid_jump$time/60
mantid_jump$time.min <- mantid_jump$time - (min(mantid_jump$time))
mantid_jump$time.cent <- scale(mantid_jump$time, scale=FALSE)
mantid_jump$Age.min <- mantid_jump$Age - (min(mantid_jump$Age))
mantid_jump$Age.cent <- scale(mantid_jump$Age, scale=FALSE)
mantid_jump$Temp.min <- mantid_jump$Temp - (min(mantid_jump$Temp))
mantid_jump$Temp.cent <- scale(mantid_jump$Temp, scale = F)

## Model ##

modelE12.MCMCglmm <- MCMCglmm(fixed = Freq ~ time.cent + pred.state + Age.min + Temp.min + start.time + Sex,
                              random =~ us(1 + pred.state + time.cent):file.num  + Date, data = mantid_jump,
                              nitt=500000, thin=100, burnin=100000, verbose=FALSE, prior = prior1.3, family = "poisson")
summary(modelE12.MCMCglmm)

