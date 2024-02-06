# Date: 01/29/2024 # nolint
# Last updated: 01/29/2024
# Name: Christian Carson
# Description: Assignment 1 - Generalized linear model - REM 661
# Notes:

#libs
#install.packages("conflicted")
library(conflicted)
library(ggplot2)
library(dplyr)

### INPUT ###
rm(list=ls()) # nolint
graphics.off()
getwd()

dir.data <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Assignment 1/Input/" # nolint
dir.out <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Asssignment 1/Output/" # nolint
datafile <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Assignment 1/Input/GN CPUE.csv" # nolint: line_length_linter.

### INITIAL SET-UP ###
setwd(dir.data)

data <- read.csv(file = datafile, header=T)  # nolint

str(data)
head(data)
names(data)
summary(data)
print(data)

#pairwise correlations#
cor(data)


# Setup 10 models that can include varying interaction terms, squared terms or nothing, and be fit using different link functions within the Gaussian family.
#link function takes the process that is driving your dependant variable and translates it into another family (link gaussian - normally distributed -> translates one of several discrete types of data)
#M1 - CPUE ~ Temperature# #nolint
M1 <- glm(CPUE ~ Temperature, data = data, family = gaussian(link = "identity"))
#M2 - CPUE ~ Temperature + WaterLevel #nolint
M2 <- glm(CPUE ~ Temperature + WaterLevel, data = data, family = gaussian(link = "identity"))
#M2 - CPUE ~ Temperature + WaterLevel #nolint
M3 <- glm(CPUE ~ Temperature + WaterLevel + I(Temperature^2), data = data, family = gaussian(link = "identity"))
#M4 - CPUE ~ Temperature + WaterLevel + I(Temperature^2) + I(WaterLevel^2) #nolint
M4 <- glm(CPUE ~ Temperature + WaterLevel + I(Temperature^2) + I(WaterLevel^2), data = data, family = gaussian(link = "identity"))
#M5 - CPUE ~ 



#AIC Comparison table
AIC1 <- AIC(M1)
AIC2 <- AIC(M2)
AIC3 <- AIC(M3)
AIC4 <- AIC(M4)
AIC5 <- AIC(M5)
AIC6 <- AIC(M6)
AIC7 <- AIC(M7)
AIC8 <- AIC(M8)
AIC9 <- AIC(M9)
AIC10 <- AIC(M10)
#compare the model in table
tab <- data.frame(AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, AIC7, AIC8, AIC9, AIC10)
#as.numer

#print
print(tab)
 #RULE OF THUMB: the model with the lowest AIC value is the best model, but if the difference is less than 2, the models are equally good

######Practice code for GLM########
#####fish mortality as a function of temperature, fight duration,
#and air exposure time########
#define number of observations
N <- 250 # nolint
#define independent variables
#normally distributed fight time variable
FightTime <- rlnorm(N, 0, 0.5) #n = n, meanlog = 0, sdlog = 0.5  # nolint
#normally distributed air temperature variable, rep 5 times each
Temp <- rep(18:22, each = N/5) # nolint
#amount of time catching fish, pulling them out of water, and taking pictures
AirExposure <- rlnorm(N, -1, 0.2) # nolint #n = n, meanlog = -1, sdlog = 0.2

#simulate model on logit scale, for every minute the fish fights, the probability of being alive decreases by 0.5
logitAlive <- 1 + -0.5*FightTime + -0.5*Temp + -0.5*AirExposure # nolint # 1 = intercept, -0.5 = coefficient weight, FightTime, Temp, AirExposure, respectively
#probability of being alive
pAlive <- plogis(logitAlive) # nolint, plogis = inverse logit function to get probability of being alive
#simulate binary response variable
Alive <- rbinom(N, 1, pAlive) # nolint, rbinom = simulate binary response variable
#RBINOM(n, size, prob) # nolint, n = number of observations, size = number of trials, prob = probability of success
#put everthing into a dataframe
AliveDF <- data.frame(logitAlive, FightTime, Temp, AirExposure) # nolint #this takes the logit scale, fight time, temperature, and air exposure time and puts them into a dataframe

#give overview of the data
str(AliveDF)#show the structure of the dataframe, which includes the number of observations and the variables # nolint
head(AliveDF)#show the first few rows of the dataframe
summary(AliveDF) # show summary statistics of the dataframe, including the mean, median, min, max, and quartiles # nolint
cor(AliveDF) # show the correlation matrix of the dataframe, which shows the correlation between the variables # nolint

#estimate the model using the logit link function
#remove dependent variable from dataframe to determine independent variables are correlated # nolint
noAlive <- AliveDF[, -1] # nolint
cor(noAlive) # nolint

#fit 1
Fit1 <- glm(Alive ~ FightTime + Temp + AirExposure, data = AliveDF, family = binomial(link = "logit")) # nolint #dependant varible = Alive, independent variables = FightTime, Temp, AirExposure, family = binomial, link function = logit, logit is defines as the natural log of the odds of the probability of being alive
#natural log of the odds of the probability of being alive is the probability of being alive divided by the probability of not being alive
#summary of the model
summary(Fit1) # nolint

#Fit2
Fit2 <- glm(Alive ~ FightTime + Temp + I(Temp^2), data = AliveDF, family = binomial(link = "logit")) # nolint #dependant varible = Alive, independent variables = FightTime, Temp, family = binomial, link function = logit, logit is defines as the natural log of the odds of the probability of being alive
#summary of the model
summary(Fit2) # nolint

#akiake information criterion
AIC1 <- AIC(Fit1) # nolint
AIC2 <- AIC(Fit2) # nolint

#compare the model in table
data.frame(AIC1, AIC2) # nolint


