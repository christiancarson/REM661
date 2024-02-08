# Date: 01/29/2024 # nolint
# Last updated: 01/29/2024
# Name: Christian Carson
# Description: Assignment 1 - Generalized linear model - REM 661
# Notes:

#libs
#install.packages("conflicted")
library(conflicted)

#table of conflicts


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
### LOAD PACKAGES ###
library()

#pairwise correlations#
cor(data)

### DATA EXPLORATION ###
plot(data$Temperature, data$CPUE, xlab = "Temperature", ylab = "CPUE", main = "Scatterplot of CPUE vs. Temperature")

### MODEL 1 ###
model1 <- glm(CPUE ~ Temperature, data = data, family = gaussian(link = "identity"))
summary(model1)




#####fish mortality as a function of temperature, fight duration,
#and air exposure time########
#define number of observations
N <- 250 # nolint
#define independent variables
#normally distributed fight time variable
FightTime <- rlnorm(N, 0, 0.5) #n = n, meanlog = 0, sdlog = 0.5  # nolint
#normally distributed air temperature variable, rep 5 times each
AirTemp <- rep(18:22, each = N/5) # nolint
#amount of time catching fish, pulling them out of water, and taking pictures
AirExposure <- rlnorm(N, -1, 0.2) # nolint #n = n, meanlog = -1, sdlog = 0.2

#simulate model on logit scale


#put everthing into a dataframe

#estimate the model using the logit link function
