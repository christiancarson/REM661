# Date: 01/29/2024
# Last updated: 01/29/2024
# Name: Christian Carson
# Description: Assignment 1 - Generalized linear model - REM 661
# Notes: 

### INPUT ###
rm(list=ls())
graphics.off()
getwd()

dir.data <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Assignment 1/Input/"
dir.out <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Asssignment 1/Output/"
datafile <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Assignment 1/Input/GN CPUE.csv"

### INITIAL SET-UP ###
setwd(dir.data)

data <- read.csv(file = datafile, header=T) 

str(data)
head(data)
names(data)
summary(data)

### LOAD PACKAGES ###
library()

#pairwise correlations#
cor(data)

### MODEL 1 ###
model1 <- glm(CPUE ~ Temperature, data = data, family = gaussian(link = "identity"))
summary(model1)



