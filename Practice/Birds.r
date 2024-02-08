# Date: 02/05/2024 # nolint
# Last updated: 02/05/2024
# Name: Christian Carson
# Description: Bird data analysis
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
datafile <- "/Users/critty/Desktop/GitHub/MRM Coursework/REM661/Assignment 1/Input/ground bird data.csv" # nolint: line_length_linter.

### INITIAL SET-UP ###
setwd(dir.data)

data <- read.csv(file = datafile, header=T)  # nolint

#transform density data to presence/absence
for (i in 1:nrow(data)) {
  if (data$Density[i] > 0) {
    data$Presence[i] <- 1
  } else {  
    data$Presence[i] <- 0
  }
}

data$Presence <- as.factor(data$Presence)

colnames(data)
#model Pres ~ Elev.m + Slope + Ave.temp + Ave.ppt + Pct.graze + Pct.grass +Road.dens
M1 <- glm(Presence ~ Elev.m + Slope + Ave.temp + Ave.ppt + Pct.graze + Pct.grass + Road.dens, data = data, family = binomial(link = "logit"))
