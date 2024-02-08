# Date: 01/29/2024 # nolint
# Last updated: 01/29/2024
# Name: Christian Carson
# Description: Assignment 1 - Generalized linear model - REM 661
# Notes: #nolint is used to ignore the line length error, its scattered throughout the code

#libs insallation
#install.packages("conflicted")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("MASS")
#install.packages("pscl")
#install.packages("flextable")
#install.packages("textreadr")

#load libraries
library(conflicted)
library(ggplot2)
library(dplyr)
library(readr)
library(MASS)
library(pscl)
library(flextable)
library(broom)
conflicts_prefer(dplyr::select)


### INPUT ###
rm(list=ls()) # nolint
graphics.off()
getwd()

dir.data <- "/Users/critty/Library/CloudStorage/OneDrive-SimonFraserUniversity(1sfu)/GitHub/MRM Coursework/REM661/Assignment 1/Input/" # nolint
dir.out <- "/Users/critty/Library/CloudStorage/OneDrive-SimonFraserUniversity(1sfu)/GitHub/MRM Coursework/REM661/Asssignment 1/Output/" # nolint
datafile <- "/Users/critty/Library/CloudStorage/OneDrive-SimonFraserUniversity(1sfu)/GitHub/MRM Coursework/REM661/Assignment 1/Input/GN CPUE.csv" # nolint: line_length_linter.

#for reproducibility, use the following data in the datafile from github
#urlfile <- "https://github.com/christiancarson/REM661/blob/main/Assignment%201/Input/GN%20CPUE.csv"
#datafile <- read_csv(url(urlfile))
#dir.out <- "set your output directory here"

### INITIAL SET-UP ###
setwd(dir.data)

data <- read.csv(file = datafile, header=T)  # nolint

#structure of the data
str(data)
#show the first few rows of the data
head(data)
#get variable names
names(data)
#give a summary of the data
summary(data)
#print to see what the data looks like
print(data)

#pairwise correlations to see if any of the variables are correlated
cor(data)
#pairwise correlations bewteen independent variables (Temperature and WaterLevel)
cor(data$Temperature, data$WaterLevel)

#get an overview of the distributionn of each variable
hist(data$Temperature)
hist(data$WaterLevel)
hist(data$Year)
#CPUE Hist small bins
hist(data$CPUE, breaks=200)

#temperature and water level are not correlated and have a norm dis
#Year is not normally distributed and is a factor
#CPUE is not normally distributed and looks like a poisson distribution but likely sometihng else

#first we'll start with a log distribution conversion for CPUE and take a look
#natural lof transform CPUE 
data$LogCPUE <- log(data$CPUE)
#to compare with +1 because log(0) is undefined
data$LogCPUE <- log(data$CPUE + 1)

#plot the log transformed CPUE
hist(data$LogCPUE)

#boxplot for outliers
boxplot(data$LogCPUE)

#ggplot of densit of CPUE
ggplot(data, aes(x=CPUE)) + geom_density()

print(data$CPUE)

#identify outliers
outliers <- boxplot.stats(data$LogCPUE)$out

#we seem to have relativley few options other than log based on the fact CPUE is continuous and positive
#see if CPUE has 0s
sum(data$CPUE == 0)

#assignment says to start with gaussian
#M1 = Base model
M1 <- glm(CPUE ~ Temperature + WaterLevel + Year, family=gaussian(link="identity"), data=data)
#M2 = Interaction model with temp and water level
M2 <- glm(CPUE ~ Temperature * WaterLevel + Year, family=gaussian(link="identity"), data=data)
#M3 = Quadratic model with temp being quadratic
M3 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + Year, family=gaussian(link="identity"), data=data)
#M4 <- water level being quadratic
M4 <- glm(CPUE ~ Temperature + WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="identity"), data=data)
#M5 <- both temp and water level being quadratic or plynomial
M5 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="identity"), data=data)
#M6 = interaction between both temp and water level as a polynomial
M6 <- glm(CPUE ~ Temperature + I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="identity"), data=data)
#M7 = interaction across all, both temp and water level as a polynomial
M7 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * Year, family=gaussian(link="identity"), data=data)
#M8 = interaction all but year and  both temp and water level as a polynomial 
M8 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="identity"), data=data)
#M9 = interaction all but year and  both temp and water level as a polynomial and factor for year
M9 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + factor(Year), family=gaussian(link="identity"), data=data)
#M10 = interaction all and  both temp and water level as a polynomial and factor for year
M10 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * factor(Year), family=gaussian(link="identity"), data=data)
#check residuals
par(mfrow=c(2,2))
plot(M1)
plot(M2)
plot(M3)
plot(M4)
plot(M5)
plot(M6)
plot(M7)
plot(M8)
plot(M9)
plot(M10)

#check AIC
AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
AIC(M5)
AIC(M6)
AIC(M7)
AIC(M8)
AIC(M9)
AIC(M10)

#ok so the best model seemed to be 9, M9 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + factor(Year), family=gaussian(link="identity"), data=data)
#but the residuals are all over the place and the AIC is high, this is likely due to the fact that the CPUE right skewed

#now I am going to try all the same log link
#M11 = Base model
M11 <- glm(CPUE ~ Temperature + WaterLevel + Year, family=gaussian(link="log"), data=data)
#M12 = Interaction model with temp and water level
M12 <- glm(CPUE ~ Temperature * WaterLevel + Year, family=gaussian(link="log"), data=data)
#M13 = Quadratic model with temp being quadratic
M13 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + Year, family=gaussian(link="log"), data=data)
#M14 <- water level being quadratic
M14 <- glm(CPUE ~ Temperature + WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="log"), data=data)
#M15 <- both temp and water level being quadratic or plynomial
M15 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="log"), data=data)
#now add interaction terms
#M16 = interaction between both temp and water level as a polynomial
M16 <- glm(CPUE ~ Temperature + I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="log"), data=data)
#M17 = interaction across all, both temp and water level as a polynomial
M17 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * Year, family=gaussian(link="log"), data=data)
#M18 = interaction all but year and  both temp and water level as a polynomial
M18 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=gaussian(link="log"), data=data)
#M19 = interaction all but year and  both temp and water level as a polynomial and factor for year
M19 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + factor(Year), family=gaussian(link="log"), data=data)
#M20 = interaction all and  both temp and water level as a polynomial and factor for year
M20 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * factor(Year), family=gaussian(link="log"), data=data)
#model 20 truncated due to divergence -> removing it in the model summaries
#check residuals
par(mfrow=c(2,2))
plot(M11)
plot(M12)
plot(M13)
plot(M14)
plot(M15)
plot(M16)
plot(M17)
plot(M18)
plot(M19)
plot(M20)

#check AIC
AIC(M11)
AIC(M12)
AIC(M13)
AIC(M14)
AIC(M15)
AIC(M16)
AIC(M17)
AIC(M18)
AIC(M19)
AIC(M20)

#yet again, the interaction model with temp and water level as a polynomial and factor for year seems to be the best
#but variance is through the roof and along with AIC

#it is clear that polynomials on the temp and water level have substantially better residuals and AIC, as well as interaction
#considering I have read so far, I'm going to try a gamma distribution with a Gamma and log link
#https://www.ices.dk/sites/pub/CM%20Doccuments/CM-2008/I/I0708.pdf

#same models as above but with a gamma distribution and log link
#M1 = Base model
M21 <- glm(CPUE ~ Temperature + WaterLevel + Year, family=Gamma(link="log"), data=data)
#M2 = Interaction model with temp and water level
M22 <- glm(CPUE ~ Temperature * WaterLevel + Year, family=Gamma(link="log"), data=data)
#M3 = Quadratic model with temp being quadratic
M23 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + Year, family=Gamma(link="log"), data=data)
#M4 <- water level being quadratic
M24 <- glm(CPUE ~ Temperature + WaterLevel + I(WaterLevel^2) + Year, family=Gamma(link="log"), data=data)
#M5 <- both temp and water level being quadratic or plynomial
M25 <- glm(CPUE ~ Temperature + I(Temperature^2) + WaterLevel + I(WaterLevel^2) + Year, family=Gamma(link="log"), data=data)
#M6 = interaction between both temp and water level as a polynomial
M26 <- glm(CPUE ~ Temperature + I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=Gamma(link="log"), data=data)
#M7 = interaction across all, both temp and water level as a polynomial
M27 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * Year, family=Gamma(link="log"), data=data)
#M8 = interaction all but year and  both temp and water level as a polynomial
M28 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=Gamma(link="log"), data=data)
#M9 = interaction all but year and  both temp and water level as a polynomial and factor for year
M29 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel + I(WaterLevel^2) + factor(Year), family=Gamma(link="log"), data=data)
#M10 = interaction all and  both temp and water level as a polynomial and factor for year
M30 <- glm(CPUE ~ Temperature * I(Temperature^2) * WaterLevel * I(WaterLevel^2) * factor(Year), family=Gamma(link="log"), data=data)

#check residuals
par(mfrow=c(2,2))
plot(M21)
plot(M22)
plot(M23)
plot(M24)
plot(M25)
plot(M26)
plot(M27)
plot(M28)
plot(M29)
plot(M30)

#check AIC
AIC(M21)
AIC(M22)
AIC(M23)
AIC(M24)
AIC(M25)
AIC(M26)
AIC(M27)
AIC(M28)
AIC(M29)
AIC(M30)

#unlike both the gaussian with identity and log link, the gamma distribution with log link seems to have the best AIC and residuals and the best model is M26 <- glm(CPUE ~ Temperature + I(Temperature^2) * WaterLevel + I(WaterLevel^2) + Year, family=Gamma(link="log"), data=data)
#this model had an AIC of 691, but still the residuals are not great
#gonna try inverse gaussian with log link for shits and giggles

#Inverse Gaussian GLM with a log links
#M1 = Base model - Temperature and WaterLevel and Year with a inverse gaussian distribution and log link
M31 <- glm(CPUE ~ Temperature + WaterLevel + Year, family=inverse.gaussian(link="log"), data=data)
#error: "Error: inner loop 1; cannot correct step size, In addition: Warning messages: 1: step size truncated due to divergence 2: step size truncated due to divergence "

#nvm:) that didn't work

#code adapted from https://www.r-bloggers.com/2019/07/creating-a-summary-table-of-glm-models-using-broom-and-flextable/
extract_model_info <- function(model, model_name) {
  if (!inherits(model, "glm")) {
    stop("Model is not a glm object")
  }

  model_family <- tolower(family(model)$family)
  distribution_type <- ifelse(model_family == "gamma", "gamma",
                              ifelse(model_family == "gaussian", "gaussian", "other"))
  link_function <- model$family$link

  df <- broom::glance(model) %>%
    dplyr::mutate(Model = model_name,
                  Formula = paste(as.character(formula(model)), collapse = " "),
                  Distribution = distribution_type,
                  Link = link_function)

  df <- df %>%
    dplyr::select(Model, Formula, AIC, Distribution, Link)

  return(df)
}

#code adapted from https://www.r-bloggers.com/2019/07/creating-a-summary-table-of-glm-models-using-broom-and-flextable/
#df, removed M20 due to error in model fit
model_summaries <- bind_rows(
  extract_model_info(M1, "M1"),
  extract_model_info(M2, "M2"),
  extract_model_info(M3, "M3"),
  extract_model_info(M4, "M4"),
  extract_model_info(M5, "M5"),
  extract_model_info(M6, "M6"),
  extract_model_info(M7, "M7"),
  extract_model_info(M8, "M8"),
  extract_model_info(M9, "M9"),
  extract_model_info(M10, "M10"),
  extract_model_info(M11, "M11"),
  extract_model_info(M12, "M12"),
  extract_model_info(M13, "M13"),
  extract_model_info(M14, "M14"),
  extract_model_info(M15, "M15"),
  extract_model_info(M16, "M16"),
  extract_model_info(M17, "M17"),
  extract_model_info(M18, "M18"),
  extract_model_info(M19, "M19"),
  extract_model_info(M21, "M21"),
  extract_model_info(M22, "M22"),
  extract_model_info(M23, "M23"),
  extract_model_info(M24, "M24"),
  extract_model_info(M25, "M25"),
  extract_model_info(M26, "M26"),
  extract_model_info(M27, "M27"),
  extract_model_info(M28, "M28"),
  extract_model_info(M29, "M29"),
  extract_model_info(M30, "M30")
)

#code adapted from https://www.r-bloggers.com/2019/07/creating-a-summary-table-of-glm-models-using-broom-and-flextable/
#delta AIC 
min_aic <- min(model_summaries$AIC)
model_summaries <- model_summaries %>%
  mutate(deltaAIC = AIC - min_aic)

#sort by delta AIC
model_summaries <- model_summaries %>%
  arrange(deltaAIC)

#flextable
flextable(model_summaries)

#save to output
setwd(dir.out)
write.csv(model_summaries, "model_summaries.csv")

