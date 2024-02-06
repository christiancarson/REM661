### Week 4 - GLMs
### Brett
### Feb 1, 2024
### We are going to simulate fish mortality as a function
####   of temperate, fight time and air exposure

# (1) define the number of observations
n <- 250

# (2) define my independent variables
# this is the lognormally distributed fight time variable
FightTime <- rlnorm( n=n, meanlog=0, sdlog=0.5 )
Temperature <- rep( 18:22, each=n/5 )
AirExposure <- rlnorm( n=n, meanlog=-1, sdlog=0.2 )

# (3) simulate the model on the logit scale
logitAlive <- 1 + -0.5*FightTime + -0.05*Temperature +
  -0.05*AirExposure
# probability of each animal being alive
pAlive <- plogis( logitAlive )

# generating alive/dead data for each fish
Alive <- rbinom( n=n, size=1, prob=pAlive )

# (4) put everything in a data frame
AliveDF <- data.frame( Alive=Alive, FightTime=FightTime,
                       Temperature=Temperature,
                       AirExposure=AirExposure )

# (5) estimate the actual model using the logit link function
# determine correlations in independent variables
# remove the dependent variable
noAliveDF <- AliveDF[,-1]
cor( noAliveDF )

Fit1 <- glm( formula=Alive ~ FightTime + Temperature +
               AirExposure,
             family=binomial(link="logit"))
Fit2 <- glm( formula=Alive~FightTime+Temperature+
               I(Temperature^2), family=
               binomial(link="logit"))

# (6) summarize the model fit
summary( Fit1 )
summary( Fit2 )

AIC1 <- Fit1$aic
AIC2 <- Fit2$aic

