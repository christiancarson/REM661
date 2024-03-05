# Sockeye SR.R
# This code estimates Ricker and Larkin parameters for multiple sockeye stock-recruit datasets and plots results
# Date: March 3, 2024
# Programmer: Christian Carson

# DATA SECTION
# Read all data here and calculate constants
getwd()
# /Users/critty/Library/CloudStorage/OneDrive-SimonFraserUniversity(1sfu)/GitHub/MRM Coursework/REM661/Assingment 4
setwd("/Users/critty/Library/CloudStorage/OneDrive-SimonFraserUniversity(1sfu)/GitHub/MRM Coursework/REM661/Assingment 4")

file.name <- "Sockeye.data"
data      <- read.table(file=file.name, header=TRUE, sep="\t")
stocks    <- unique( data$Stock )
nstock    <- length( stocks )

# NOTE that spawners and recruits are already adjusted to line up - so spawners are actually 
#   what laid eggs 3 years previous

# PROCEDURE SECTION

# create a function to estimate parameters and fitted values based on the Ricker model
"Ricker" <- function(){
  # Fit linear model to estimate alpha and beta in log( R/S ) = alpha - beta * S
  
  # create a list to fill with outputs from each stock
  fit <- list()    
  
  # loop over stocks
  for( i in 1:nstock ){
    # identify parts of data file that corresponds to this stock
    stock.id   <- data$Stock == stocks[i]
    # separate out stock-specific recruits and spawners
    R          <- data$Recruits[ stock.id ]
    S          <- data$Spawners[ stock.id ]
    fit[[i]]   <- lm( log( R/S ) ~ S )   # linear regression on transformed Ricker model
    fit[[i]]$r <- exp( fit[[i]]$fitted.values ) * S # back-transform to original Ricker model
  }
  
  # return findings so they are preserved outside the function
  return( fit )
}

# create a function to estimate parameters and fitted values based on the Larkin model

  # Fit linear model to estimate alpha and beta in log( R/S ) = alpha - beta * S
  
  # create a list to fill with outputs from each stock
  fit <- list(){
  
  # loop over stocks
  for( i in 1:nstock ){
    # identify parts of data file that corresponds to this stock
    stock.id   <- data$Stock == stocks[i]
    # separate out stock-specific recruits and spawners
    R          <- data$Recruits[ stock.id ]
    S          <- data$Spawners[ stock.id ]
    fit[[i]]   <- lm( log( R/S ) ~ S )   # linear regression on transformed Larkin model
    fit[[i]]$r <- exp( fit[[i]]$fitted.values ) * S # back-transform to original Larkin model
  }
  
  # return findings so they are preserved outside the function
  return( fit )
}

#create a function to estimate parameters and fitted values based on the Larkin model
"Larkin" <- function(){
  # Fit linear model to estimate alpha and beta in log( R/S ) = alpha - beta * S
  
  # create a list to fill with outputs from each stock
  fit <- list()    
  for( i in 1:nstock ){
    stock.id   <- data$Stock == stocks[i]
    R          <- data$Recruits[ stock.id ]
    S          <- data$Spawners[ stock.id ]
    nyr       <- length( R )
    #all recruits except first three years
    Rt        <- R[ 4:nyr ]
    #all spawners except for first three years
    St        <- S[ 4:nyr ]
    #Spanwers 1 year before current year
   Sm1 <- S[ 4: (nyr-1) ]
    #Spanwers 2 years before current year
    Sm2 <- S[ 4: (nyr-2) ]
    #Spanwers 3 years before current year
    Sm3 <- S[ 4: (nyr-3) ]
  }
  return( fit )
}

# create a glm function to estimate parameters and fitted values based on the Ricker model and Larkin model
#and plot the results
"plot" <- function(){
  # Fit linear model to estimate alpha and beta in log( R/S ) = alpha - beta * S
  
  # create a list to fill with outputs from each stock
  fit <- list()    
  for( i in 1:nstock ){
    stock.id   <- data$Stock == stocks[i]
    R          <- data$Recruits[ stock.id ]
    S          <- data$Spawners[ stock.id ]
    nyr       <- length( R )
    #all recruits except first three years
    Rt        <- R[ 4:nyr ]
    #all spawners except for first three years
    St        <- S[ 4:nyr ]
    #Spanwers 1 year before current year
   Sm1 <- S[ 4: (nyr-1) ]
    #Spanwers 2 years before current year
    Sm2 <- S[ 4: (nyr-2) ]
    #Spanwers 3 years before current year
    Sm3 <- S[ 4: (nyr-3) ]
  }
  return( fit )
}
library(ggplot2)
#plot the results
ggplot2::ggplot( data, aes( x=Spawners, y=Recruits, color=Stock ) ) +
  geom_point() +
  geom_line( aes( x=Spawners, y=r, color=Stock ), data=fit[[i]] ) +
  facet_wrap( ~Stock ) +
  theme( legend.position="none" ) +
  xlab( "Spawners" ) +
  ylab( "Recruits" ) +
  ggtitle( "Ricker Model" )


# create a loop to isolate data from each stock, run the Ricker() and Larkin() functions, and plot predicted recruitment on top of data from that stock
