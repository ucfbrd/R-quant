library(moments)
set.seed(1337)

# Brief example of a binomial tree exercise in R

# Inputs (arbitrary numbers)
rf <- .03; mu <- 0.1; sigma <- 0.15; s0 <- 1; t <- 1; n <- 252
Nsims <- c(1000,10000,100000)
statlabel <- c("Mean","Standard Deviation","Kurtosis","Skewness")
S0 <- 1

Nperiods <- 5

# Computing calibrated parameters u, d, and p
u <- exp(sigma*sqrt(t/n))
d <- 1/u
p <- 0.5 + 0.5 * (mu/sigma) * sqrt(t/n)
parameters <- c(u,d,p)
names(parameters) <- c("u","d","p"); parameters

# Creating matrices stats to later store the statistics of terminal prices
# Note that each row corresponds to a combination of number of periods and number of simulations
stats <- matrix(NA,nrow=length(Nsims)*length(Nperiods),ncol=4)
rownames(stats) <- c(outer(paste(Nsims," "), Nperiods, FUN=paste0)); colnames(stats) <- statlabel

# Binomial trees are generated without loops. However, I used loops to generate all three requested number of 
# periods as well as number of simulations.
# x is a counting variable
x <-0
for (i in 1:length(Nsims)){
  for (j in 1:length(Nperiods)){
    
    x<-x+1
    
    # Path will be populated with the generated binomial tree, according to the matching number of simulations and
    # periods
    path <- matrix(S0, nrow = Nsims[i],ncol=Nperiods[j]+1)
    # Shock is a matrix with N*Nt random number drawn from an uniform distribution. I use it to create the signal
    # of whether the process went up or down the tree (using p previously calculated)
    shock <- matrix(runif(Nsims[i]*Nperiods[j]), nrow = Nsims[i], ncol = Nperiods[j])
    shocksign <- (shock <= p)
    return <- u*shocksign+d*(!shocksign)
    path[,-1] <- S0*t(apply(return,1,cumprod))
    S_terminal <- path[,ncol(path)]
    
    # Generate a histogram. Note that this is inside loop, so overall 3*3 = 9 histograms will be generated in the end
    hist(S_terminal,breaks=20,main=paste("Distribution of Terminal Prices (N=",Nperiods[j],",",Nsims[i],"simulations, daily step)"),xlab="Terminal Price")
    
    # Populate the corresponding row of the stats matrix with the statistics for these paths
    stats[x,] <- c(mean(S_terminal),sd(S_terminal),kurtosis(S_terminal),skewness(S_terminal))
    
  }
}

print("Statistics for Terminal Prices, daily time step"); stats

#######################
#### FOR 1 HOUR
#######################

# Time step is now 252 days, 8 hours per day
n <- 252*8
#Time horizon now is 5 days, 8 hours per day
Nperiods <- 5*8

# Recomputing calibrated parameters u, d, and p
u <- exp(sigma*sqrt(t/n))
d <- 1/u
p <- 0.5 + 0.5 * (mu/sigma) * sqrt(t/n)
parameters <- c(u,d,p)
names(parameters) <- c("u","d","p"); parameters

# Creating matrices stats to later store the statistics of terminal prices
# Note that each row corresponds to a combination of number of periods and number of simulations
stats <- matrix(NA,nrow=length(Nsims)*length(Nperiods),ncol=4)
rownames(stats) <- c(outer(paste(Nsims," "), Nperiods, FUN=paste0)); colnames(stats) <- statlabel

# Binomial trees are generated without loops. However, I used loops to generate all three requested number of 
# periods as well as number of simulations.
# x is a counting variable
x <-0
for (i in 1:length(Nsims)){
  for (j in 1:length(Nperiods)){
    
    x<-x+1
    
    # Path will be populated with the generated binomial tree, according to the matching number of simulations and
    # periods
    path <- matrix(S0, nrow = Nsims[i],ncol=Nperiods[j]+1)
    # Shock is a matrix with N*Nt random number drawn from an uniform distribution. I use it to create the signal
    # of whether the process went up or down the tree (using p previously calculated)
    shock <- matrix(runif(Nsims[i]*Nperiods[j]), nrow = Nsims[i], ncol = Nperiods[j])
    shocksign <- (shock <= p)
    return <- u*shocksign+d*(!shocksign)
    path[,-1] <- S0*t(apply(return,1,cumprod))
    S_terminal <- path[,ncol(path)]
    
    # Generate a histogram. Note that this is inside loop, so overall 3*3 = 9 histograms will be generated in the end
    hist(S_terminal,breaks=20,main=paste("Distribution of Terminal Prices (N=",Nperiods[j],",",Nsims[i],"simulations, hourly step)"),xlab="Terminal Price")
    
    # Populate the corresponding row of the stats matrix with the statistics for these paths
    stats[x,] <- c(mean(S_terminal),sd(S_terminal),kurtosis(S_terminal),skewness(S_terminal))
    
  }
}

print("Statistics for Terminal Prices, Hourly time step"); stats

##############################
### MINUTE STEP
##############################

# Time step is now 252 days, 8 hours per day, 60 minutes per hour
n <- 252*8*60
#Time horizon now is 5 days, 8 hours per day, 60 minutes per hour
Nperiods <- 5*8*60

# Recomputing calibrated parameters u, d, and p
u <- exp(sigma*sqrt(t/n))
d <- 1/u
p <- 0.5 + 0.5 * (mu/sigma) * sqrt(t/n)
parameters <- c(u,d,p)
names(parameters) <- c("u","d","p"); parameters

# Creating matrices stats to later store the statistics of terminal prices
# Note that each row corresponds to a combination of number of periods and number of simulations
stats <- matrix(NA,nrow=length(Nsims)*length(Nperiods),ncol=4)
rownames(stats) <- c(outer(paste(Nsims," "), Nperiods, FUN=paste0)); colnames(stats) <- statlabel

# Binomial trees are generated without loops. However, I used loops to generate all three requested number of 
# periods as well as number of simulations.
# x is a counting variable
x <-0
for (i in 1:length(Nsims)){
  for (j in 1:length(Nperiods)){
    
    x<-x+1
    
    # Path will be populated with the generated binomial tree, according to the matching number of simulations and
    # periods
    path <- matrix(S0, nrow = Nsims[i],ncol=Nperiods[j]+1)
    # Shock is a matrix with N*Nt random number drawn from an uniform distribution. I use it to create the signal
    # of whether the process went up or down the tree (using p previously calculated)
    shock <- matrix(runif(Nsims[i]*Nperiods[j]), nrow = Nsims[i], ncol = Nperiods[j])
    shocksign <- (shock <= p)
    return <- u*shocksign+d*(!shocksign)
    path[,-1] <- S0*t(apply(return,1,cumprod))
    S_terminal <- path[,ncol(path)]
    
    # Generate a histogram. Note that this is inside loop, so overall 3*3 = 9 histograms will be generated in the end
    hist(S_terminal,breaks=20,main=paste("Distribution of Terminal Prices (N=",Nperiods[j],",",Nsims[i],"simulations, minute step)"),xlab="Terminal Price")
    
    # Populate the corresponding row of the stats matrix with the statistics for these paths
    stats[x,] <- c(mean(S_terminal),sd(S_terminal),kurtosis(S_terminal),skewness(S_terminal))
    
  }
}

print("Statistics for Terminal Prices, Minute time step"); stats



