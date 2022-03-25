# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 

# Load the required libraries
library(runjags)
source("HDIofMCMC.r") # Load the HDI function

#------------------------------------------------------------------------------- 
# Load The data file 
myDataFrame = read.csv( file="TwoGroupIQ.csv" )
# For purposes of this one-group example, use data from Smart Drug group:
IQscore = myDataFrame$Score[myDataFrame$Group=="Smart Drug"]
# plot the histogram of the data
hist(IQscore)

## Save the length of the data and number of subjects (We want to pass this info to JAGS)
y = IQscore
Ntotal = length(y)


# prepare the data for JAGS
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Initialize chains
inits1 <- dump.format(list(mu=100, sigma=30, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(mu=120, sigma=20, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(mu=80, sigma=40, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Tell JAGS which latent variables to monitor
monitor = c("mu", "sigma")

# Run the function that fits the models using JAGS
results <- run.jags(model="../models/oneSampleNorm.txt",
	monitor=monitor, data=dat, n.chains=3, 
	inits=c(inits1, inits2, inits3), plots = FALSE,
	burnin=4000, sample=1000, thin=4)

# read the summary of the results
results

# plot the chains
plot(results$mcmc)

# readout the 3 chains from the "results" structure and combine them into a single matrix
# each of the resulting matrix represent a single MCMC sample, the columns represent the monitored variables
chains = rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]])

## store the estimates of interest in vector variables for easier manipulation (see below)
mu.est = chains[,"mu"]

# Do not close this plot window, because we sill update this plot (see code below)
plot(density(mu.est), xlim=c(90, 130), lwd=3)


# Return the HDI of the MCMC of interest
hdi = HDIofMCMC(mu.est , credMass=0.95)

# add the estimated median of the MCMC of interest
abline(v=median(mu.est), col="red", lwd=2)

# add HDI limits of the MCMC of interest
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)

# compare the HDI limits to 100
# Can we conclude that this distribution is different from 100?
abline(v=100, col="grey", lwd=2)

# You can also get a "p-value" from the MCMC 
mean(mu.est<100) * 2





