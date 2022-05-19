# from Pedersen, Frank, & Biele 2017 
# DOI 10.3758/s13423-016-1199-y
# adapted for runjags

library(runjags)
library(rjags)
load.module("wiener")

# *.csv files assumed to be in the working directory
dat = read.csv(file = "../PedersenEtAl2017/data.csv")

# This section allows you to reduce the size of the data set in two ways for testing/teaching purposes ----
niters = 120 #max "trial" number for each subject (true max = 120)
dat = dat[dat$iter <= niters, ]
nsubs = 5 #max mumber of subjects (true max = 17)
dat = dat[dat$ord_sbj <= nsubs, ]
dat$rownum = 1:nrow(dat)

first = aggregate(rownum ~ ord_sbj + med, data = dat, min)
first = matrix(first$rownum, ncol = 2, nrow = nsubs)
last = aggregate(rownum ~ ord_sbj + med, data = dat, max)
last = matrix(last$rownum, ncol = 2, nrow = nsubs)

#note: first and last must be converted to matrix format below if not so already
data = dump.format(list(choice=dat$choice,nonchoice=dat$nonchoice,value=dat$value,RT=dat$rt,pair=dat$pair, valence = dat$valence, iter=dat$iter, S = length(unique(dat$ord_sbj)), first = as.matrix(first), last = as.matrix(last), not1 = dat$not_cond1, not2 = dat$not_cond2,G = 2))


inits1 <- dump.format(list(tg_mu = 0.17, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(tg_mu = 0.18, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(tg_mu = 0.15, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))
#inits4 <- dump.format(list(tg_mu = 0.19, .RNG.name="base::Super-Duper", .RNG.seed=54321 ))

#currently set for demo only, more 
n.adapt <-1000 
n.burnin <- 10000 # 40000 in Pedersen, Frank, & Biele 2017
n.simu <- 2000 #in Pedersen, Frank, & Biele 2017

#################################
#########  run model ############
#################################

par = c("etag", "eta",'eta_sd',"mg", "m", 'm_sd') 

# Run the function that fits the models using JAGS
start_time = Sys.time()
results_ddm <- run.jags(model="../models/model6.txt",
                    monitor=par, data=data, 
                    inits=c(inits1, inits2, inits3), #, inits4),
                    plots = FALSE,
                    n.chains=3, 
                    adapt =n.adapt, burnin=n.burnin, sample=n.simu, thin=1,
                    modules=c("wiener"), method=c("parallel"))
end_time = Sys.time()
end_time  - start_time

summ_ddm = summary(results_ddm)

chains_ddm = as.data.frame(rbind(results_ddm$mcmc[[1]], results_ddm$mcmc[[2]], results_ddm$mcmc[[3]]))

