# 2_bayes_model_sample_diff.R
# Bayesian model of the difference between the actual and target sample size (Bland-Altman)
# May 2020
library(R2WinBUGS)
library(dplyr)

# get the data - running from Rmd for now
#load('data/AnalysisReady.RData') # from 0_read_data.R 

## Prepare the data for winbugs
# create external text file with bugs model
model.file = 'bugs_bland_altman.txt'
bugs = file(model.file, 'w')
cat('model{
for (j in 1:N) {
  diff[j] ~ dnorm(mu[j], tau[j])
  mu[j] <- alpha[1] + alpha[2]*av[j] + alpha[3]*av2[j]
  log(tau[j]) <- beta[1] + beta[2]*av[j] + beta[3]*av2[j]
}
for (i in 1:3){
  alpha[i] ~ dnorm(0, 0.001)
  beta[i] ~ dnorm(0, 0.001)
}
# prediction
for (k in 1:M) {
  alpha_pred[k] <- alpha[1] + alpha[2]*av.pred[k] + alpha[3]*av.pred2[k]
  log(tau_pred[k]) <- beta[1] + beta[2]*av.pred[k] + beta[3]*av.pred2[k]
  diff.pred[k] ~ dnorm(alpha_pred[k], tau_pred[k])
  sigma[k] <- 1/sqrt(tau_pred[k])
}
}\n', file=bugs)
close(bugs)

# MCMC parameters
n_chains = 2
n_samples = 5000
n_thin = 3

# prepare the random data
N = nrow(to_plot) # 
mean_aver = mean(to_plot$aver)
to_plot = mutate(to_plot, av = aver - mean_aver) # centre
M = 25 # number of predictions
av.pred = seq(min(to_plot$av), max(to_plot$av), length.out=M)
bdata = with(to_plot, list(N = N, M=M, diff=diff, av=av, av2=av*av, av.pred=av.pred, av.pred2=av.pred*av.pred))
inits = list(alpha=c(0,0,0), beta=c(0,0,0))
inits = rep(list(inits), n_chains)

# run BUGS
parms = c('alpha','beta','sigma','diff.pred')
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                     n.chains=n_chains, n.iter=n_samples*2*n_thin, n.thin=n_thin, debug=TRUE,
                     bugs.directory="c:/Program Files/WinBUGS14")
bugs.results$summary

save(bugs.results, n_chains, n_samples, n_thin, av.pred, mean_aver, file='results/bland_altman_bayes_model.RData') # 
