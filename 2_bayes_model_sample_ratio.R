# 2_bayes_model_sample_ratio.R
# Bayesian model of the ratio between the actual and target sample size (Bland-Altman)
# February 2021
library(R2WinBUGS)
library(dplyr)

# get the data and add the average and ratio
load('data/AnalysisReady.RData') # from 0_read_data.R 
for.model  = filter(studies, !is.na(samplesize_actual)) %>% # must have actual sample size
  mutate(samplesize_target_log = log(samplesize_target+0.1), # add small constant because of zeros
    samplesize_actual_log = log(samplesize_actual+0.1),
    diff = samplesize_actual_log - samplesize_target_log,
    #diff_perc = 100*(exp(diff)-1), # percent difference
    aver = (samplesize_actual_log + samplesize_target_log)/2 ) %>%
  select(number, samplesize_target_log, samplesize_actual_log, diff, aver )

## Prepare the data for winbugs
# create external text file with bugs model; fractional polynomial version
model.file = 'bugs_bland_altman_fp.txt'
bugs = file(model.file, 'w')
cat('model{
for (j in 1:N) {
  diff[j] ~ dnorm(mu[j], tau[j])
  mu[j] <- inprod(alpha[1:C], X_mu[j,1:C])
  log(tau[j]) <- inprod(beta[1:C], X_tau[j,1:C])
}
for (i in 1:C){
  alpha[i] ~ dnorm(0, 0.001)
  beta[i] ~ dnorm(0, 0.001)
}
# prediction
for (k in 1:M) {
  alpha_pred[k] <- inprod(alpha[1:C], X_mu_pred[k,1:C])
  log(tau_pred[k]) <- inprod(beta[1:C], X_tau_pred[k,1:C])
  diff.pred[k] ~ dnorm(alpha_pred[k], tau_pred[k])
  sigma[k] <- 1/sqrt(tau_pred[k])
}
}\n', file=bugs)
close(bugs)

# MCMC parameters
n_chains = 2
n_samples = 5000
n_thin = 3

# fractional polynomials to test varying change over time; also no change:
powers = c(-2, -1, -0.1, 0, 0.5, 1, 2, 3)

# prepare the random data
for.model = sample_n(for.model, 1000) # temporary
N = nrow(for.model) # 
mean_aver = mean(for.model$aver)
for.model = mutate(for.model, av = aver - mean_aver) # centre
M = 25 # number of predictions
av.pred = seq(min(for.model$av), max(for.model$av), length.out=M)
# transform predictions
power = 2
X_mu_pred = cbind(rep(1, M), av.pred^power)
X_tau_pred = cbind(rep(1, M), av.pred^power)
# transform data using fractional polynomial
X_mu = cbind(rep(1, N), for.model$av^power)
X_tau = cbind(rep(1, N), for.model$av^power)
C = 2
bdata = with(for.model, list(N = N, C=C, M=M, diff=diff, X_mu=X_mu, X_tau=X_tau, X_mu_pred=X_mu_pred, X_tau_pred=X_tau_pred))
inits = list(alpha=rep(0,C), beta=rep(0,C))
inits = rep(list(inits), n_chains)

# run BUGS
parms = c('alpha','beta','sigma','diff.pred')
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                     n.chains=n_chains, n.iter=n_samples*2*n_thin, n.thin=n_thin, DIC=TRUE, debug=TRUE,
                     bugs.directory="c:/Program Files/WinBUGS14")
bugs.results$summary

## move to HPC, too slow here
save(for.model, file='//hpc-fs/barnetta/anzctr/bland_altman_data.RData')
