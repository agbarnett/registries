# 2_bayes_model_samplesize.R
# bayesian model for sample size using spike-slab prior
# using conventional lasso instead
# May 2020
library(R2WinBUGS)
library(dplyr)

# may need to shift to lyra and jags

# visualise prior for variable include:
hist(rbeta(n=1000, shape1=2, shape2=2))

# get the data
load('data/AnalysisReady.RData') # from 0_read_data.R 

## Prepare data for regression model
# change missing to a category
for.model = filter(studies, 
                   !is.na(gender),  # just 4 missing gender
                   !is.na(submitted),  # just 2 missing date
                   samplesize_target > 0) %>% # exclude small number with 0 sample size
  mutate(
    #   date = as.numeric(submitted - as.Date('2010-01-01')) / (5*365.25), # standardised to five years
    # make combined age limit variable
    age_limit = case_when(
      age_max_type == 'No limit' ~ 'No limit',
      age_max_type == 'Not stated' ~ 'Not stated',
      age_max_type == 'Restricted' & age_max <18  ~ 'Under 18',
      age_max_type == 'Restricted' & age_max ==18  ~ 'Exactly 18',
      age_max_type == 'Restricted' & age_max >18  ~ 'Over 18',
    ),
    # add categories for missing
    endpoint = ifelse(is.na(endpoint), 'Missing', endpoint),
    masking = ifelse(is.na(masking), 'Missing', masking),
    endpoint = ifelse(is.na(endpoint), 'Missing', endpoint),
    phase = ifelse(is.na(phase), 'Missing', phase))

## Prepare the data for winbugs
# create external text file with bugs model
model.file = 'bugs_spike_slab.txt'
bugs = file(model.file, 'w')
cat('model{
# Prior specification
sdBeta ~ dunif(0, 10) # Uniform distribution on std. deviation
tau_in <- pow(sdBeta, −2) # Convert std. deviation to precision tau_in
pind ~ dbeta(2, 2)
for (j in 1 : M) {
	ind[j] ~ dbern(pind) # Define indicators
	zbeta_unselected[j] ~ dnorm( 0 , tau_in) # Prior unconditional 
	zbeta[j] <- ind[j] * zbeta_unselected[j]
}
# Likelihood specification
for (i in 1:N) {
	size[i]~ dnorm( mu[i], tau) 
	mu[i] <- intercept + inprod( X[i,1:M], zbeta[1:M] )
}
intercept ~ dnorm(0, 0.001)
tau ~ dgamma(1,1)
}\n', file=bugs)
close(bugs)

# MCMC parameters
n_chains = 2
n_samples = 1000
n_thin = 2

# prepare the random data
for.model = sample_n(for.model, size=1000, replace=FALSE) # temporary
N = nrow(for.model) # number of repeats
# formula without intercept:
formula = log2(samplesize_target) ~ -1 + gender + studytype + phase + masking + endpoint
X = model.matrix(formula, data=for.model)
M = ncol(X)
bdata = list(N = N, M=M, X=X, size=log2(for.model$samplesize_target))
inits = list(pind=0.5, ind=rbinom(n=M, size=1, prob=0.5), zbeta_unselected=rep(0,M), intercept=mean(bdata$size), tau=1)
inits = rep(list(inits), n_chains)

# run BUGS
parms = c('pind','zbeta','sdBeta','intercept','tau','ind')
bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=model.file,
                     n.chains=n_chains, n.iter=n_samples*2*n_thin, n.thin=n_thin, debug=TRUE,
                     bugs.directory="c:/Program Files/WinBUGS14")
bugs.results$summary
