# 2_elasticnet_model_samplesize_clintrial.R
# elastic net model for predicting target sample size
# version for clinicaltrials.gov data
# used by 3_clintrials_summary.Rmd
# January 2021
library(dplyr)
library(ggplot2)
library(glmnet)
library(broom)
library(stringr)
library(vctrs)
library(olsrr) # for VIF

## co-linear variables from trial and error
colinear = read.table(header=TRUE, sep=',', stringsAsFactors=FALSE, text='
stype,outcome,variable,colinear_with
Interventional,target,allocationN/A,assignmentSingle Group
Interventional,actual,age_limit_minExactly 18,age_limit_minOver 18 
')

# get the data
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 
source('2_prep_clintrials.R')
  
## split by: 1) observational or not, 2) actual or target
all_ests = variables_excluded = numbers = vars = xval = NULL
standard_models = list()
types = c('Observational','Interventional')
otypes = c('target','actual') # outcome (target or actual sample size)
for (otype in otypes){ # loop through model types
  for (stype in types){
    mtype = paste(otype, stype) # model type
    cat(otype, ', ', stype,'\n', sep='')
    set.seed(30991199) # for x-validation
    
## make design matrix
# formula without intercept:
vars_to_include = c('date','gender','age_limit_max','age_limit_min','volunteers',
                    'behavioral','biological','combination','device','diagnostic','dietary','drug','genetic','procedure','radiation','other',
                    'n_primary','n_secondary','n_condition','n_arms','lead_sponsor_class')
if(stype == 'Interventional'){
  vars_to_include = c(vars_to_include, 'phase','purpose','masking',
                      'assignment','allocation') # add trial details ****
  this_vars = data.frame(stype = 'Interventional', variables = vars_to_include)
  vars = bind_rows(vars, this_vars) # for variable summary table
}
if(stype == 'Observational'){
  vars_to_include = c(vars_to_include, 'study_design_observational','study_design_time') # add observational details
  this_vars = data.frame(stype = 'Observational', variables = vars_to_include)
  vars = bind_rows(vars, this_vars) # for variable summary table
} 
if(otype == 'actual'){vars_to_include = c(vars_to_include, 'status')} # add status for completed studies
names(for.model)[names(for.model) %in% vars_to_include==FALSE] # check what's not included
vars_to_include[vars_to_include %in% names(for.model) ==FALSE] # should be empty
formula = paste('log(sample_size) ~ -1 +', paste(vars_to_include, collapse = '+'))

# just one study type (observational or interventional); and remove missing outcome
if(otype=='target'){
  this_data = filter(for.model, 
                     study_type == stype,
                     sample_size_type =='Anticipated')
}
if(otype=='actual'){
  this_data = filter(for.model, 
                     study_type == stype,
                     sample_size_type =='Actual')
}
n_missing_outcome = sum(is.na(this_data$samplesize_actual))
this_data = filter(this_data, !is.na(sample_size)) # remove missing dependent variable

# prepare data for glmnet
X = model.matrix(as.formula(formula), data=this_data)
# remove X's with no variance (all the same value)
variances = apply(X, 2, sd)
index = which(variances==0)
X = X[, !1:ncol(X)%in%as.numeric(index)] # remove from X
# option to remove co-linear variables
to_remove = filter(colinear, stype==stype, outcome==otype)
if(nrow(to_remove) > 0){
  drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
  X = X[, !drop]
}

# dependent variable:
y = log(this_data$sample_size+1) # could try other logs; +1 due to zero

# run model
enet_model = glmnet(y=y, x=X, alpha=0.95)
plot(enet_model, main=paste(stype, ', ', otype, sep=''))

# x-validation to find best cut-off
cv_enet_model = cv.glmnet(y=y, x=X, alpha=0.95) # elastic net penalty
plot(cv_enet_model, main=paste(stype, ', ', otype, sep=''))
beta = coef(cv_enet_model, s='lambda.1se')
# store x-validation results for later plotting:
cv_ests = tidy(cv_enet_model) %>%  
  mutate(
    lambda.1se = cv_enet_model$lambda.1se, # add lambda min and 1SE
    lambda.min = cv_enet_model$lambda.min,
    outcome = otype, # add outcome
    study_type = stype) # add model type
xval = bind_rows(xval, cv_ests)

## re-run standard model for best coefficients with confidence intervals ##
# use sum to zero constraint to remove reference groups #
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
vars_excluded = data.frame(studytype=stype, outcome=otype, variables=vars_excluded)
variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
standard_model = glm(y~X.dash)

# tidy estimates (takes a while)
standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
  mutate(term = str_remove(string=term, pattern='X.dash'),
         study_type = stype, outcome=otype)
all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
standard_models[[mtype]] = standard_model # concatenate entire model

## compare enet and standard model estimates ##
enet_beta = data.frame(term=row.names(beta), beta = beta[,1]) %>%
  filter(beta !=0)
row.names(enet_beta) = NULL
compare = full_join(standard_model_ests, enet_beta, by='term') %>%
  filter(term!='(Intercept)') %>% # don't compare intercept
  mutate(diff = beta - estimate,
         av = (beta+estimate)/2)
# Bland-Altman plot (standard estimates are slightly bigger as expected because of shrinkage)
cplot = ggplot(data=compare, aes(x=av, y=diff, label=term))+
  geom_point()+
  xlab('Average')+
  ylab('Difference (elastic net minus standard)')+
  geom_label(size=2)+
  theme_bw()
cplot
outfile = paste('figures/bland_altman_enet_standard_clintrials_', stype, "_", otype, '.jpg', sep='')
jpeg(outfile, width=5, height=4.5, units='in', res=300)
print(cplot)
dev.off()

# check VIF
vif = ols_vif_tol(lm(standard_model))
VIF = vif$VIF
vif$Variables[which(VIF>5)]
cat('Any VIF?' , any(VIF>5), '\n')

# basic numbers
numbers_frame = data.frame(outcome = otype,
                           study_type = stype,
                           n_missing_outcome = n_missing_outcome,
                           n_model = nrow(this_data))
numbers = bind_rows(numbers, numbers_frame)

# residuals
this_data$fitted = fitted(standard_model)
this_data$res = resid(standard_model)
# check mode in residuals at -5
check = filter(this_data, res> -5.1, res < -5)

} # end of study type loop
} # end of outcome loop

## save processed data and mode estimates
which_data = 'clintrials'
save(which_data, numbers, standard_models, all_ests, variables_excluded, colinear, VIF, xval, file='results/clintrials_sample_size.RData')

## save variable list and which were log-transformed
# list of log-transformed variables
logged.vars = c('n_primary', 'n_secondary', 'n_condition', 'n_arms')
vars = unique(vars) %>% # remove duplicates due to target/actual repeat
  mutate(database = 'clintrials.gov',
         categories = '', # prepare
         log = as.numeric(variables %in% logged.vars),
         log = factor(log, levels=0:1, labels=c('No','Yes')))
# add variable categories for character, factor and logical variables
all_character_variables = names(studies[, sapply(studies, class) %in% c('character','factor')])
all_logical_variables = names(studies[, sapply(studies, class) == 'logical'])
character_variables = intersect(all_character_variables, unique(vars$variables)) # just the predictors
logical_variables = intersect(all_logical_variables, unique(vars$variables)) # just the predictors
for (c in character_variables){
  temporary = select(studies, all_of(c)) %>% pull(c)  
  categories = unique(temporary)
  categories = categories[!is.na(categories)] # remove missing
  index = vars$variables == c
  vars$categories[index] = paste(categories, collapse=', ', sep='')
}
for (c in logical_variables){
  temporary = select(studies, all_of(c)) %>% pull(c)  
  index = vars$variables == c
  vars$categories[index] = "Yes, No"
}

# save
save(vars, file='results/var_list_clintrials.RData')
