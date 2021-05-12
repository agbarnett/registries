# 3_elasticnet_model_samplesize_ANZCTR.R
# elastic net model for predicting target sample size
# version for ANZCTR data
# used by 4_anzctr_summary.Rmd
# April 2021
library(dplyr)
library(ggplot2)
library(glmnet)
library(broom)
library(stringr)
library(vctrs)
library(olsrr) # for VIF
library(selectiveInference) # for CIs

## colinear variables from trial and error
colinear = read.table(header=TRUE, sep=',', stringsAsFactors=FALSE, text='
outcome,variable,colinear_with
target,phaseNot Applicable,missing masking
target,age_limit_minExactly 18,age_limit_min Over 18
actual,phaseNot Applicable,missing masking
actual,age_limit_minExactly 18,age_limit_min Over 18
')

# get the registry data
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
source('3_prep_ANZCTR.R') # prepare data for regression model

## split by: 1) actual or target
all_ests = variables_excluded = numbers = vars = xval = NULL
standard_models = list()
otypes = c('target','actual') # outcome (target or actual sample size)
for (otype in otypes){
  cat(otype, '\n', sep='')
  mtype = paste(otype) # combined
  set.seed(30991199) # for x-validation
  
  ## make design matrix
  # not included: ethics variables (not clear causal pathway), ccode2 (too detailed)
  vars_to_include = c('date','gender','age_limit_max','age_limit_min',
                      'control','n_primary','n_secondary','ccode1','n_funding','volunteers',
                      'allocation','phase','endpoint','purpose','masking','assignment','intervention_code')
  if(otype == 'actual'){vars_to_include = c(vars_to_include, 'study_status')} # just for actual sample size
  vars_to_include = c(vars_to_include) # add variables for interventional studies
  this_vars = data.frame(variables = vars_to_include)
  vars = bind_rows(vars, this_vars) # for variable summary table

  
  # formula without intercept:
  formula = paste('log(samplesize_target) ~ -1 +', paste(vars_to_include, collapse = '+'))
  
  # remove missing outcome
  this_data = for.model
  if(otype=='target'){
    n_missing_outcome = sum(is.na(this_data$samplesize_target))
    this_data = filter(this_data, 
                       !is.na(samplesize_target),
                       samplesize_target > 0) # 3 studies with zero target sample size
  }
  if(otype=='actual'){
    n_missing_outcome = sum(is.na(this_data$samplesize_actual))
    this_data = filter(this_data, !is.na(samplesize_actual))
  }
  
  # prepare data for glmnet
X = model.matrix(as.formula(formula), data=this_data)

# option to remove colinear
to_remove = filter(colinear, outcome==otype)
if(nrow(to_remove) > 0){
  drop = colnames(X) %in% to_remove$variable  # because of high VIF
  X = X[, !drop]
}

# dependent variable:
if(otype=='target'){y = log(this_data$samplesize_target)} # log-transform, no zeros
if(otype=='actual'){y = log(this_data$samplesize_actual+1)} 

# run model
enet_model = glmnet(y=y, x=X, alpha=0.95) #
plot(lasso_model, main=otype)

# x-validation to find best cut-off
cv_enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
plot(cv_enet_xval, main=otype)
# store x-validation results for later plotting:
cv_ests = tidy(cv_enet_xval) %>%  
  mutate(
    lambda.1se = cv_enet_xval$lambda.1se, # add lambda min and 1SE
    lambda.min = cv_enet_xval$lambda.min,
    outcome = otype) # add outcome
xval = bind_rows(xval, cv_ests)

## get confidence intervals using lasso - not converging so abandoned
ci_lasso = function(){
  n = nrow(X)
  lambda = cv_lasso_xval$lambda.1se # using alpha = 1
  beta  = coef(lasso_model, x=X, y=y, s=lambda/n, exact=TRUE)[-1] # drop intercept
  out = fixedLassoInf(x=X, y=y, lambda=lambda, beta=beta)
}


# basic numbers
numbers_frame = data.frame(outcome = otype,
                           n_missing_outcome = n_missing_outcome,
                           n_model = nrow(this_data))
numbers = bind_rows(numbers, numbers_frame)

## re-run standard model ##
beta  = coef(enet_model, x=X, y=y, s=cv_enet_xval$lambda.1se)
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
vars_excluded = data.frame(outcome=otype, variables=vars_excluded)
variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
if(ncol(X.dash)>0){ # in any variables remaining
  standard_model = glm(y~X.dash)
  # tidy estimates
  standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
    mutate(term = str_remove(string=term, pattern='X.dash'),
           outcome = otype) # add outcome
  all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
}
if(ncol(X.dash)==0){ # in no variables remaining
  standard_model = NULL
}
# check VIF
if(is.null(standard_model) == FALSE){
  vif = ols_vif_tol(lm(standard_model))
  VIF = vif$VIF
  vif$Variables[which(VIF>5)]
  cat('Any VIF?' , any(VIF>5), '\n')
}

} # end of outcome type loop

# save processed data and mode estimates
which_data = 'anzctr'
save(which_data, numbers, standard_models, all_ests, variables_excluded, vif, colinear, xval, file='results/ANZCTR_sample_size.RData')

## save variable list and which were log-transformed
# list of log-transformed variables
logged.vars = c('n_primary', 'n_secondary', 'n_condition', 'n_arms')
vars = unique(vars) %>% # remove duplicates due to target/actual repeat
  mutate(database = 'ANZCTR',
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
save(vars, file='results/var_list_anzctr.RData')
