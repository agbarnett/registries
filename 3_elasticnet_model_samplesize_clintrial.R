# 3_elasticnet_model_samplesize_clintrial.R
# elastic net model for predicting target sample size
# version for clinicaltrials.gov data
# used by 4_clintrials_summary.Rmd
# April 2021
library(dplyr)
library(ggplot2)
library(glmnet)
library(broom)
library(stringr)
library(vctrs)
library(olsrr) # for VIF
library(selectiveInference) # for CIs

## co-linear variables from trial and error
colinear = read.table(header=TRUE, sep=',', stringsAsFactors=FALSE, text='
outcome,variable,colinear_with
target,allocationN/A,assignmentSingle Group
')
#Interventional,actual,age_limit_minExactly 18,age_limit_minOver 18 

# get the data and process ready for the model
load('data/clinicaltrials_analysis_plus.RData') # from 2a_update_clintrials_data.R 
source('3_prep_clintrials.R')

## sensitivity analysis based on post 2006 data only (far fewer missing)
#for.model = filter(for.model, submitted >= as.Date('2007-01-01'))

## split by: 1) actual or target
all_ests = variables_excluded = numbers = vars = xval = NULL
standard_models = list()
otypes = c('target','actual') # outcome (target or actual sample size)
for (otype in otypes){ # loop through model types
    mtype = paste(otype) # model type
    cat(otype,'\n', sep='')
    set.seed(30991199) # for x-validation
    
    ## make design matrix
    # formula without intercept:
    vars_to_include = c('date','gender','age_limit_max','age_limit_min','volunteers',
                        'behavioral','biological','combination','device','diagnostic','dietary','drug','genetic','procedure','radiation','other',
                        'n_primary','n_secondary','n_condition','n_arms','lead_sponsor_class', 'longitudinal', 'phase','purpose','masking',
                        'assignment','allocation','adaptive_trial')
    if(otype == 'actual'){vars_to_include = c(vars_to_include, 'status')} # add status for completed studies
    this_vars = data.frame(variables = vars_to_include)
    vars = bind_rows(vars, this_vars) # for variable summary table
    
    names(for.model)[names(for.model) %in% vars_to_include==FALSE] # check what's not included
    vars_to_include[vars_to_include %in% names(for.model) ==FALSE] # should be empty
    formula = paste('log(samplesize_',otype,') ~ -1 +', paste(vars_to_include, collapse = '+'), sep='')
    
    # remove missing outcome
    if(otype=='target'){
      n_missing_outcome = sum(is.na(for.model$samplesize_target))
      this_data = filter(for.model, 
                         !is.na(samplesize_target),
                         samplesize_target > 0) # 3 studies with zero target sample size
    }
    if(otype=='actual'){
      n_missing_outcome = sum(is.na(for.model$samplesize_actual))
      this_data = filter(for.model, !is.na(samplesize_actual))
    }
    
    # prepare data for glmnet
    X = model.matrix(as.formula(formula), data=this_data)
    # remove X's with no variance (all the same value)
    variances = apply(X, 2, sd)
    index = which(variances==0)
    X = X[, !1:ncol(X)%in%as.numeric(index)] # remove from X
    # option to remove co-linear variables
    to_remove = filter(colinear, outcome==otype)
    if(nrow(to_remove) > 0){
      drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
      X = X[, !drop]
    }
    
    # dependent variable:
    if(otype=='target'){y = log(this_data$samplesize_target)} # log-transform, no zeros
    if(otype=='actual'){y = log(this_data$samplesize_actual+1)} 
    
    # run model
    enet_model = glmnet(y=y, x=X, alpha=0.95)
    plot(enet_model, main=otype)
    
    # x-validation to find best cut-off
    cv_enet_model = cv.glmnet(y=y, x=X, alpha=0.95) # elastic net penalty
    plot(cv_enet_model, main=otype)
    beta = coef(cv_enet_model, s='lambda.1se')
    # store x-validation results for later plotting:
    cv_ests = tidy(cv_enet_model) %>%  
      mutate(
        lambda.1se = cv_enet_model$lambda.1se, # add lambda min and 1SE
        lambda.min = cv_enet_model$lambda.min,
        outcome = otype) # add outcome
    xval = bind_rows(xval, cv_ests)
    
    ## re-run standard model for best coefficients with confidence intervals ##
    # use sum to zero constraint to remove reference groups #
    update_vars_to_include = row.names(beta)[which(beta!=0)]
    X.dash = X[, colnames(X) %in% update_vars_to_include]
    vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
    vars_excluded = data.frame(outcome=otype, variables=vars_excluded)
    variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
    standard_model = glm(y~X.dash)
    
    # tidy estimates (takes a while)
    standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
      mutate(term = str_remove(string=term, pattern='X.dash'),
             outcome=otype)
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
    outfile = paste('figures/bland_altman_enet_standard_clintrials_', otype, '.jpg', sep='')
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
                               n_missing_outcome = n_missing_outcome,
                               n_model = nrow(this_data))
    numbers = bind_rows(numbers, numbers_frame)
    
    # residuals
    this_data$fitted = fitted(standard_model)
    this_data$res = resid(standard_model)
    # check mode in residuals at -5
    check = filter(this_data, res> -5.1, res < -5)
    
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
