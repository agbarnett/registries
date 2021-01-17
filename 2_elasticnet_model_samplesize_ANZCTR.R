# 2_elasticnet_model_samplesize_ANZCTR.R
# elastic net model for predicting target sample size
# version for ANZCTR data
# used by 2_anzctr_summary.Rmd
# December 2020
library(dplyr)
library(ggplot2)
library(glmnet)
library(stringr)
library(vctrs)
library(olsrr) # for VIF

## colinear variables from trial and error
colinear = read.table(header=TRUE, sep=',', stringsAsFactors=FALSE, text='
stype,outcome,variable,colinear_with
Interventional,target,phaseNot Applicable,missing masking
Interventional,target,age_limit_minExactly 18,age_limit_min Over 18
Interventional,actual,age_limit_minExactly 18,age_limit_min Over 18
')

# get the registry data
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 

## Prepare data for regression model
# change missing to a category
for.model = filter(studies, 
                   !is.na(gender),  # just 4 missing gender
                   !is.na(submitted),  # just 3 missing date
                   !is.na(ccode1)) %>% # just 8 missing
  mutate(
    date = as.numeric(submitted - as.Date('2010-01-01')) / (5*365.25), # standardised to five years
    # secondary outcomes is horribly skewed, so log
    n_secondary = log2(n_secondary+1),
    n_primary = log2(n_primary+1),
    ## make combined age limit variable
    # first change missing to not stated
    age_min_type = ifelse(is.na(age_min_type), 'Not stated', age_min_type),
    age_max_type= ifelse(is.na(age_max_type), 'Not stated', age_max_type),
    # max age
    age_limit_max = case_when(
      age_max_type == 'No limit' ~ 'No limit',
      age_max_type == 'Not stated' ~ 'Not stated',
      age_max_type == 'Restricted' & age_max <18  ~ 'Under 18',
      age_max_type == 'Restricted' & age_max ==18  ~ 'Exactly 18',
      age_max_type == 'Restricted' & age_max >18  ~ 'Over 18'
    ),
    # min age
    age_limit_min = case_when(
      age_min_type == 'No limit' ~ 'No limit',
      age_min_type == 'Not stated' ~ 'Not stated',
      age_min_type == 'Restricted' & age_min <18  ~ 'Under 18',
      age_min_type == 'Restricted' & age_min ==18  ~ 'Exactly 18',
      age_min_type == 'Restricted' & age_min >18  ~ 'Over 18'
    ),
    # lastly, if missing then add category
    age_limit_min = ifelse(is.na(age_limit_min), 'Missing', age_limit_min),
    age_limit_max = ifelse(is.na(age_limit_max), 'Missing', age_limit_max),
    # add categories for missing
    assignment = ifelse(is.na(assignment), 'Missing', assignment),
    masking = ifelse(is.na(masking), 'Missing', masking),
    purpose = ifelse(is.na(purpose), 'Missing', purpose),
    intervention_code = ifelse(is.na(intervention_code), 'Missing', intervention_code),
    control = ifelse(is.na(control), 'Missing', control),
    phase = ifelse(is.na(phase), 'Missing', phase),
    endpoint = ifelse(is.na(endpoint), 'Missing', endpoint)
  ) %>%
  filter(!is.na(age_limit_max),
         !is.na(age_limit_min)) %>% # exclude few missing new age variable
  # set reference levels for categorical variables
  mutate(purpose = relevel(factor(purpose), ref='Treatment'),
         assignment = relevel(factor(assignment), ref='Parallel'),
         masking = relevel(factor(masking), ref='Blinded (masking used)'),
         intervention_code = relevel(factor(intervention_code), ref='Treatment: Drugs'),
    control = relevel(factor(control), ref='Active'),
    phase = relevel(factor(phase), ref='Phase 3'),
    endpoint = relevel(factor(endpoint), ref='Efficacy'),
    gender = relevel(factor(gender), ref='All'),
    provisional = relevel(provisional, ref='No'),
    study_status = relevel(factor(study_status), ref='Completed'),
    age_limit_min = relevel(factor(age_limit_min), ref='No limit'),
    age_limit_max = relevel(factor(age_limit_max), ref='No limit'),
    ccode1 = relevel(factor(ccode1), ref='Cardiovascular'))
## merge from funding data?
# simpler funder
#simple_funder = case_when(
#  funding=='ARC' ~ 1,
#  funding=='NHMRC' ~ 2,
#  TRUE ~ 3,
#),
#simple_funder = factor(simple_funder, levels=1:3, labels=c('ARC','NHMRC','Other'))

## split by: 1) observational or not, 2) actual or target
all_ests = variables_excluded = numbers = NULL
standard_models = list()
types = c('Observational','Interventional')
otypes = c('target','actual') # outcome (target or actual sample size)
for (otype in otypes){
for (stype in types){
  cat(otype, ', ', stype,'\n', sep='')
  set.seed(30991199) # for x-validation
  
  ## make design matrix
  # not included ethics variables (not clear causal pathway), ccode2 (too detailed)
  vars_to_include = c('date','gender','age_limit_max','age_limit_min','purpose','masking','assignment','intervention_code',
                      'control','phase','endpoint','n_primary','n_secondary','ccode1','n_funding','provisional')
  if(otype == 'actual'){vars_to_include = c(vars_to_include, 'study_status')}
  # formula without intercept:
  formula = paste('log(samplesize_target) ~ -1 +', paste(vars_to_include, collapse = '+'))
  
  # just one study type (observational or interventional); and remove missing outcome
  this_data = filter(for.model, study_type == stype)
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
to_remove = filter(colinear, stype==stype, outcome==otype)
if(nrow(to_remove) > 0){
  drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
  X = X[, !drop]
}

# dependent variable:
if(otype=='target'){y = log(this_data$samplesize_target)} # log-transform, no zeros
if(otype=='actual'){y = log(this_data$samplesize_actual+1)} 

# run model
enet_model = glmnet(y=y, x=X, alpha=0.95)
plot(enet_model, main=paste(stype, ', ', otype, sep=''))

# x-validation to find best cut-off
enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
plot(enet_xval, main=paste(stype, ', ', otype, sep=''))
beta = coef(enet_xval, s='lambda.1se')

## re-run standard model for best coefficients with confidence intervals ##
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
vars_excluded = data.frame(study_type=stype, outcome=otype, variables=vars_excluded)
variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
if(ncol(X.dash)>0){ # in any variables remaining
standard_model = glm(y~X.dash)
# tidy estimates
standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
  mutate(term = str_remove(string=term, pattern='X.dash'),
         outcome = otype, # add outcome
         study_type = stype) # add model type
all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
}
if(ncol(X.dash)==0){ # in no variables remaining
  standard_model = NULL
}
model_name = paste(stype, otype, sep=', ')# 
standard_models[[model_name]] = standard_model # concatenate entire model

## compare lasso and standard model estimates ##
lasso_beta = data.frame(term=row.names(beta), beta = beta[,1]) %>%
  filter(beta !=0)
row.names(lasso_beta) = NULL
compare = full_join(standard_model_ests, lasso_beta, by='term') %>%
  mutate(diff = beta - estimate,
         av = (beta+estimate)/2) %>%
  filter(term !='(Intercept)') # don't need to see intercept
# Bland-Altman plot (standard estimates are slightly bigger as expected because of shrinkage)
cplot = ggplot(data=compare, aes(x=av, y=diff, label=term))+
  geom_point()+
  ggtitle(stype)+
  xlab('Average')+
  ylab('Difference (lasso minus standard)')+
  geom_label(size=2)+
  theme_bw()
outfile = paste('figures/bland_altman_lasso_standard_clintrials_', stype, '.jpg', sep='')
jpeg(outfile, width=5, height=4.5, units='in', res=300)
print(cplot)
dev.off()

# basic numbers
numbers_frame = data.frame(outcome = otype,
                           study_type = stype,
                           n_missing_outcome = n_missing_outcome,
                           n_model = nrow(this_data))
numbers = bind_rows(numbers, numbers_frame)

# check VIF
if(is.null(standard_model) == FALSE){
  vif = ols_vif_tol(lm(standard_model))
  VIF = vif$VIF
  vif$Variables[which(VIF>5)]
  cat('Any VIF?' , any(VIF>5), '\n')
}

} # end of study type loop
} # end of outcome type loop

# save processed data and mode estimates
which_data = 'anzctr'
save(which_data, numbers, standard_models, all_ests, variables_excluded, vif, colinear, file='results/ANZCTR_sample_size.RData')

# compare interventional/observational estimates
to_plot = filter(all_ests, 
                 !str_detect(string=term, pattern='Intercept')) # remove intercept from plot
compare_plot = ggplot(to_plot, aes(x=term, y=estimate, col=factor(study_type)))+
  geom_point()+
  xlab('')+
  coord_flip()+
  theme(legend.position = 'top')
compare_plot
