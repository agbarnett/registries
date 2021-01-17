# 2_elasticnet_model_samplesize_clintrial.R
# elastic net model for predicting target sample size
# version for clinicaltrials.gov data
# used by 2_clintrials_summary.Rmd
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
')
#Interventional,target,allocationNot Applicable,?
#  Interventional,target,age_limit_minExactly 18,?

# get the data
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 

## Prepare data for regression model
# change missing to a category
for.model = mutate(studies,
    date = as.numeric(posted - as.Date('2010-01-01')) / (5*365.25), # standardised to five years
    # horribly skewed, so log (base 2)
    n_primary = log2(n_primary+1),
    n_secondary = log2(n_secondary+1),
    n_condition = log2(n_condition),
    n_arms = log2(n_arms+1), # the least skewed
    # max age
    age_limit_max = case_when(
      age_max_type == 'No limit' ~ 'No limit',
      age_max_type == 'Restricted' & age_max <18  ~ 'Under 18',
      age_max_type == 'Restricted' & age_max ==18  ~ 'Exactly 18',
      age_max_type == 'Restricted' & age_max >18  ~ 'Over 18'
    ),
    # min age
    age_limit_min = case_when(
      age_min_type == 'No limit' ~ 'No limit',
      age_min_type == 'Restricted' & age_min <18  ~ 'Under 18',
      age_min_type == 'Restricted' & age_min ==18  ~ 'Exactly 18',
      age_min_type == 'Restricted' & age_min >18  ~ 'Over 18'
    ),
    # add categories for missing
    gender = ifelse(is.na(gender), 'Missing', gender),
    volunteers = ifelse(is.na(volunteers), 'Missing', volunteers),
    masking = ifelse(is.na(masking), 'Missing', masking),
    purpose = ifelse(is.na(purpose), 'Missing', purpose),
    allocation = ifelse(is.na(allocation), 'Missing', allocation),
    assignment = ifelse(is.na(assignment), 'Missing', assignment),
    lead_sponsor_class = ifelse(is.na(lead_sponsor_class), 'Missing', lead_sponsor_class),
    phase = ifelse(is.na(phase), 'Missing', phase)
  ) %>%
  #filter(!is.na(age_limit_max),
  #       !is.na(age_limit_min)) %>% # exclude few missing new age variable - not needed
  # set reference levels for categorical variables
  mutate(
    lead_sponsor_class = relevel(factor(lead_sponsor_class), ref='Other'),
    purpose = relevel(factor(purpose), ref='Treatment'),
    masking = relevel(factor(masking), ref='None'),
    phase = relevel(factor(phase), ref='Phase 3'),
    assignment = relevel(factor(assignment), ref='Parallel'), # 
    allocation = relevel(factor(allocation), ref='Randomized'), # 
    gender = relevel(factor(gender), ref='All'), # 
    volunteer = relevel(factor(volunteer), ref='No'), # 
    status = case_when( # combine two small categories
      status == 'No longer available' ~ 'Not available',
      status == 'Temporarily not available' ~ 'Not available',
      TRUE ~ as.character(status)
    ),
    status = relevel(factor(status), ref='Completed'),
    age_limit_max = relevel(factor(age_limit_max), ref='No limit'),
    age_limit_min = relevel(factor(age_limit_min), ref='No limit')) %>%
  select(-age_min_type, -age_min, -age_max_type, -age_max)

  
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
# formula without intercept:
vars = list()    
vars_to_include = c('date','gender','age_limit_max','age_limit_min','volunteers','purpose','masking',
                    'phase','assignment','allocation',
                    'behavioral','biological','combination','device','diagnostic','dietary','drug','genetic','procedure','radiation','other',
                    'n_primary','n_secondary','n_condition','n_arms','lead_sponsor_class')
if(stype == 'Interventional'){
  vars_to_include = c(vars_to_include, 'phase') # add trial details ****
  vars[['Interventional']] = vars_to_include # for variable summary table
}
if(stype == 'Observational'){
  vars_to_include = c(vars_to_include, 'study_design_observational','study_design_time') # add observational details
  vars[['Observational']] = vars_to_include # for variable summary table
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
this_data = filter(this_data, !is.na(sample_size)) # remove missing

# prepare data for glmnet
X = model.matrix(as.formula(formula), data=this_data)
# remove X's with no variance (all the same value)
variances = apply(X, 2, sd)
index = which(variances==0)
X = X[, !1:ncol(X)%in%as.numeric(index)] # remove from X
# option to remove colinear
to_remove = filter(colinear, stype==stype, outcome==otype)
if(nrow(to_remove) > 0){
  drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
  X = X[, !drop]
}

# dependent variable:
y = log(this_data$sample_size+1) # could try other logs; +1 due to zero

# run model
enet_model = glmnet(y=y, x=X, alpha=1)
plot(enet_model, main=paste(stype, ', ', otype, sep=''))

# x-validation to find best cut-off
enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
plot(enet_xval, main=paste(stype, ', ', otype, sep=''))
beta = coef(enet_xval, s='lambda.1se')

## re-run standard model for best coefficients with confidence intervals ##
# use sum to zero constraint to remove reference groups #
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
vars_excluded = data.frame(studytype=stype, outcome=otype, variables=vars_excluded)
variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
standard_model = glm(y~X.dash)

# tidy estimates
standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
  mutate(term = str_remove(string=term, pattern='X.dash'),
         study_type = stype, outcome=otype)
all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
standard_models[[stype]] = standard_model # concatenate entire model

## compare lasso and standard model estimates ##
lasso_beta = data.frame(term=row.names(beta), beta = beta[,1]) %>%
  filter(beta !=0)
row.names(lasso_beta) = NULL
compare = full_join(standard_model_ests, lasso_beta, by='term') %>%
  mutate(diff = beta - estimate,
         av = (beta+estimate)/2)
# Bland-Altman plot (standard estimates are slightly bigger as expected because of shrinkage)
cplot = ggplot(data=compare, aes(x=av, y=diff, label=term))+
  geom_point()+
  xlab('Average')+
  ylab('Difference (lasso minus standard)')+
  geom_label(size=2)+
  theme_bw()
cplot
outfile = paste('figures/bland_altman_lasso_standard_clintrials_', stype, '.jpg', sep='')
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
save(which_data, numbers, standard_models, all_ests, variables_excluded, colinear, VIF, file='results/clintrials_sample_size.RData')

## save variable list and which were log-transformed
# add variable categories
save(vars, file='results/var_list_clintrials.RData')