# 2_elasticnet_model_samplesize_clintrial.R
# elastic net model for predicting target sample size
# version for clinicaltrials.gov data
# used by 2_clintrials_summary.Rmd
# June 2020
library(dplyr)
library(ggplot2)
library(glmnet)
library(stringr)
library(vctrs)
library(olsrr) # for VIF
set.seed(30991199)

# get the data
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 

## Prepare data for regression model
# change missing to a category
for.model = filter(studies, 
                   !is.na(gender),  # just 4 missing gender
                   !is.na(submitted),  # just 2 missing date
                   !is.na(ccode1), # just 7 missing
                   samplesize_target > 0) %>% # exclude small number with 0 sample size
  mutate(
    date = as.numeric(submitted - as.Date('2010-01-01')) / (5*365.25), # standardised to five years
    # secondary outcomes is horribly skewed, so log
    n_secondary = log2(n_secondary+1),
    # make combined age limit variable
    age_limit = case_when(
      age_max_type == 'No limit' ~ 'No limit',
      age_max_type == 'Not stated' ~ 'Not stated',
      age_max_type == 'Restricted' & age_max <18  ~ 'Under 18',
      age_max_type == 'Restricted' & age_max ==18  ~ 'Exactly 18',
      age_max_type == 'Restricted' & age_max >18  ~ 'Over 18',
    ),
    # add categories for missing
    masking = ifelse(is.na(masking), 'Missing', masking),
    purpose = ifelse(is.na(purpose), 'Missing', purpose),
    intervention_code = ifelse(is.na(intervention_code), 'Missing', intervention_code),
    control = ifelse(is.na(control), 'Missing', control),
    phase = ifelse(is.na(phase), 'Missing', phase),
    endpoint = ifelse(is.na(endpoint), 'Missing', endpoint)
  ) %>%
  filter(!is.na(age_limit)) %>% # cross out few missing new age variable
  # set reference levels for categorical variables
  mutate(studytype = relevel(factor(studytype), ref='Interventional'),
         purpose = relevel(factor(purpose), ref='Treatment'),
         masking = relevel(factor(masking), ref='Blinded (masking used)'),
         intervention_code = relevel(factor(intervention_code), ref='Treatment: Drugs'),
    control = relevel(factor(control), ref='Active'),
    phase = relevel(factor(phase), ref='Phase 3'),
    endpoint = relevel(factor(endpoint), ref='Efficacy'),
    gender = relevel(factor(gender), ref='Both males and females'),
    age_limit = relevel(factor(age_limit), ref='No limit'),
    ccode1 = relevel(factor(ccode1), ref='Cardiovascular'))
## to do, merge from funding data
# simpler funder
#simple_funder = case_when(
#  funding=='ARC' ~ 1,
#  funding=='NHMRC' ~ 2,
#  TRUE ~ 3,
#),
#simple_funder = factor(simple_funder, levels=1:3, labels=c('ARC','NHMRC','Other'))

## make design matrix
# formula without intercept:
vars_to_include = c('date','gender','age_limit','studytype','purpose','masking','intervention_code',
                    'control','phase','endpoint','n_primary','n_secondary','ccode1','n_funding')
formula = paste('log(samplesize_target) ~ -1 +', paste(vars_to_include, collapse = '+'))
# prepare data for glmnet
X = model.matrix(as.formula(formula), data=for.model)
# remove not applicable for phase and intervention code, co-linear with observational 
drop = colnames(X) %in% c('intervention_codeNot applicable','phaseNot Applicable')  # because of high VIF; NA phase is perfectly correlated with Observational study
X = X[, !drop]
# dependent variable:
y = log(for.model$samplesize_target) # could try other logs

# run model
enet_model = glmnet(y=y, x=X, alpha=0.95)
plot(enet_model)

# x-validation to find best cut-off
enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
plot(enet_xval)
beta = coef(enet_xval, s='lambda.1se')

## re-run standard model for best coefficients with confidence intervals ##
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
variables_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
standard_model = glm(y~X.dash)
# tidy
standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
  mutate(term = str_remove(string=term, pattern='X.dash'))

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

# check VIF
vif = ols_vif_tol(lm(standard_model))
VIF = vif$VIF
vif$Variables[which(VIF>5)]

# save estimates
save(standard_model, standard_model_ests, variables_excluded, VIF, file='results/ANZCTR_sample_size.RData')
