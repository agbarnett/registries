# 3_model_actual_target_ratio_ANZCTR.R
# multiple regression model of the actual/target ratio in ANZCTR using elastic net
# April 2021
library(dplyr)
library(tidyr)
library(xtable)
library(glmnet)
library(ggplot2)
library(scales)
library(stringr)
library(broom)
library(olsrr) # for VIF

# get data
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
# make ratio of sample sizes
studies = mutate(studies, ratio = samplesize_actual / samplesize_target) %>%
  filter(!is.na(ratio)) # exclude missing

### Section 1: histogram

# summary stats on majority of studies
q90 = summarise(studies, quantiles=quantile(ratio, c(0.1,0.9))) %>%
  mutate(
    q = 90,
    limit=c('lower','upper'))
q50 = summarise(studies, quantiles=quantile(ratio, c(0.25,0.75))) %>%
  mutate(
    q = 50,
    limit=c('lower','upper'))
# data for arrows (but without pointers); go below the x-axis
arr = bind_rows(q50, q90) %>%
  tidyr::spread(limit, quantiles) %>%
  mutate(y = case_when(
    q==90 ~ -70,
    q==50 ~ -240
  ),
  yend = y
  )
# text labels for arrows (high on y-axis)
arr.text = mutate(arr, x=50, label=paste(c('50%','90%'),'of studies')) # at far right
# text label for x-axis
labels1 = data.frame(x=1, y=3330, label='Above target')
labels2 = data.frame(x=1, y=3330, label='Below target')
#
hplot = ggplot(data=studies, aes(x=ratio+0.001))+
  geom_histogram(fill='dodgerblue')+
  geom_vline(lty=2, xintercept=1)+
  scale_y_continuous(labels=comma)+ #
  scale_x_log10(limits=c(1/50,50), 
                breaks=c(1/50, 0.2, 0.5, 1, 2, 5, 50),
                labels=c(1/50, 0.2, 0.5, 1, 2, 5, 50))+
  geom_text(data=labels1, aes(x=x, y=y, label=label), hjust=-0.2, vjust=1.1, col=grey(0.2), size=4)+
  geom_text(data=labels2, aes(x=x, y=y, label=label), hjust=1.2, vjust=1.1, col=grey(0.2), size=4)+
  geom_text(data=arr.text, aes(x=x, y=y, label=label), hjust=1, col='darkseagreen4', size=3.5)+
  geom_segment(data=arr, size=1.7, aes(x = lower, y = y, xend = upper, yend = yend),
               lineend='butt', col='darkseagreen4')+ # block arrows
  xlab('Sample size ratio (actual / target)')+
  ylab('Count')+
  ggtitle('ANZCTR')+
  theme_bw()+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())
hplot
save(hplot, file='figures/ratio_plot_anzctr.RData')

## output ratios to table
to_table = bind_rows(q50, q90) %>%
  pivot_wider(values_from=quantiles, names_from=limit)
print(xtable(to_table), include.rownames=FALSE)
      
### section 2: model ###

source('3_prep_ANZCTR.R') # prepare data for regression model
for.model = mutate(for.model, samplesize_target = log2(samplesize_target)) # log-transform target sample size

#
all_ests = variables_excluded = xval = colinear = NULL
standard_models = list()
for (cutoff in c('1se','min')){ # try both penalties
  set.seed(30991199) # for x-validation
  
  ## make design matrix
  # not included: ethics variables (not clear causal pathway), ccode2 (too detailed)
  # not included status
  vars_to_include = c('samplesize_target','date','gender','age_limit_max','age_limit_min',
                      'control','n_primary','n_secondary','ccode1','n_funding','volunteers','study_status',
                      'allocation','phase','endpoint','purpose','masking','assignment','intervention_code')

  # formula without intercept:
  formula = paste('log(ratio+0.001) ~ -1 +', paste(vars_to_include, collapse = '+'))
  
  # remove missing outcome
  this_data = filter(for.model)
  
  # prepare data for glmnet
  X = model.matrix(as.formula(formula), data=this_data)
  # remove X's with no variance (all the same value)
  variances = apply(X, 2, sd)
  index1 = which(variances==0)
  # had to remove 'all' gender 
  index2= which(colnames(X) == 'genderAll')
  index = union(index1, index2)
  X = X[, !1:ncol(X)%in%as.numeric(index)] # remove from X
  # option to remove colinear - none for these models
  use_colinear = FALSE
  if(use_colinear==TRUE){
  to_remove = filter(colinear, outcome==otype)
  if(nrow(to_remove) > 0){
    drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
    X = X[, !drop]
  }
  }
  
  # dependent variable:
  y = log(this_data$ratio + 0.001) # log-transform, small constant because of few zeros
  
  # run model
  enet_model = glmnet(y=y, x=X, alpha=0.95)
  plot(enet_model)
  
  # x-validation to find best cut-off
  cv_enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
  plot(cv_enet_xval)
  if(cutoff=='1se'){beta = coef(cv_enet_xval, s='lambda.1se')} # 
  if(cutoff=='min'){beta = coef(cv_enet_xval, s='lambda.min')} # 
  # store x-validation results for later plotting:
  cv_ests = tidy(cv_enet_xval) %>%  
    mutate(
      lambda.1se = cv_enet_xval$lambda.1se, # add lambda min and 1SE
      lambda.min = cv_enet_xval$lambda.min)
  xval = bind_rows(xval, cv_ests)
  
  ## re-run standard model for best coefficients with confidence intervals ##
  update_vars_to_include = row.names(beta)[which(beta!=0)]
  if(sum(colnames(X) %in% update_vars_to_include) > 1){ # will be a matrix
    X.dash = X[, colnames(X) %in% update_vars_to_include]
  }
  if(sum(colnames(X) %in% update_vars_to_include) == 1){ #force into a matrix
    X.dash = as.matrix(X[, colnames(X) %in% update_vars_to_include])
    colnames(X.dash) = update_vars_to_include[2]
  }
  vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
  vars_excluded = data.frame(lambda = cutoff,
                             variables = vars_excluded)
  variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
  if(length(update_vars_to_include)>0){ # in any variables remaining
    standard_model = glm(y~X.dash)
    # tidy estimates
    standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
      mutate(term = str_remove(string=term, pattern='X.dash'),
             lambda = cutoff) # include penalty cut-off
    # horrible work around for single variable model
    if(sum(colnames(X) %in% update_vars_to_include) == 1){standard_model_ests$term[2] = update_vars_to_include[2]}
    #
    all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
  }
  if(length(update_vars_to_include)==0){ # in no variables remaining
    standard_model = NULL
  }
  standard_models[[cutoff]] = standard_model # concatenate entire model
  
  # check VIF
  if(is.null(standard_model) == FALSE & sum(colnames(X) %in% update_vars_to_include) > 1){ # must be more than one variable
    vif = ols_vif_tol(lm(standard_model))
    VIF = vif$VIF
    vif$Variables[which(VIF>5)]
    cat('Any VIF?' , any(VIF>5), '\n')
  }
  
}

## store estimates
save(all_ests, xval, standard_models, variables_excluded, file='results/sample_size_ratio.RData')
