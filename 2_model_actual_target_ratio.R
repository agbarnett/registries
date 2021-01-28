# 2_model_actual_target_ratio.R
# multiple regression model of the actual/target ratio in ANZCTR using elastic net
# January 2021
library(dplyr)
library(glmnet)
library(ggplot2)
library(stringr)
library(broom)
library(olsrr) # for VIF
# for PLOS
library(extrafont)
loadfonts(device = "win")

# get data
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
# make ratio of sample sizes
studies = mutate(studies, ratio = samplesize_actual / samplesize_target) %>%
  filter(!is.na(ratio)) # exclude missing

### Section 1: plot

# summary stats on majority of studies
q90 = group_by(studies, study_type) %>%
  summarise(quantiles=quantile(ratio, c(0.1,0.9))) %>%
  mutate(
    q = 90,
    limit=c('lower','upper'))
q50 = group_by(studies, study_type) %>%
  summarise(quantiles=quantile(ratio, c(0.25,0.75))) %>%
  mutate(
    q = 50,
    limit=c('lower','upper'))
# data for arrows (but without pointers)
arr = bind_rows(q50, q90) %>%
  tidyr::spread(limit, quantiles) %>%
  mutate(y = case_when(
    study_type == 'Interventional' & q==90 ~ 1400,
    study_type == 'Interventional' & q==50 ~ 1000,
    study_type == 'Observational' & q==90 ~ 190,
    study_type == 'Observational' & q==50 ~ 130
  ),
  yend = y
  )
# text labels for arrows
arr.text = mutate(arr, x=lower+(upper/2), label=paste(c('50%','90%'),'of studies'))
# text label for x-axis
labels1 = data.frame(x=1, y=0, label='Above target', study_type='Interventional')
labels2 = data.frame(x=1, y=0, label='Below target', study_type='Interventional')
#
hplot = ggplot(data=studies, aes(x=ratio+0.001, fill=factor(study_type)))+
  geom_histogram()+
  scale_fill_manual(NULL, values=c('goldenrod1','dodgerblue'))+
  geom_vline(lty=2, xintercept=1)+
  scale_x_log10(limits=c(1/50,50), 
                breaks=c(1/50, 0.2, 0.5, 1, 2, 5, 50),
                labels=c(1/50, 0.2, 0.5, 1, 2, 5, 50))+
  geom_text(data=labels1, aes(x=x, y=y, label=label), hjust=-0.1, vjust=1.1, col=grey(0.2))+
  geom_text(data=labels2, aes(x=x, y=y, label=label), hjust=1.1, vjust=1.1, col=grey(0.2))+
  geom_label(data=arr.text, aes(x=x, y=y, label=label), vjust=-0.3, fill='white', col='darkorchid3')+
  geom_segment(data=arr, size=1.2, aes(x = lower, y = y, xend = upper, yend = yend),
               lineend='butt', col='darkorchid3')+
  xlab('Sample size ratio (actual / target)')+
  ylab('Count')+
  theme_bw()+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())+
  facet_wrap(~study_type, scale='free_y')
hplot
jpeg('figures/histogram_sample_size_ratio.jpg', width=6, height = 5, units='in', res=300, quality=100)
print(hplot)
dev.off()
# for plos
tiff('figures/Fig6.tif', width=6, height=4.5, units='in', res=300, family='Times New Roman', compression = 'lzw') # PLOS
print(hplot)
invisible(dev.off())


### section 2: model ###

source('2_prep_ANZCTR.R') # prepare data for regression model
for.model = mutate(for.model, samplesize_target = log2(samplesize_target)) # log-transform target sample size

## split by observational or not
all_ests = variables_excluded = xval = colinear = NULL
standard_models = list()
types = c('Observational','Interventional')
for (cutoff in c('1se','min')){ # try both penalties
for (stype in types){
  cat(stype,'\n', sep='')
  set.seed(30991199) # for x-validation
  
  ## make design matrix
  # not included: ethics variables (not clear causal pathway), ccode2 (too detailed)
  # not included status
  vars_to_include = c('samplesize_target','date','gender','age_limit_max','age_limit_min',
                      'control','endpoint','n_primary','n_secondary','ccode1','n_funding','provisional','study_status')
  if(stype == 'Interventional'){
    vars_to_include = c(vars_to_include, 'phase','purpose','masking','assignment','intervention_code') # add trial details ****
  }
  if(stype == 'Observational'){
    this_vars = data.frame(stype = 'Observational', variables = vars_to_include) # no additional variables for observational data
  } 
  
  # formula without intercept:
  formula = paste('log(ratio+0.001) ~ -1 +', paste(vars_to_include, collapse = '+'))
  
  # just one study type (observational or interventional); and remove missing outcome
  this_data = filter(for.model, study_type == stype)
  
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
  to_remove = filter(colinear, stype==stype, outcome==otype)
  if(nrow(to_remove) > 0){
    drop = colnames(X) %in% to_remove$variable  # because of high VIF; NA phase is perfectly correlated with Observational study
    X = X[, !drop]
  }
  }
  
  # dependent variable:
  y = log(this_data$ratio + 0.001) # log-transform, small constant because of few zeros
  
  # run model
  enet_model = glmnet(y=y, x=X, alpha=0.95)
  plot(enet_model, main=stype)
  
  # x-validation to find best cut-off
  cv_enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
  plot(cv_enet_xval, main=stype)
  if(cutoff=='1se'){beta = coef(cv_enet_xval, s='lambda.1se')} # 
  if(cutoff=='min'){beta = coef(cv_enet_xval, s='lambda.min')} # 
  # store x-validation results for later plotting:
  cv_ests = tidy(cv_enet_xval) %>%  
    mutate(
      lambda.1se = cv_enet_xval$lambda.1se, # add lambda min and 1SE
      lambda.min = cv_enet_xval$lambda.min,
      study_type = stype) # add model type
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
  vars_excluded = data.frame(study_type = stype, 
                             lambda = cutoff,
                             variables = vars_excluded)
  variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
  if(length(update_vars_to_include)>0){ # in any variables remaining
    standard_model = glm(y~X.dash)
    # tidy estimates
    standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
      mutate(term = str_remove(string=term, pattern='X.dash'),
             lambda = cutoff, # include penalty cut-off
             study_type = stype) # add model type
    # horrible work around for single variable model
    if(sum(colnames(X) %in% update_vars_to_include) == 1){standard_model_ests$term[2] = update_vars_to_include[2]}
    #
    all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
  }
  if(length(update_vars_to_include)==0){ # in no variables remaining
    standard_model = NULL
  }
  standard_models[[stype]] = standard_model # concatenate entire model
  
  # check VIF
  if(is.null(standard_model) == FALSE & sum(colnames(X) %in% update_vars_to_include) > 1){ # must be more than one variable
    vif = ols_vif_tol(lm(standard_model))
    VIF = vif$VIF
    vif$Variables[which(VIF>5)]
    cat('Any VIF?' , any(VIF>5), '\n')
  }
  
}
}

## store estimates
save(all_ests, xval, standard_models, variables_excluded, file='results/sample_size_ratio.RData')

## plot the estimates
load('data/labels.RData') # from 0_labels.R
source('99_functions.R')
all_ests_which = filter(all_ests, 
                        lambda=='min') %>% # plot for min
  mutate(outcome = 'actual') # dummy
to_plot = plot_function(indata = all_ests_which, 
                        which_outcome = 'actual', 
                        table_names = table_names_anzctr, 
                        ljust = 0.15,
                        x_limits = seq(-100,100,50),
                        minor_breaks =seq(-100,100,25))
to_plot = to_plot + 
  ylab('Percent change in sample size ratio (actual / target)')+
  theme(axis.title = element_text(vjust=1, hjust = 1))
  
# export to tall-thin plot
#jpeg('figures/anzctr_sample_size_ratio.jpg', width=6.5, height=9.5, units='in', res=600, family='Times New Roman')
tiff('figures/Fig7.tif', width=6.5, height=8.75, units='in', res=600, family='Times New Roman', compression = 'lzw') # PLOS
print(to_plot)
invisible(dev.off())
