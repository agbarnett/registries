### use combined observational and interventional data ###
vars_to_include = c('study_type','samplesize_target','date','gender','age_limit_max','age_limit_min',
                    'control','endpoint','n_primary','n_secondary','ccode1','n_funding','provisional')
formula = paste('log(ratio+0.001) ~ -1 +', paste(vars_to_include, collapse = '+'))
# prepare data for glmnet
X = model.matrix(as.formula(formula), data=for.model)
# remove X's with no variance (all the same value)
variances = apply(X, 2, sd)
index = which(variances==0)
X = X[, !1:ncol(X)%in%as.numeric(index)] # remove from X

# dependent variable:
y = log(for.model$ratio + 0.02) # log-transform, small constant because of few zeros

# run model
enet_model = glmnet(y=y, x=X, alpha=0.95)
plot(enet_model, main=stype)

# x-validation to find best cut-off
enet_xval = cv.glmnet(y=y, x=X, alpha=0.95)
#enet_xvalr = cvr.glmnet(Y=y, X=X, alpha=0.95, nfolds=10, ncv=5, type.measure = 'mse', family='gaussian')  # with repeats, does not give SE
plot(enet_xval, main=stype)
beta = coef(enet_xval, s='lambda.min') # 
xval[[mtype]] = cv_enet_model # store x-validation results for later plotting


#beta = coef(enet_xval, s=2.966518e-02) # particular penalty
## re-run standard model for best coefficients with confidence intervals ##
update_vars_to_include = row.names(beta)[which(beta!=0)]
X.dash = X[, colnames(X) %in% update_vars_to_include]
vars_excluded = colnames(X)[colnames(X) %in% update_vars_to_include == FALSE] # variables that were excluded by elastic net
vars_excluded = data.frame(study_type=stype, variables=vars_excluded)
variables_excluded = bind_rows(variables_excluded, vars_excluded) # concatenate
if(ncol(X.dash)>0){ # in any variables remaining
  standard_model = glm(y~X.dash)
  # tidy estimates
  standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
    mutate(term = str_remove(string=term, pattern='X.dash'),
           study_type = 'Both') # add model type
}
sand = filter(standard_model_ests, p.value<0.001) %>%
  arrange(estimate)


##
## Additional model for interventional
beta = coef(cv_enet_xval, s='lambda.min')  # using minimum error instead of 1+se 
#b = abs(as.vector(beta))
#index = which(b >= quantile(b, 0.90)) # top 10% of variables in terms of absolute size
index = which(beta!=0)
## re-run standard model for best coefficients with confidence intervals ##
update_vars_to_include = row.names(beta)[index]
X.dash = X[, colnames(X) %in% update_vars_to_include]
standard_model = glm(y~X.dash)
# tidy estimates
standard_model_ests= broom::tidy(standard_model, conf.int=TRUE) %>%
  mutate(term = str_remove(string=term, pattern='X.dash'),
         lambda = 'min', # include penalty cut-off
         study_type = stype) # add model type
all_ests = bind_rows(all_ests, standard_model_ests) # concatenate nicely formatted estimates
