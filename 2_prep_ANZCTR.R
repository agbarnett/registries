# 2_prep_ANZCTR.R
# Prepare data for regression models (used by 3_basic_table.R, 2_model_actual_target_ratio.R and 2_elasticnet_model_samplesize_ANZCTR.R)
# January 2021

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
