# 2_prep_clintrials.R
# Prepare data for regression models (used by 3_basic_table.R, 2_elasticnet_model_samplesize_clintrial.R)
# January 2021

## Prepare data for regression model
# change missing to a category
for.model = mutate(studies,
                   date = as.numeric(posted - as.Date('2010-01-01')) / (5*365.25), # standardised to five years
                   # horribly skewed, so log (base 2)
                   n_primary = log2(n_primary+1),
                   n_secondary = log2(n_secondary+1),
                   n_condition = log2(n_condition+1),
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
                   phase = ifelse(is.na(phase), 'Missing', phase),
                   study_design_time = ifelse(is.na(study_design_time), 'Missing', study_design_time),
                   study_design_observational = ifelse(is.na(study_design_observational), 'Missing', study_design_observational)
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
    volunteers = relevel(factor(volunteers), ref='No'), # 
    status = case_when( # combine two small categories
      status == 'No longer available' ~ 'Not available',
      status == 'Temporarily not available' ~ 'Not available',
      TRUE ~ as.character(status)
    ),
    status = relevel(factor(status), ref='Completed'),
    age_limit_max = relevel(factor(age_limit_max), ref='No limit'),
    age_limit_min = relevel(factor(age_limit_min), ref='No limit'),
    study_design_observational = relevel(factor(study_design_observational), ref='Cohort'),
    study_design_time = relevel(factor(study_design_time), ref='Prospective')) %>%
  select(-age_min_type, -age_min, -age_max_type, -age_max)
