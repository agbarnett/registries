# 1_process_clintrials_data.R
# process the data from clinicaltrials.gov
# May 2020
library(dplyr)
library(stringr)
source('99_functions.R')
censor.date = as.Date('2020-05-29') # date I downloaded zip

# find the data
to_process = dir('data/raw/') # from 0_read_data_clintrials_xml.R

# concatenate multiple RData files
all_excluded = all_studies = all_countries = all_types = NULL
for(file in to_process){
  infile = paste('data/raw/', file, sep='')
  load(infile)
  all_excluded = c(all_excluded, excluded)
  all_countries = c(all_countries, countries)
  all_types = c(all_types, types)
  all_studies = bind_rows(all_studies, studies)
}

# data management
all_studies = mutate(all_studies,
                     # format dates
                     posted = as.Date(posted, '%B %d, %Y'),
                     updated = as.Date(updated, '%B %d, %Y'),
                     results = as.Date(results, '%B %d, %Y'),
                     completed = my.as.Date(completed), # dates are sometimes without month
                     # simplify variable as all categories had "assignment",
                     assignment = str_remove_all(string=assignment, pattern=' Assignment'),
                     # convert to numbers
                     description_nchar = as.numeric(description_nchar),
                     description_nchar = ifelse(is.na(description_nchar), 0, description_nchar), # convert missing to zero
                     n_condition = as.numeric(n_condition),
                     n_arms = as.numeric(n_arms),
                     n_primary = as.numeric(n_primary),
                     n_secondary = as.numeric(n_secondary),
                     sample_size = as.numeric(sample_size),
                     # remove what appear to be dummy sample sizes
                     sample_size = ifelse(sample_size == 9999999, NA, sample_size),
                     sample_size = ifelse(sample_size == 99999999, NA, sample_size),
                     sample_size = ifelse(sample_size == 999999999, NA, sample_size),
                     #
                     sample_size_type = ifelse(is.na(sample_size) == FALSE & is.na(sample_size_type) == TRUE, 'Not stated', sample_size_type), # if sample size but no type
                     # used below
                     age_min = NA,
                     age_max = NA,
                     age_min_type = NA,
                     age_max_type = NA
                     )
# convert ages - takes a while
for (k in 1:nrow(all_studies)){
  res_min = convert_age_clintrials(all_studies$min_age[k])
  all_studies$age_min[k] = res_min$num
  all_studies$age_min_type[k] = res_min$type
  res_max = convert_age_clintrials(all_studies$max_age[k])
  all_studies$age_max[k] = res_max$num
  all_studies$age_max_type[k] = res_max$type
}
# no longer need these variables
all_studies = select(all_studies, -min_age, -max_age)

# blank a few impossible dates - to do

## save
# rename for simplicity
studies = all_studies
types = all_types
countries = all_countries
excluded = all_excluded
save(censor.date, studies, types, countries, excluded, file='data/clinicaltrials_analysis_ready.RData')

# simpler version for shiny page
studies = filter(studies) %>%
  filter(sample_size_type != 'Not stated', # exclude studies without a sample size type
         !is.na(sample_size)) %>% # exclude studies without a sample size
  select(study_type, sample_size_type, sample_size, purpose, allocation, assignment, phase)
save(censor.date, studies, file='data/clinicaltrials_shiny_ready.RData')
