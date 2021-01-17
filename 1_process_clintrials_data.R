# 1_process_clintrials_data.R
# process the data from clinicaltrials.gov
# January 2021
library(dplyr)
library(stringr)
source('99_functions.R')
censor.date = as.Date('2020-12-11') # date I downloaded zip

# find the data
to_process = dir('data/raw/') # from 0_read_data_clintrials_xml.R

# concatenate multiple RData files
all_excluded = all_studies = NULL
for (file in to_process){
  infile = paste('data/raw/', file, sep='')
  load(infile)
  all_excluded = bind_rows(all_excluded, excluded)
  all_studies = bind_rows(all_studies, studies)
}

# data management
all_studies = mutate(all_studies,
                     # simplify volunteers
                     volunteers = ifelse(volunteers=='Accepts Healthy Volunteers', 'Yes', volunteers), 
                     # simplify variable as all categories had "assignment",
                     assignment = str_remove_all(string=assignment, pattern=' Assignment'),
                     assignment = ifelse(nchar(assignment)>30, "Missing", assignment), # one very odd result NCT00828919 
                     # small edits
                     lead_sponsor_class = ifelse(lead_sponsor_class=='AMBIG', 'Other', lead_sponsor_class), # just a handful in this category
                     assignment = ifelse(masking=='Deep Brain Stimulation', 'Missing', assignment), # just one
                     masking = ifelse(masking=='open', 'None', masking), # tiny edits
                     masking = ifelse(masking=='Blinded|Dynamic|Investigator|there|This', 'Missing', masking), # one of each
                     # convert to numbers
                     n_arms = as.numeric(n_arms),
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
excluded = all_excluded
save(censor.date, studies, excluded, file='data/clinicaltrials_analysis_ready.RData')

# simpler version for shiny page
studies = filter(studies,
          sample_size_type != 'Not stated', # exclude studies without a sample size type
         !is.na(sample_size)) %>% # exclude studies without a sample size
  mutate(sample_size_type = ifelse(sample_size_type=='Anticipated', 'Target', sample_size_type)) %>% # rename
  select(study_type, sample_size_type, sample_size, purpose, allocation, assignment, phase)
save(censor.date, studies, file='shiny/clinicaltrials_shiny_ready.RData')
