# 2a_update_clintrials_data.R
# update the clintrials data with the historical data on sample size and new data on study status
# Oct 2021
library(dplyr)
library(tidyverse)

# a) historic data from 2_combine_clinicaltrials_samplesize.R
load('data/clinicaltrials_ratio.RData')
all_data = dplyr::select(all_data, -date) # do not need date
# b) prior data from 1_process_clintrials_data.R
load('data/clinicaltrials_analysis_ready.RData')

## convert historical data to wide format
historical = dplyr::select(all_data, -status) %>%
  unique() %>%
  pivot_wider(values_from='sample_size', names_from='sample_size_type') %>%
  filter(Anticipated > 0) %>% # exclude those who anticipated zero, presuming this is an error
  rename('samplesize_target' = 'Anticipated',
         'samplesize_actual'= 'Actual') 
# add study status from actual row
status = filter(all_data, sample_size_type=='Actual') %>%
    select(id, status)
historical = left_join(historical, status, by='id')

## convert existing data to match the format
studies = mutate(studies,
                  samplesize_target = ifelse(sample_size_type=='Anticipated', sample_size, NA),
                  samplesize_actual = ifelse(sample_size_type=='Actual', sample_size, NA)) %>%
  dplyr::select(-sample_size, -sample_size_type)

## Merge two data sets
# use newer historical data if it is available, otherwise keep original data
studies = left_join(studies, historical, by='id') %>%
  mutate(samplesize_target = ifelse(!is.na(samplesize_target.y), samplesize_target.y, samplesize_target.x),
         samplesize_actual = ifelse(!is.na(samplesize_actual.y), samplesize_actual.y, samplesize_actual.x),
         status = ifelse(!is.na(status.y), status.y, status.x)) %>%
  select(-ends_with('.x'), -ends_with('.y'))

# save
save(censor.date, studies, excluded, file='data/clinicaltrials_analysis_plus.RData')
