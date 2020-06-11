# 0_random_check.R
# random check of the data
# June 2020
set.seed(4334049)

library(dplyr)
load('data/AnalysisReady.RData') # from 0_read_data.R 
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 

# ANZCTR
numbers = sample(studies$number, size=10, replace=FALSE)

studies = filter(studies, number %in% numbers)
funding = filter(funding_data, number %in% numbers)
ethics = filter(ethics_data, number %in% numbers)

k = 5 
str(filter(studies, number ==numbers[k]))
str(filter(funding, number ==numbers[k]))
str(filter(ethics, number ==numbers[k]))

# clinicaltrials.gov
numbers = sample(studies$id, size=10, replace=FALSE)

studies = filter(studies, id %in% numbers)
k = 4
str(filter(studies, id ==numbers[k]))
