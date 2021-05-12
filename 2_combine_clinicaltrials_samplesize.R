# 2_combine_clinicaltrials_samplesize.R
# combine the web-scraped results from clinicaltrials.gov
# April 2021
library(dplyr)
library(tidyverse)

# load the data and concatenate
all_data = NULL
to_load = dir('data', pattern='history') # from 1_historical_clintrials.R
all_data = NULL
for (file in to_load){
  load(paste('data/', file, sep='')) # from this program
  all_data = bind_rows(all_data, data)
}
# get one result per combination
all_data = unique(all_data) %>% # remove duplicates
  filter(sample_size_type !='') %>% # remove those with missing type
  group_by(id, sample_size_type) %>%
  arrange(id, sample_size_type, desc(date)) %>%
  slice(1) %>% # take latest date if there are two results with the same type
  ungroup()

## remove a few errors, found after checking very large sizes
index = all_data$id == 'NCT01675427' & all_data$sample_size_type == 'Anticipated' # entered 4000 twice
all_data = all_data[!index,]
index = all_data$id == 'NCT02361008'  # must be typo in sample size, way too big
all_data = all_data[!index,]
index = all_data$id == 'NCT01607255' & all_data$sample_size_type == 'Anticipated' # must be typo in Anticipated sample size, way too big
all_data$sample_size[index] = 480 # taken sample size from next entry
index = all_data$id == 'NCT00732238' & all_data$sample_size_type == 'Anticipated' # must be typo in Anticipated sample size, way too big
all_data$sample_size[index] = 80 # taken sample size from later entry
## something gone wrong with these, anticipated is cluster and actual is patients?
index = all_data$id == 'NCT00685867'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT01256658'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT03119389'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT02735382'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT01852617'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT03367364'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT02798354'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT02454101'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT02732600'  #
all_data = all_data[!index,]
index = all_data$id == 'NCT01653405'  #
all_data = all_data[!index,]
# this one is definitely cluster versus patients, luckily they also gave patients
index = all_data$id == 'NCT03537573' & all_data$sample_size_type =='Anticipated'  #
all_data$sample_size[index] = 10936
# ditto
index = all_data$id == 'NCT01760239' & all_data$sample_size_type =='Anticipated'  #
all_data$sample_size[index] = 17000
# fixed odd anticipated with later information
index = all_data$id == 'NCT01609842' & all_data$sample_size_type =='Anticipated'  #
all_data$sample_size[index] = 2500
# fixed odd anticipated with later information
index = all_data$id == 'NCT01235975' & all_data$sample_size_type =='Anticipated'  #
all_data$sample_size[index] = 440
# fixed odd anticipated with later information
index = all_data$id == 'NCT03541798' & all_data$sample_size_type =='Anticipated'  #
all_data$sample_size[index] = 300
# must be typo
index = all_data$id == 'NCT03313466' & all_data$sample_size_type == 'Actual' # 
all_data = all_data[!index, ]

# save
save(all_data, file='data/clinicaltrials_ratio.RData')

## old code below; keep in long format ...
# convert to wide to generate actual to anticipated ratio
wide = select(all_data, -date, -status) %>%
  unique() %>%
  pivot_wider(values_from='sample_size', names_from='sample_size_type') %>%
  filter(Anticipated > 0) %>% # exclude those who anticipated zero, presuming this is an error
  mutate(ratio = Actual/Anticipated) 

# save
save(wide, file='data/clinicaltrials_ratio.RData')
