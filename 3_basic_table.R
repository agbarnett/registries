# 3_basic_table.R
# output basic study characteristic table in latex
# January 2021
library(xtable) # for latex
library(dplyr)
library(tidyr)
source('99_functions.R')

## get the two data sets
#
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
source('2_prep_ANZCTR.R') # prepare data for regression model
anzctr = select(for.model, study_type, submitted, gender, age_limit_min, age_limit_max, n_primary, n_secondary, "samplesize_actual", "samplesize_target") %>%
  mutate(database = 'anzctr') 
#
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 
source('2_prep_clintrials.R')
clintrials = select(for.model, study_type, submitted, gender, age_limit_min, age_limit_max, n_primary, n_secondary, "sample_size", "sample_size_type") %>%
  mutate(database = 'clintrials',
         samplesize_target = ifelse(sample_size_type=='Anticipated', sample_size, NA),
         samplesize_actual = ifelse(sample_size_type=='Actual', sample_size, NA))
#
to_table = bind_rows(anzctr, clintrials)

# percent variables
percent_vars = c('gender', 'age_limit_min', 'age_limit_max','study_type')
freq = select(to_table, database, all_of(percent_vars)) %>%
  gather(key='var', value='res', -`database`) %>%
  group_by(database, var, res) %>%
  tally() %>%
  group_by(database, var) %>%
  mutate(percent = round(prop.table(n)*100)) %>%
  mutate(cell = paste(format(n,big.mark = ','), ' (', percent, ')', sep='')) %>%
  select(-n, -percent) %>%
  spread(database, cell)

# continuous variables
cont_vars = c('n_primary','n_secondary','samplesize_target','samplesize_actual')
long = select(to_table, database, all_of(cont_vars)) %>%
  mutate(n_primary = (2^n_primary) - 1 , # inverse log-transform
         n_secondary = (2^n_secondary) - 1 
  ) %>% # 
  gather(key='var', value='res', -`database`) %>%
  group_by(database, var) %>%
  filter(!is.na(res))
continuous = summarise(long,
                       median = median(res), 
            q1 = quantile(res, 0.25),
            q3 = quantile(res, 0.75)) %>%
  mutate(cell = paste(median, ' (', q1, ' to ', q3, ')', sep='')) %>%
  select(-median, -q1, -q3) %>%
  spread(database, cell)
# quick plot
ggplot(data=long, aes(x=res))+
  geom_histogram()+
  facet_wrap(~var, scales='free')

# date submitted (make as year submitted)
dates = mutate(to_table, year = as.numeric(format(submitted, '%Y'))) %>%
  group_by(database) %>%
  summarise(median = median(year), 
            q1 = quantile(year, 0.25),
            q3 = quantile(year, 0.75)) %>%
  mutate(cell = paste(median, ' (', q1, ' to ', q3, ')', sep='')) %>%
  select(-median, -q1, -q3) %>%
  spread(database, cell)

# latex
to_table_latex = bind_rows(dates, freq, continuous) %>%
  select('var','res','anzctr','clintrials') # ordering
print(xtable(to_table_latex), include.rownames=FALSE)
