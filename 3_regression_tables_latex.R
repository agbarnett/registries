# 3_regression_tables_latex.R
# output regression tables for the final models in latex
# January 2021
library(xtable)
library(dplyr)
source('99_functions.R')

# get the labels
load('data/labels.RData') # from 0_labels.R

make_table = function(database, this_outcome='target'){
  # get the results and labels
  if(database=='anzctr'){
    load('results/ANZCTR_sample_size.RData')
    labs = table_names_anzctr
  }
  if(database=='clintrials'){
    load('results/clintrials_sample_size.RData')
    labs = table_names_clintrials
  }
  if(database=='ratio'){
    load('results/sample_size_ratio.RData')
    labs = table_names_anzctr
    all_ests = mutate(all_ests, outcome='ratio') %>%  # need this label to match other data
      filter(lambda=='min') # use minimum xval error
  }
  # format estimates
  to_table_ests = filter(all_ests, 
                         outcome == this_outcome,
                         term != '(Intercept)') %>%
  mutate(
    PC = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    # now change to percent change and report to 1 dp
    PC = roundz(100*(PC-1),1),
    conf.low = roundz(100*(conf.low-1),1),
    conf.high = roundz(100*(conf.high-1),1),
    CI = paste(conf.low, ' to ', conf.high, sep=''))
# split into interventional and observational
int = filter(to_table_ests, study_type == 'Interventional' ) %>% select(term, PC, CI) 
obs = filter(to_table_ests, study_type == 'Observational' ) %>% select(term, PC, CI) %>%
  rename('PC1' = 'PC',
         'CI1' = 'CI')
# join interventional and observational? Are there results in both
both = int
are_both = FALSE
if(nrow(obs)>0){
  both = full_join(int, obs, by='term')
  are_both = TRUE
}
#
to_table_ests = left_join(both, labs, by='term') %>% # add nice labels
  #arrange(group_number, as.numeric(RR)) # arrange by size or ...
  arrange(group_number, label) # ... arrange alphabetically
if(are_both==TRUE){to_table_ests_nice = select(to_table_ests, group, label, PC, CI, PC1, CI1)}
if(are_both==FALSE){to_table_ests_nice = select(to_table_ests, group, label, PC, CI)}
# remove repeated group labels
to_blank = c(1,diff(to_table_ests$group_number))
to_table_ests_nice$group[to_blank == 0] = ''
# where to add hline
hline.after = diff(to_table_ests$group_number)
hline.after = which(hline.after==1)
# output to latex
print(xtable(to_table_ests_nice), include.rownames=FALSE, math.style.negative=TRUE, hline.after=hline.after)
## text for table footnote
# a) reference groups
text = select(to_table_ests, group) %>%  # get each included group
  unique() %>%
  left_join(labs, by='group') %>%
  filter(reference==TRUE) %>% # just reference groups
  mutate(text = paste(group, ' = ', label, sep='')) %>%
  pull(text)
cat(text, sep='; ')
# b) variables with no categories selected, e.g., volunteers
included = select(to_table_ests, group) %>%  # get each included group
  unique() %>%
  pull(group)
all_groups = unique(labs$group) # list of all groups
text = all_groups[all_groups%in%included == FALSE]
cat('. Variables not selected: ')
cat(text, sep='; ')
} # end of function

# run through the tables:
make_table(database='anzctr', this_outcome='target')
make_table(database='anzctr', this_outcome='actual')
make_table(database='clintrials', this_outcome='target')
make_table(database='clintrials', this_outcome='actual')
make_table(database='ratio', this_outcome='ratio')

