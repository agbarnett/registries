# 3_regression_tables_latex.R
# output regression tables for the final models in latex
# Feb 2021
library(xtable)
library(dplyr)
source('99_functions.R')

# get the labels
load('data/labels.RData') # from 0_labels.R

make_table = function(database){
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
      filter(lambda=='1se') # use minimum xval error or 1se
  }
  # format estimates
  to_table_ests = filter(all_ests, 
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
#
to_table_ests = left_join(to_table_ests, labs, by='term') %>% # add nice labels from 0_labels
  #arrange(group_number, as.numeric(RR)) # arrange by size or ...
  arrange(group_number, label) %>% # ... arrange alphabetically
  mutate(cell = paste(PC, ' (', CI , ')', sep='')) %>%
  select(outcome, group_number, group, label, cell)
# pivot wide
wide_table = tidyr::pivot_wider(to_table_ests, names_from=outcome, values_from=cell)
# remove repeated group labels
to_blank = c(1, diff(wide_table$group_number))
wide_table$group[to_blank == 0] = ''
# where to add hline
hline.after = diff(wide_table$group_number)
hline.after = which(hline.after==1)
if(length(hline.after)==0){hline.after=NULL}
wide_table = select(wide_table, -group_number)
# output to latex
print(xtable(wide_table), include.rownames=FALSE, math.style.negative=TRUE, hline.after=hline.after)
## text for table footnote
# a) reference groups
text = select(wide_table, group) %>%  # get each included group
  unique() %>%
  left_join(labs, by='group') %>%
  filter(reference==TRUE) %>% # just reference groups
  mutate(text = paste(group, ' = ', label, sep='')) %>%
  pull(text)
cat(text, sep='; ')
# b) variables with no categories selected, e.g., volunteers
included = select(wide_table, group) %>%  # get each included group
  unique() %>%
  pull(group)
all_groups = unique(labs$group) # list of all groups
text = all_groups[all_groups%in%included == FALSE]
cat('. Variables not selected: ')
cat(text, sep='; ')
} # end of function

# run through the tables:
make_table(database='anzctr')
make_table(database='clintrials')
make_table(database='ratio')

