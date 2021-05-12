# 3_plot_actual_target_ratio_model.R
# plot of the model estimates for the actual/target ratio
# using plots made in 3_model_actual_target_ratio_[clintrials/ANZCTR].R
# May 2021
library(dplyr)
library(ggplot2)
library(gridExtra)
load('data/labels.RData') # from 0_labels.R
source('99_functions.R')
# for PLOS
library(extrafont)
loadfonts(device = "win")

# get results from ANZCTR
load('results/sample_size_ratio.RData') # from 3_model_actual_target_ratio_ANZCTR.R
all_ests_anzctr = mutate(all_ests, 
                         term = ifelse(term=='study_statusStopped early', 'statusStopped early', term), # to match clintrials
                         term = ifelse(term=='study_statusWithdrawn', 'statusWithdrawn', term), # to match clintrials
                         outcome='target') # to trick colours

# get results from clintrials
load('results/sample_size_ratio_clintrials.RData') # from 3_model_actual_target_ratio_clintrials.R
all_ests_clintrials = mutate(all_ests, outcome='actual') # to trick colours
all_ests = bind_rows(all_ests_anzctr, all_ests_clintrials) %>%
  filter(lambda == '1se') # min or 1se

## plot the estimates
to_plot = plot_function(indata = all_ests, 
                        lsize = 11, # size of labels text
                        xlabs = c('Lower','Higher'), # for ratio labels
                        label_location = 'left', # put group labels on left
                        table_names = table_names_clintrials, 
                        remove_this_size = 0, # plot labels for small groups
                        ljust = 0.5, # move top legend
                        x_limits = seq(-100,0,20),
                        minor_breaks =seq(-100,0,10))
to_plot = to_plot + 
  ylab('Percent difference in sample size ratio (actual / target)')+
  scale_color_manual('Database:', labels=c('ANZCTR','clinicaltrials.gov','Reference'), values=c("forestgreen", "magenta3",'grey')) # change colours

# for journal
tiff('figures/Fig3.tif', width=6.6, height=6, units='in', res=400, family='Times New Roman', compression = 'lzw') # 
print(to_plot)
invisible(dev.off())

# for bmj open rmarkdown
save(to_plot, file='figures/fig3.RData')
