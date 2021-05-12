# 3_bland_altman_ratio.R
# bland-altman work on the sample size ratio
# Comparing target and achieved sample size, with limits of agreement from Bayesian model
# April 2021
library(scales) # for comma format
library(stringr)
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(dplyr)
# for PLOS
library(extrafont)
loadfonts(device = "win")

## get the registry data
# a) ANZCTR
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
source('3_prep_ANZCTR.R') # prepare data for regression model
anzctr = for.model
# b) clintrials
load('data/clinicaltrials_analysis_plus.RData') # from 2a_update_clintrials_data.R 
source('3_prep_clintrials.R') # prepare data for regression model
clintrials = for.model 
# concatenate
for.model = bind_rows(anzctr, clintrials, .id='database') %>%
  mutate(database = factor(database, levels=1:2, labels=c('ANZCTR','clinicaltrials.gov'))) %>%
  filter(!is.na(samplesize_actual), # must have both
         !is.na(samplesize_target))

# Use a log-transformation (base e) to remove the strong skew in the sample sizes.
to_plot = mutate(for.model,
    samplesize_target_log = log(samplesize_target+0.1), # add small constant because of zeros
    samplesize_actual_log = log(samplesize_actual+0.1),
    diff = samplesize_actual_log - samplesize_target_log,
    diff_perc = 100*(exp(diff)-1), # percent difference
    aver = (samplesize_actual_log + samplesize_target_log)/2 )
# check
#filter(to_plot, diff< log(0.1)) %>% select(samplesize_target, samplesize_actual) # where sample was 10% of target
# stats
stats = group_by(to_plot, database) %>%
  summarise(mean=mean(diff), sd=sd(diff)) %>%
  mutate(z = qnorm(0.975),
         lower = mean - z*sd, # limits of agreement
         upper = mean + z*sd) %>%
  ungroup()

# label log axis
x.labels = c(1,5,50,500,5000,50000,1000000)
x.ticks = log(x.labels+1)
y.labels = c(0.1,0.25,1,4,10) # relative (like a risk ratio); symmetric
y.ticks = log(y.labels)
# plot
ba.plot_log2 = ggplot(data=to_plot, aes(x=aver, y=diff))+
  geom_point(pch=1)+
  scale_x_continuous(breaks=x.ticks, labels=x.labels)+
  scale_y_continuous(breaks=y.ticks, labels=y.labels)+
  xlab('Average sample size (log scale)')+
  ylab('Sample size ratio (actual / target)')+
  geom_hline(data=stats, aes(yintercept=mean), lty=2, col='red')+
  geom_hline(data=stats, aes(yintercept=lower), lty=2, col='green')+
  geom_hline(data=stats, aes(yintercept=upper), lty=2, col='green')+
  g.theme+
  facet_wrap(~database)
ba.plot_log2 

### try tile plot
resolution = 4 # controls size of tiles
# exclude a few large studies that squash plot
excluded = nrow(filter(to_plot, exp(aver) > 100000))
cat('Number excluded = ', excluded, ', percent = ', 100*excluded/nrow(to_plot), '.\n', sep='')
#
to_tile = filter(to_plot, exp(aver) <= 100000) %>%
   mutate(diff_bin = round(diff*resolution)/resolution,
   diff_aver = round(aver*resolution)/resolution) %>%
  group_by(database, diff_bin, diff_aver) %>%
  summarise(n=n()) %>%
  group_by(database)%>%
  mutate(percent = 100*prop.table(n))  %>%
  ungroup() 
# axis
x.labels = c(1,5,50,500,5000,100000)#,1000000)
x_labs = str_remove_all(format(x.labels, big.mark=',', scientific = FALSE), ' ')
x.ticks = log(x.labels+1)
y.labels = c(0.001,0.01,0.1,1,10,100,1000) # relative (like a risk ratio); symmetric
y.ticks = log(y.labels)
#
tile_plot = ggplot(data=to_tile, aes(x=diff_aver, y=diff_bin, fill=percent))+
  geom_tile()+
  scale_x_continuous(breaks=x.ticks, labels=x_labs)+
  scale_y_continuous(breaks=y.ticks, labels=y.labels)+
  scale_fill_gradient('Percent', low = "grey", high = "dark red")+
#  scale_fill_distiller()+
#  scale_fill_viridis_c(option = "magma")+
  xlab('Average sample size (log scale)')+
  ylab('Sample size ratio (actual / target)')+
#  geom_hline(data=stats, aes(yintercept=lower), lty=2, col='darkorchid1')+ # limits of agreement
#  geom_hline(data=stats, aes(yintercept=upper), lty=2, col='darkorchid1')+
  g.theme+
  facet_wrap(~database)
tile_plot

## add lines from best model ##
## get the best model 
# ANZCTR
load('//hpc-fs/barnetta/anzctr/results/fp.best.t.ANZCTR.RData')
anzctr = diff.pred
anzctr$x = av.pred
# clintrials
load('//hpc-fs/barnetta/anzctr/results/fp.best.t.clintrials.RData')
clintrials = diff.pred
clintrials$x = av.pred
to_plot_lines = bind_rows(anzctr, clintrials, .id='database') %>%
  filter(chain==99) %>%
  mutate(
    database = factor(database, levels=1:2, labels=c("ANZCTR",'clinicaltrials.gov')),
    percent = 0) %>% # needed to merge
  filter(exp(x) < 100000, # use same cut as plot
         row > 4) # very small estimates are too wide (also not applicable)
#
plus_plot = tile_plot + 
  geom_line(data=to_plot_lines, aes(x=x, y=lower), size=0.5, lty=5)+
  geom_line(data=to_plot_lines, aes(x=x, y=upper), size=0.5, lty=5)
#  geom_line(data=to_plot, aes(x=x, y=mean), size=0.5, lty=1) # do not add mean
plus_plot

# for plos
tiff('figures/Fig2.tif', width=6, height=4, units='in', res=600, family='Times New Roman', compression = 'lzw') # PLOS
print(plus_plot)
invisible(dev.off())

# save for BMJ Open markdown
save(plus_plot, file='figures/fig2.RData')

# look at results for two reference points (~50 and 500)
filter(diff.pred, chain==99) %>%
  mutate(x = exp(av.pred),
         lower = exp(lower),
         upper = exp(upper)) %>%
  select(row, x, mean, lower, upper) %>%
  filter(row %in% c(34,53))

