# 2_plot_sample_size_by_year.R
# Plot sample size by year
# March 2021
library(dplyr)
library(tidyverse)
library(ggplot2)
options(dplyr.summarise.inform=FALSE)
N.boot = 1000 # number of bootstrap estimates for trimmed mean uncertainty
set.seed(202020)
# for PLOS
library(extrafont)
loadfonts(device = "win")

## a) anzctr ###
# get the data
load('data/AnalysisReady.RData') # from 0_read_data_anzctr.R 
# summarise by year
anzctr = mutate(studies, year=as.numeric(format(submitted, '%Y'))) %>% # use date submitted
  select(year, samplesize_target , samplesize_actual) %>%
  pivot_longer(-year, names_to='sample_size_type', values_to='sample_size') %>%
  mutate(sample_size_type = ifelse(sample_size_type == 'samplesize_actual', 'Actual', 'Anticipated')) %>%
  filter(!is.na(sample_size)) %>% # remove missing
  filter(year < 2021) # remove few from 2021
anzctr_stats = group_by(anzctr, year, sample_size_type ) %>%
  summarise(n = n(),
            median = median(sample_size),
            mean = mean(sample_size), # prone to outliers, but kept for illustartive purposes
            tmean = mean(sample_size, trim=0.01), # prone to outliers so use trimmed mean
            sd = sd(sample_size)) %>% 
  ungroup() 
# bootstrap for uncertainty
all_ests = NULL
for (k in 1:N.boot){
  boot = group_by(anzctr, year, sample_size_type) %>%
    sample_n(size=n(), replace=TRUE) %>%
    summarise(mean = mean(sample_size), # prone to outliers, but kept for illustartive purposes
              tmean = mean(sample_size, trim=0.01), # prone to outliers so use trimmed mean
              median = median(sample_size))
  all_ests = bind_rows(all_ests, boot)
}
# get bootstrap confidence intervals
ci = group_by(all_ests, year, sample_size_type) %>%
  summarise(
    mlower = quantile(mean, 0.05), 
    mupper = quantile(mean, 0.95),
    tlower = quantile(tmean, 0.05), 
    tupper = quantile(tmean, 0.95),
    lowerm = quantile(median, 0.05), 
    upperm = quantile(median, 0.95)) %>% 
  ungroup()
# add to median
anzctr_stats = full_join(anzctr_stats, ci, by=c('year', 'sample_size_type'))

## b) clintrials ###
# get the data
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R 
# summarise by year
clintrials = mutate(studies, year=as.numeric(format(submitted, '%Y'))) %>% # use date submitted
  filter(year < 2021) # remove few from 2021
clintrials_stats = group_by(clintrials, year, sample_size_type ) %>%
  summarise(n = n(),
            median = median(sample_size),
            mean = mean(sample_size), # prone to outliers, but kept for illustartive purposes
            tmean = mean(sample_size, trim=0.01), # prone to outliers so use trimmed mean
            sd = sd(sample_size)) %>% 
  ungroup()
# bootstrap for uncertainty
all_ests = NULL
for (k in 1:N.boot){
  boot = group_by(clintrials, year, sample_size_type) %>%
    sample_n(size=n(), replace=TRUE) %>%
    summarise(mean = mean(sample_size), # prone to outliers, but kept for illustartive purposes
              tmean = mean(sample_size, trim=0.01), # prone to outliers so use trimmed mean
              median = median(sample_size))
  all_ests = bind_rows(all_ests, boot)
}
# get bootstrap confidence intervals
ci = group_by(all_ests, year, sample_size_type) %>%
  summarise(mlower = quantile(mean, 0.05), 
            mupper = quantile(mean, 0.95),
            tlower = quantile(tmean, 0.05), 
            tupper = quantile(tmean, 0.95),
            lowerm = quantile(median, 0.05), 
            upperm = quantile(median, 0.95))
# add to median
clintrials_stats = full_join(clintrials_stats, ci, by=c('year', 'sample_size_type'))

## concatentate two data sources
to_plot = bind_rows(clintrials_stats, anzctr_stats, .id='database') %>%
  mutate(database_char = ifelse(database==1, 'clintrials.gov','ANZCTR'),
         sample_size_type = ifelse(sample_size_type =='Anticipated','Target',sample_size_type)) ### change labels to target to match paper

# plot
select = filter(to_plot, year >= 2005) # remove initial years with fewer observations
colours = c('dodgerblue','darkseagreen')
tplot = ggplot(data=select, aes(x=year, y=tmean, ymin=tlower, ymax=tupper, fill=sample_size_type, col=sample_size_type))+
  geom_ribbon(alpha=0.5, color=NA)+ # colour NA to remove lines on outside
  geom_line(size=1.05)+
  scale_y_continuous(limits=c(0,NA))+ # start at zero
  scale_color_manual('Sample size', values=colours)+
  scale_fill_manual('Sample size', values=colours)+
  theme_bw()+
  xlab('Year')+
  ylab('Trimmed mean sample size')+
  facet_wrap(~database_char, scales = 'free')+
  theme(legend.position = c(0.99,0.99),# top-right corner
        legend.justification = c(1,1),
        panel.grid.minor = element_blank()) 
tplot

#
jpeg('figures/sample_size_time.jpg', width=5.5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()
## export to PLOS ready plot
tiff('figures/Fig5.tif', width=6, height=4.5, units='in', res=600, family='Times New Roman', compression = 'lzw') # 
print(tplot)
invisible(dev.off())


### illustrative plot for appendix using all data
mplot = ggplot(data=to_plot, aes(x=year, y=mean, ymin=mlower, ymax=mupper, fill=sample_size_type, col=sample_size_type))+
  geom_ribbon(alpha=0.5, color=NA)+ # colour NA to remove lines on outside
  geom_line(size=1.05)+
  scale_y_continuous(limits=c(0,NA))+ # start at zero
  scale_color_manual('Sample size', values=colours)+
  scale_fill_manual('Sample size', values=colours)+
  theme_bw()+
  xlab('Year')+
  ylab('Mean sample size')+
  facet_wrap(~database_char, scales = 'free')+
  theme(legend.position = c(0.99,0.99),# top-right corner
        legend.justification = c(1,1),
        panel.grid.minor = element_blank()) 
mplot
jpeg('figures/sample_size_time_non_trimmed_mean.jpg', width=5.5, height=4, units='in', res=400, quality=100)
print(mplot)
dev.off()

# make into appendix
rmarkdown::render(input = "98_trend_plot_mean.Rmd",
                  output_format = "pdf_document",
                  output_file = 'S6_Fig.pdf') # output to specific file
