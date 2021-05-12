# 3_plot_actual_target_ratio.R
# histogram of actual to target ratio using both databases
# using plots made in 3_model_actual_target_ratio_[clintrials/ANZCTR].R
# April 2021
library(ggplot2)
library(gridExtra)

# for PLOS
library(extrafont)
loadfonts(device = "win")

# get plot from ANZCTR
load('figures/ratio_plot_anzctr.RData')
anzctr = hplot

# get plot from clintrials
load('figures/ratio_plot_clintrials.RData')
clintrials = hplot

# for plos
tiff('figures/Fig1.tif', width=9, height=4, units='in', res=300, family='Times New Roman', compression = 'lzw') # PLOS
grid.arrange(anzctr, clintrials, ncol=2)
invisible(dev.off())

# for bmj open
save(anzctr, clintrials, file='figures/fig1.RData')
