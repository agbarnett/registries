# 3_plot_jags_chains.R
# plot the chains from the best JAGS model to show convergence/mixing
# Feb 2021
library(dplyr)
library(ggplot2)

# load the results for the best model
load("//hpc-fs/barnetta/anzctr/results/fp.best.t.RData")
num.chains = 2
MCMC = 5000
nrow = 2

# process the data
process = function(var){
  var <- with(output, get(var))
  mu <- as.matrix(var[,,])
  reshaped <- matrix(mu, nrow=MCMC*num.chains, ncol=nrow, byrow=TRUE)
  reshaped = data.frame(reshaped)
  reshaped$chain = rep(1:num.chains, each=MCMC)
  reshaped$iter = rep(1:MCMC, num.chains)
  long = reshape2:::melt.data.frame(reshaped, id.vars=c('chain','iter')) 
  return(long)
}
p1 = process('alpha') %>%
  mutate(
    facet = 'Mean',
    variable = case_when(
      variable == 'X1' ~ "Intercept",
      variable == 'X2' ~ "Slope"
    ))
p2 = process('beta') %>%
  mutate(facet = 'Variance',
         variable = case_when(
    variable == 'X1' ~ "Intercept",
    variable == 'X2' ~ "Slope"
  ))

## plot the data
mean_plot = ggplot(data=p1, aes(x=iter, y=value, linetype=factor(chain), col=factor(chain)))+
  geom_line()+
  facet_wrap(~variable, scales='free_y')+
  xlab('Iteration (after burn-in)')+
  ylab('Value')+
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1),"mm"))+
  facet_wrap(~variable, scale='free_y', ncol=1)+
  scale_colour_manual('Chain', values=c('goldenrod1','grey'))+
  scale_linetype_manual('Chain', values=1:2)
mean_plot

variance_plot = ggplot(data=p2, aes(x=iter, y=value, linetype=factor(chain), col=factor(chain)))+
  geom_line()+
  facet_wrap(~variable, scales='free_y')+
  xlab('Iteration (after burn-in)')+
  ylab('Value')+
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1),"mm"))+
  facet_wrap(~variable, scale='free_y', ncol=1)+
  scale_colour_manual('Chain', values=c('goldenrod1','grey'))+
  scale_linetype_manual('Chain', values=1:2)
  
variance_plot