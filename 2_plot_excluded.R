# 2_plot_excluded.R
# CONSORT style diagram of excluded studies
# January 2021
library(dplyr)
library(ggplot2)
library(diagram)

# load the processed data from the two registries
load('data/AnalysisReady.RData')# from 0_read_data_anzctr.R
anzctr_excluded = excluded
n_anzctr = nrow(studies)
load('data/clinicaltrials_analysis_ready.RData') # from 1_process_clintrials_data.R
clintrials_excluded = excluded
n_clintrials = nrow(studies)

# exclusion counts counts
count_anzctr = group_by(anzctr_excluded, reason) %>%
  tally()%>% 
  arrange(-n) # high to low
count_clintrials = group_by(clintrials_excluded, reason) %>%
  tally() %>% 
  arrange(-n)
# get starting numbers
n_start_anzctr = n_anzctr + sum(count_anzctr$n)
n_start_clintrials = n_clintrials + sum(count_clintrials$n)

## a) function to make diagram ANZCTR
make_diagram_anzctr = function(){
par(mai=c(0,0,0,0))
labels = c(paste('Downloaded\n n = ', format(n_start_anzctr, big.mark = ','), sep=''),
           paste('Excluded\n- ', count_anzctr$reason[1], ', n = ', count_anzctr$n[1],
                 '\n- ', count_anzctr$reason[2], ', n = ', count_anzctr$n[2], sep=''),
           paste('Analysed\n n =', format(n_anzctr, big.mark = ',')))
n_labels = length(labels)
M = matrix(nrow=n_labels, ncol=n_labels)
M[3,1] = "' '" 
pos = matrix(data=c(0.2,0.8,
                    0.7,0.5,
                    0.2,0.2), ncol=2, byrow=TRUE)
sizes=c(1.5,2.3,1.5) / 10
props = c(0.67,0.5,0.67) # narrower for first and last
plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
shape::Arrows(x0=0.2, x1=0.45, y0=0.5, y1=0.5, arr.width=0.2, arr.length=0.22, arr.type='triangle')
# heading
text(0.5, 0.95, "ANZCTR", font=2)
}
make_diagram_anzctr()

## a) function to make diagram clintrials.gov
make_diagram_clintrials = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(n_start_clintrials, big.mark = ','), sep=''),
             paste('Excluded\n- ', count_clintrials$reason[1], ', n = ', count_clintrials$n[1],
                   '\n- ', count_clintrials$reason[2], ', n = ', count_clintrials$n[2], 
                   '\n- ', count_clintrials$reason[3], ', n = ', count_clintrials$n[3], sep=''),
             paste('Analysed\n n =', format(n_clintrials, big.mark = ',')))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 
  pos = matrix(data=c(0.2,0.8,
                      0.62,0.5,
                      0.2,0.2), ncol=2, byrow=TRUE)
  sizes=c(1.5,3.1,1.5) / 10
  props = c(0.67,0.45,0.67) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.2, x1=0.29, y0=0.5, y1=0.5, arr.width=0.2, arr.length=0.22, arr.type='triangle')
  # heading
  text(0.5, 0.95, "clintrials.gov", font=2)
}
make_diagram_clintrials()


# export
jpeg('figures/consort_plot.jpg', width=7, height=5, units='in', res=300)
layout(mat=t(1:2))
make_diagram_anzctr()
make_diagram_clintrials()
dev.off()
layout(1)
