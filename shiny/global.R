library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(scales)

# load the shiny-ready data
load('clinicaltrials_shiny_ready.RData') # from 1_process_clintrials_data.R
clintrials = filter(studies, study_type=='Interventional')
censor.date.clintrials = censor.date

load('anzctr_shiny_ready.RData') # from 0_read_data_anzctr.R
