# 99_functions.R
# functions for reading data
# December 2020

# function to replace null with NA, used by clintrials reading
null_na = function(x, collapse=FALSE, date=FALSE){
  y = ifelse(is.null(x), NA, x)
  if(collapse==TRUE){# collapse vector into one character
     y = paste(unique(y), sep='', collapse=', ') 
  }
  if(date==TRUE){ # convert to date
     q = str_detect(string=y, pattern='\\?') # search for question mark, does not seem to be an issue
     y = ifelse(q==FALSE, as.Date(y, '%B %d, %Y'), 'Question') # flag `?` for now
     y = as.Date(y, origin='1970-01-01')
  }
  return(y)
}

# function to extract XML results for clinicaltrials.gov, no longer used
my_extract_xml = function(x, part, name, collapse=FALSE, count=FALSE, char=FALSE){
   path = paste(".//Struct[@Name='", part, "']//Field[@Name='", name, "']", sep='')
   get = xml_text(xml_find_all(x, path))
   length_get = length(get)
   if(length_get == 0){get = NA} # replace missing with NA
   if(length_get > 1){
      if(collapse==TRUE){get = paste(get, sep='', collapse=', ')} # combine
   }
   if(count == TRUE){
      get = length_get
   }
   if(char == TRUE){
      get = nchar(get) # just count characters
   }
   get[get=='N/A'] = 'Not Applicable' # replace N/A with 'Not Applicable' - avoid confusion with NA = missing
   return(get)
}

# converting exclusion age from text to number (ANZCTR)
convert_age = function(type, number){
 if(type %in% c('No limit' , 'Not stated', 'N/A') == TRUE){ 
   if(type=='N/A'){type = 'No limit'} # assume N/A means no limit
   t = type
   num = NA
 }
 if(type %in% c('No limit' , 'Not stated', 'N/A') == FALSE){
   t = 'Restricted'
   number = as.numeric(number)
   # scale number to years:
   if(type %in% c('Minutes','Minute')){num = number / (365.25*24*60)} 
   if(type %in% c('Hours','Hour')){num = number / (365.25*24)} 
   if(type %in% c('Days','Day')){num = number / 365.25} 
   if(type %in% c('Weeks','Week')){num = number / 52} 
   if(type %in% c('Months','Month')){num = number / 12} 
   if(type %in% c('Years','Year')){num = number / 1} 
   # if zero
   if(num==0){type = 'No limit'; num=NA}
 }
 toreturn = data.frame(type=t, num = num)
 return(toreturn)
}

# converting exclusion age from text to number (clinicaltrials.gov)
convert_age_clintrials = function(number){
   num = 99
   if(is.na(number) == TRUE | number =='N/A'){ 
      t = 'No limit'
      num = NA
   }
   if(is.na(num)==FALSE){ # not changed above
      t = 'Restricted'
      num = as.numeric(str_remove_all(number, '[^0-9]')) # just the number
      # scale number to years:
      if(str_detect(string=number, pattern='Minute')){num = num / (365.25*24*60)} 
      if(str_detect(string=number, pattern='Hour')){num = num / (365.25*24)} 
      if(str_detect(string=number, pattern='Day')){num = num / 365.25} 
      if(str_detect(string=number, pattern='Week')){num = num / 52} 
      if(str_detect(string=number, pattern='Month')){num = num / 12} 
      if(str_detect(string=number, pattern='Year')){num = num / 1} 
      # if zero
      if(num==0){type = 'No limit'; num=NA}
   }
   toreturn = data.frame(type = t, num = num)
   return(toreturn)
}

# nice variable name for anzctr - not complete
nice_anzctr_trials = function(inname){
   outname = case_when(
      inname == 'studytype' ~ 'Study type',
      inname == 'date' ~ 'Submitted date', 
      inname == 'study_update' ~ 'Updated date', # not sure
      inname == 'status' ~ 'Overall Recruitment Status',
      inname == 'purpose' ~ 'Primary Purpose',
      inname == 'phase' ~ 'Study Phase',
      inname == 'assignment' ~ 'Interventional Study Model', 
      inname == 'n_funding' ~ 'Number of funders',
      inname == 'n_primary' ~ 'Number of primary outcomes',
      inname == 'n_secondary' ~ 'Number of secondary outcomes',
      inname == 'masking' ~ 'Masking',
      inname == 'allocation' ~ 'Allocation',
      inname == 'gender' ~ 'Sex',
      inname == 'age_limit' ~ 'Age limit',
      inname == 'min_age' ~ 'Minimum Age',
      inname == 'max_age' ~ 'Maximum Age',
      TRUE ~ inname
   )
   return(outname)
}

# nice variable name for clinical trials
# https://prsinfo.clinicaltrials.gov/definitions.html
nice_clinical_trials = function(inname){
   outname = case_when(
      inname == 'study_type' ~ 'Study type',
      inname == 'study_first_submitted' ~ 'Submitted date', # not sure
      inname == 'study_update' ~ 'Updated date', # not sure
      inname == 'status' ~ 'Overall Recruitment Status',
      inname == 'purpose' ~ 'Primary Purpose',
      inname == 'phase' ~ 'Study Phase',
      inname == 'assignment' ~ 'Interventional Study Model', 
      inname == 'n_arms' ~ 'Number of Arms',
      inname == 'n_primary' ~ 'Number of primary outcomes',
      inname == 'n_secondary' ~ 'Number of secondary outcomes',
      inname == 'masking' ~ 'Masking',
      inname == 'allocation' ~ 'Allocation',
      inname == 'gender' ~ 'Sex',
      inname == 'min_age' ~ 'Minimum Age',
      inname == 'max_age' ~ 'Maximum Age'
   )
   return(outname)
}

# convert date that sometimes has no day
my.as.Date = function(indate){
   indate[is.na(indate)] = ''
   spaces = str_count(string=indate, pattern=' ')
   dformat = ifelse(spaces==1, '%d %B %Y', '%B %d, %Y') # format depends on number of spaces
   indate[spaces==1] = paste('15 ', indate[spaces==1], sep='') # assume middle of month
   outdate = as.Date(indate, dformat)
   return(outdate)
}

# function for rounding numbers with zeros kept
roundz = function(x, digits){
   dformat = paste('%.', digits, 'f', sep='')
   x = sprintf(dformat, round(x, digits))
   return(x)
}

## create nice plot of sample size regression model estimates
plot_function = function(indata, 
                         which_outcome, 
                         table_names, 
                         label_side = NULL, # side for group labels
                         ljust=0.5, # justfication of legend at top
                         x_limits=1:3, # major ticks for x-axis
                         minor_breaks=0 # minor ticks for x-axis
                         ){
   # get estimates
   this_ests = filter(indata, outcome == which_outcome) # from elastic net models
   # remove status from labels if target
   if(which_outcome=='target'){table_names = filter(table_names, group != 'Status')}
   # add estimates to labels
   add_ests = full_join(table_names, this_ests, by='term') %>%
      filter(!term == '(Intercept)',
             !is.na(estimate)|reference==TRUE)  %>% # remove missing estimates, but keep reference categories
      select(-p.value, -std.error, -statistic) %>% # tidy up
      mutate(estimate = ifelse(reference==TRUE, 0, estimate), # for reference groups
             study_type = ifelse(reference==TRUE, 'Reference group', study_type)) # just to avoid missing
   # only include groups that were in the final model
   add_ests  =group_by(add_ests, group_number) %>%
      summarise(any_non_ref = min(reference)) %>%
      right_join(add_ests, by='group_number') %>%
      filter(any_non_ref == 0) # only if there's at least one non-reference estimate
   
   # quick check that all estimates are in the reference table (should only be intercept); and vice versa
   check = function(){
      f = filter(add_ests, is.na(label)) %>% dplyr::select(term) # should be empty
   }
   
   # order results within group and make x-axis number
   est_rank = group_by(add_ests, group_number, term) %>%
      summarise(mean = mean(estimate)) %>%
      mutate(rank = rank(mean)) %>%
      ungroup() %>%
      select(-mean)
   add_x = left_join(add_ests, est_rank, by=c('group_number', 'term')) %>%
      mutate(final_number = (group_number*100) + rank, # *100 to split numbers
             xaxis = as.numeric(as.factor(final_number)))
   # reverse order to match table
   add_x = mutate(add_x, xaxis = max(xaxis) - xaxis + 1)
   
   # final
   all_res = mutate(add_x,
                    reference = as.numeric(reference) + 1, # to mark reference point
                    # back transform to relative change:
                    estimate = exp(estimate), 
                    conf.low = exp(conf.low),
                    conf.high = exp(conf.high),
                  # now make into percent change,
                  estimate = 100*(estimate-1),
                  conf.low = 100*(conf.low-1),
                  conf.high = 100*(conf.high-1)
                     
   ) 
   # get labels
   axis_labels = select(all_res, xaxis, label) %>%
      arrange(xaxis) %>%
      unique() %>%
      pull(label)
   
   # labels for groups inside plot area
   group_labels = group_by(all_res, group) %>%
      summarise(n=n(), meanx=mean(xaxis), maxx=max(xaxis)) %>%
      ungroup() %>%
      filter(n > 1) %>%
      mutate(
         estimate = max(all_res$conf.high, na.rm=T), # put labels at highest CI (right side)
         conf.low=0, conf.high=0, reference=1,
         group = ifelse(group=='continuous', 'Continuous variables', group)) # 
   dotted.lines = group_labels$maxx + 0.5 # dotted lines to split groups
   # dodge observational to avoid overlap of CIs with interventional
   all_res = mutate(all_res,
                    xaxis = ifelse(study_type=='Observational', xaxis+0.2, xaxis))
   # text for axis labels
   text1 = data.frame(xaxis=1, estimate=1, conf.low=0, conf.high=0, reference=1, label='Increase')
   text2 = data.frame(xaxis=1, estimate=1, conf.low=0, conf.high=0, reference=1, label='Decrease')
   # plot
   star.wars.relative = ggplot(data=all_res, aes(x=xaxis, y=estimate, ymin=conf.low, ymax=conf.high, shape=factor(reference), col=factor(study_type)))+
      geom_hline(lty=2, yintercept=0)+ # reference line at zero
      geom_point(size=2, shape=19)+
      geom_errorbar(width=0, size=1.02)+
      scale_color_manual(NULL, values=c('goldenrod1','dodgerblue','grey'))+
      geom_vline(lty=3, xintercept=dotted.lines)+ # breaks between groups of variables
      geom_text(data=text1, aes(x=xaxis, y=estimate, label =label), adj=-0.1, vjust=1, col=grey(0.5))+
      geom_text(data=text2, aes(x=xaxis, y=estimate, label =label), adj=1.1, vjust=1, col=grey(0.5))+
      geom_text(data=group_labels, aes(x=meanx, y=estimate, label =group), adj=1, col=grey(0.5))+
      scale_x_continuous(expand=c(0.01,0.01), breaks=1:length(axis_labels), labels=axis_labels, limits=c(0.5, length(axis_labels)+0.2))+ # plus 0.2 for dodge
      scale_y_continuous(breaks=x_limits, minor_breaks =minor_breaks )+ # 
      ylab('Percent change in sample size')+
      xlab('')+
      theme_bw()+
      ggtitle(" ")+ # create room for legend
      theme(#legend.position = 'top',
         legend.position = c(0, 1), # (x,y) position = 'top' not working
         legend.justification = c(ljust, -0.2), # further help getting it outside plot area (minus makes y bit higher)
         legend.direction = 'horizontal',
         legend.margin = unit(0,"lines"),
         text=element_text(size=13), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank())+
      coord_flip() 
   star.wars.relative
   return(star.wars.relative)
}
