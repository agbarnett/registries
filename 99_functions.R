# 99_functions.R
# functions for reading data
# May 2020

# function to extract XML results for clinicaltrials.gov
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
   if(is.na(number) == TRUE){ 
      t = 'No limit'
      num = NA
   }
   if(is.na(number) == FALSE){ 
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
