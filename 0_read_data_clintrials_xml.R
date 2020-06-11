# 0_read_data_clintrials_xml.R
# read the clinicaltrials.gov data from XML files using the web
# version reading XMLs from mega zip file
# May 2020
library(xml2)
library(dplyr)
library(stringr)
source('99_functions.R')
home = getwd()

# folders to download
folders = dir('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/AllAPIXML')
folders = folders[folders!='Contents.txt']
# does data already exist
existing_data = str_remove(dir('data/raw/', pattern='.RData'), '.RData')
folders = setdiff(folders, existing_data)
N = length(folders)

# loop through folders
for (f in 1:N){
  go = paste('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/AllAPIXML/', folders[f], sep='')
  setwd(go)
  xml_files = dir() # find all xml files in this folder (individual trials)
  
  cat('folder = ', folders[f], '\n',sep='') # progress

 studies = countries = types = excluded = conditions = NULL
 for (k in 1:length(xml_files)){
  # get the XML data into R
  x <- tryCatch (read_xml(xml_files[k], options='RECOVER'), error=function(e){}, warning=function(f){}) # supress errors and warnings with dummy functions
  if(is.null(x)){ # if empty then record and skip to next
    excluded = c(excluded, xml_files[k]) # 
    next # skip to next
  }
  
  # temporary, search for alias
  alias = length(grep('alias',tolower(as.character(x))))
  if(alias > 1){ # put in file
    out_file = 'U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/alias.txt'
    report = file(out_file, 'a')
    cat(xml_files[k], '\n', file=report)
    close(report)
  }
  
  # extract individual parts of the XML
  id = my_extract_xml(x, part = 'IdentificationModule', name = 'NCTid')
  if(is.na(id)==TRUE){id = my_extract_xml(x, part = 'IdentificationModule', name = 'NCTId')}
  status = my_extract_xml(x, part = 'StatusModule', name = 'OverallStatus')
  description_nchar = my_extract_xml(x, part = 'DescriptionModule', name = 'DetailedDescription', char=TRUE) # count characters
  study_posted = my_extract_xml(x, part = 'StatusModule', name = 'StudyFirstPostDate')
  study_update = my_extract_xml(x, part = 'StatusModule', name = 'LastUpdatePostDate') # using update date used on clinicaltrials.gov web site
  study_results = my_extract_xml(x, part = 'StatusModule', name = 'ResultsFirstPostDate')
  complete_date = my_extract_xml(x, part = 'CompletionDateStruct', name = 'CompletionDate')
  complete_type = my_extract_xml(x, part = 'CompletionDateStruct', name = 'CompletionDateType')
  #lead_sponsor = my_extract_xml(x, part = 'SponsorCollaboratorsModule', name = 'LeadSponsorName') # too varied to be useful
  lead_sponsor_class = my_extract_xml(x, part = 'SponsorCollaboratorsModule', name = 'LeadSponsorClass')
  condition = my_extract_xml(x, part = 'ConditionsModule', name = 'Condition') # can be multiple - too varied to be useful
  n_condition = length(condition)
  study_type = my_extract_xml(x, part = 'DesignModule', name = 'StudyType') # 
  phase = my_extract_xml(x, part = 'DesignModule', name = 'Phase', collapse=TRUE) # can be multiple so collapse into single result
  allocation = my_extract_xml(x, part = 'DesignInfo', name = 'DesignAllocation') 
  assignment = my_extract_xml(x, part = 'DesignInfo', name = 'DesignInterventionModel') 
  purpose = my_extract_xml(x, part = 'DesignInfo', name = 'DesignPrimaryPurpose') 
  masking = my_extract_xml(x, part = 'DesignInfo', name = 'DesignMasking') 
  type = unique(my_extract_xml(x, part = 'ArmsInterventionsModule', name = 'InterventionType')) # can be multiple, just unique
  sample_size = my_extract_xml(x, part = 'EnrollmentInfo', name = 'EnrollmentCount') 
  sample_size_type = my_extract_xml(x, part = 'EnrollmentInfo', name = 'EnrollmentType') 
  gender = my_extract_xml(x, part = 'EligibilityModule', name = 'Gender') 
  min_age = my_extract_xml(x, part = 'EligibilityModule', name = 'MinimumAge') 
  max_age = my_extract_xml(x, part = 'EligibilityModule', name = 'MaximumAge') 
  n_arms = my_extract_xml(x, part = 'ArmsInterventionsModule', name = 'ArmGroupLabel', count=TRUE) # can be multiple; just use count
  n_primary = my_extract_xml(x, part = 'OutcomesModule', name = 'PrimaryOutcomeMeasure', count=TRUE) # can be multiple; just use count
  n_secondary = my_extract_xml(x, part = 'OutcomesModule', name = 'SecondaryOutcomeMeasure', count=TRUE) # can be multiple; just use count
  country = unique(my_extract_xml(x, part = 'ContactsLocationsModule', name = 'LocationCountry')) # can be multiple, just take unique countries
  
  # get NIH funding number, abandoned - too hard
#  frame_funding = NULL
#  funding_type = my_extract_xml(x, part = 'ProtocolSection', name = 'SecondaryIdType') # can be multiple
#  if(any(is.na(funding_type)) == FALSE){
#    if(any(str_detect(funding_type, pattern='NIH'))){
#      funding_number = my_extract_xml(x, part = 'ProtocolSection', name = 'SecondaryId') # can be multiple
#      frame_funding = data.frame(id=id, type=funding_type, number=funding_number) %>%
#        filter(str_detect(string=type, pattern='NIH')) # just NIH funding for matching with NIH reporter
#    }
#  }
  
  # conditions frame
  frame_condition = data.frame(id=id, condition=condition)
  # country frame
  frame_country = data.frame(id=id, countries=country)
  # type frame
  frame_type = data.frame(id=id, conditions=type)
  # frame with one result per trial
  frame = data.frame(id = id, 
                     status = status,
                     description_nchar = description_nchar,
                     posted = study_posted, 
                     updated = study_update,
                     results = study_results,
                     completed = complete_date,
                     completed_type = complete_type,
                     lead_sponsor_class = lead_sponsor_class,
                     study_type = study_type, 
                     n_condition = n_condition,
                     purpose = purpose, 
                     masking = masking, 
                     allocation=allocation, 
                     assignment=assignment, 
                     phase = phase, 
                     n_arms = n_arms,
                     sample_size = sample_size, 
                     sample_size_type = sample_size_type,
                     gender=gender, 
                     min_age=min_age, 
                     max_age=max_age,
                     n_primary = n_primary, 
                     n_secondary = n_secondary,
                     stringsAsFactors = FALSE)
  # concatenate
  studies = bind_rows(studies, frame)
  countries = bind_rows(countries, frame_country)
  conditions = bind_rows(conditions, frame_condition)
  types = bind_rows(types, frame_type)
  # tidy up
  remove(x, frame, frame_country, frame_type, frame_condition)
  # progress
  if(k%%100==0)(cat('Up to ',k,'.\r',sep=''))
}

# remove xml file to save space?
#file.remove('temp.xml')

# save 
setwd(home)
outfile = paste('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/raw/', folders[f], '.RData', sep='')
save(studies, countries, types, conditions, excluded, file=outfile)
} # end of folders loop
