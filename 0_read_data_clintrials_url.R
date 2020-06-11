# 0_read_data_clintrials_url.R
# read the clinicaltrials.gov data from XML files using the web
# May 2020

# version using URL

library(XML)

clin_number = 'NCT04320485'
online_xml = paste("https://clinicaltrials.gov/ct2/show/", clin_number, "?displayxml=true", sep='') #
xml_file = download.file(url = online_xml, destfile='temp.xml') # read clinicaltrials.gov files from the web to local file
result_xml = xmlParse(file = "temp.xml") # read into R

# extract variables
nct = xmlValue(getNodeSet(result_xml, "/clinical_study//nct_id"))
study_first_submitted = xmlValue(getNodeSet(result_xml, "/clinical_study//study_first_submitted"))
country = xmlValue(getNodeSet(result_xml, "/clinical_study//location_countries"))
gender = xmlValue(getNodeSet(result_xml, "/clinical_study//gender"))
minimum_age = xmlValue(getNodeSet(result_xml, "/clinical_study//minimum_age"))
maximum_age = xmlValue(getNodeSet(result_xml, "/clinical_study//maximum_age"))
intervention_type = xmlValue(getNodeSet(result_xml, "/clinical_study//intervention_type"))
study_type = xmlValue(getNodeSet(result_xml, "/clinical_study//study_type"))
enrollment = xmlValue(getNodeSet(result_xml, "/clinical_study//enrollment")) # 
enrollment_type
condition = xmlValue(getNodeSet(result_xml, "/clinical_study//condition"))

# remove
file.remove('temp.xml')

# import the XML file into R 
very_raw_pubmed <- table_articles_byAuth_adapted(pubmed_data = file, # from 99_table_articles_byAuth_adapted.R
                                                 encoding = "UTF-8") # recommended encoding


xml_file = download_xml(url = online_xml, file='temp.txt')
, destfile='temp.xml') # read clinicaltrials.gov files from the web to local file
