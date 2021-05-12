# 98_word_frequency.R
# check word frequency of titles in men and women only studies
# see http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# March 2021
library(tm)
library(dplyr)

# get data from 1_process_clintrials_data.R
load('data/clinicaltrials_analysis_ready.RData')

# just the title, just for women
freqs = NULL
for (g in c('Female','Male')){
text <- filter(studies, gender==g) %>%pull(brief_title)
# convert type
 docs <- Corpus(VectorSource(text))
 # Convert the text to lower case
 docs <- tm_map(docs, content_transformer(tolower))
 # Remove numbers
 docs <- tm_map(docs, removeNumbers)
 # Remove english common stopwords
 docs <- tm_map(docs, removeWords, stopwords("english"))
 # Remove your own stop word
 # specify your stopwords as a character vector
 #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
 # Remove punctuations
 docs <- tm_map(docs, removePunctuation)
 # Eliminate extra white spaces
 docs <- tm_map(docs, stripWhitespace)
 
 # word frequency
 dtm <- TermDocumentMatrix(docs)
 m <- as.matrix(dtm)
 v <- sort(rowSums(m), decreasing=TRUE)
 d <- data.frame(word = names(v), freq=v)
 f = head(d, 10) %>% mutate(gender = g) # top ten
 freqs = bind_rows(freqs, f)
}
rownames(freqs) = NULL

# make into a PDF appendix
rmarkdown::render(input = "98_word_frequency_table.Rmd",
                  output_format = "pdf_document",
                  output_file = 'Supplement_table_3.pdf') # output to specific file


