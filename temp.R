#Quanteda Tutorial
#Resources
################      PAPER
#http://vanatteveldt.com/p/welbers-text-r.pdf
################      Tutorial de Quanteda
#https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/advancing-text-mining/#usequanteda
###################################################################
#Download Data
#Make Corpus
#Extract DFM

URL <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y#"
#
download.file(url = URL, destfile = "Data/ONU.zip")
unzip(zipfile = "Data/UNGDC+1970-2018.zip", exdir = "Data/ONU")
getwd()

#library
library(tidyverse)        # Also loads dplyr, ggplot2, and haven
library(quanteda)         # For NLP
library(readtext)         # To read .txt files
library(stm)              # For structural topic models
library(stminsights)      # For visual exploration of STM
library(wordcloud)        # To generate wordclouds
library(gsl)              # Required for the topicmodels package
library(topicmodels)      # For topicmodels
library(caret)            # For machine learning

# Download data here: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y 
# and unzip the zip file

# Read in data (.txt files)
global_path <- "Data/ONU/Converted sessions/"

# We load the data (.txt files) from all subfolders (readtext can handle 
# this without specification)  and store them in the main UNGDspeeches 
# dataframe. Beyond the speech text, this data also includes the 
# meta-data from the text filenames and add variables for the country, 
# UN session, and year.
# The code is based on https://github.com/quanteda/quanteda.corpora/issues/6
# and https://github.com/sjankin/UnitedNations/blob/master/files/UNGD_analysis_example.Rmd 

# For the purpose of this blog post, we use the data from all sessions.
UNGDspeeches <- readtext(
  paste0(global_path, "*/*.txt"),
  docvarsfrom = "filenames",
  docvarnames = c("country", "session", "year"),
  dvsep = "_",
  encoding = "UTF-8"
)

head(UNGDspeeches)
UNGDspeeches$text


mycorpus <- corpus(UNGDspeeches, country = "country")

# Assigns a unique identifier to each text
docvars(mycorpus, "Textno") <-
  sprintf("%02d", 1:ndoc(mycorpus)) 

# Save statistics in "mycorpus.stats"
mycorpus.stats <- summary(mycorpus)

# And print the statistics of the first 10 observations
head(mycorpus.stats, n = 10)

# Preprocess the text

# Create tokens
token <-
  tokens(
    mycorpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_twitter = TRUE,
    remove_url = TRUE,
    remove_hyphens = TRUE,
    include_docvars = TRUE
  )

token_ungd <- tokens_select(
  token,
  c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
  selection = "remove",
  valuetype = "regex",
  verbose = TRUE
)

mydfm <- dfm(token_ungd,
             tolower = TRUE,
             stem = TRUE,
             remove = stopwords("english")
)

mydfm.trim <-
  dfm_trim(
    mydfm,
    min_docfreq = 0.075,
    # min 7.5%
    max_docfreq = 0.90,
    #  max 90%
    docfreq_type = "prop"
  ) 


head(dfm_sort(mydfm.trim, decreasing = TRUE, margin = "both"),
     n = 10,
     nf = 10) 

dict <- dictionary(file = "policy_agendas_english.lcd")





