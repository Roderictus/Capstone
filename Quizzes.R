library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
#Quizzes
#Q1
#####What is the length of the longest line seen in any of the three en_US data sets?
max(nchar(Blog_Eng))
max(nchar(News_Eng))
max(nchar(Tweet_Eng))


####Total number of times word love is in tweeter eng, also hate 
length(grep(pattern = "[Ll]ove", x = Tweet_Eng)) #111,221
length(grep(pattern = "[Hh]ate", x = Tweet_Eng)) #24,241

Tweet_Eng_df <- data_frame(text = Tweet_Eng) #transforming from character to data frame

Tweet_token <- Tweet_Eng_df %>%
      mutate(Entry_number = row_number()) %>%
      unnest_tokens(output = sentence, input = text) %>%
      unnest_tokens(output = word, input = sentence) %>%
      mutate(word_number = row_number()) 

length(grep(pattern = "[Ll]ove", x = Tweet_token))

length(grep(pattern = "[Hh]ate", x = Tweet_Eng)) #24,241
##############################################################
#biostats
##US_twitter data sets

grep(pattern = "A computer once beat me at chess, but it was no match for me at kickboxing", x = Tweet_Eng)
Tweet_Eng[556872]



colnames(Tweet_Eng_df)

SentenceBlog <- Blog_Eng_df %>% 
      mutate(PostNumber = row_number()) %>%
      unnest_tokens(output = sentence, input = text,token = "sentences")
SentenceBlog <- SentenceBlog %>%
      mutate(line = row_number())
WordBlog <- SentenceBlog %>%
      unnest_tokens(output = word, input = sentence)
WordBlog<-mutate(WordBlog, word_number = row_number())
#####################Second Week

################################################
##############Exploratory Data Analysis#########
################################################
#Questions to consider
#Some words are more frequent than others - what are the distributions of word frequencies?
#What are the frequencies of 2-grams and 3-grams in the dataset?
#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
#How do you evaluate how many of the words come from foreign languages?
#Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number #of words in the dictionary to cover the same number of phrases?


################################################
###################Modeling#####################
################################################

#Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
#Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.
#Questions to consider

#How can you efficiently store an n-gram model (think Markov Chains)?
#How can you use the knowledge about word frequencies to make your model smaller and more efficient?
#How many parameters do you need (i.e. how big is n in your n-gram model)?
#Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
#How do you evaluate whether your model is any good?
#How can you use backoff models to estimate the probability of unobserved n-grams?

#Here are a few tools that may be of use to you as you work on their algorithm:
      
#      object.size(): this function reports the number of bytes that an R object occupies in memory
#Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
#gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.
