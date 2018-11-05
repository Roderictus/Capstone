library(tm)
library(tidyverse)
library(tidyr)

####################    Cargar archivos     ######################################
Blog_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.blogs.txt"))
News_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.news.txt"))
Tweet_Eng<-readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
Tweet_Eng <- tbl_df(Tweet_Eng)

################        1% de las entradas de blog    ########################

P_Train <- .1

Blog_Eng  <- Blog_Eng  %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "Blog" ) 
News_Eng  <- News_Eng  %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "News" )
Tweet_Eng <- Tweet_Eng %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "Tweet")

All <- rbind(rbind(Blog_Eng, News_Eng),Tweet_Eng)

################      El tamaño es el número de términos únicos  
SBlog_bigrams <- Blog_Eng  %>% 
  sample_frac(P_Train, replace = FALSE) %>% 
  mutate(Origin = "Blog" )  %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

SBlog_trigrams <- Blog_Eng  %>% 
  sample_frac(P_Train, replace = FALSE) %>% 
  mutate(Origin = "Blog" )  %>%
  unnest_tokens(trigram, value, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE)

SBlog_fourgrams <- Blog_Eng  %>% 
  sample_frac(P_Train, replace = FALSE) %>% 
  mutate(Origin = "Blog" )  %>%
  unnest_tokens(fourgram, value, token = "ngrams", n = 4) %>%
  count(fourgram, sort = TRUE)

SBlog_fivegrams <- Blog_Eng  %>% 
  sample_frac(P_Train, replace = FALSE) %>% 
  mutate(Origin = "Blog" )  %>%
  unnest_tokens(fivegram, value, token = "ngrams", n = 5) %>%
  count(fivegram, sort = TRUE)

SBlog_sixgrams <- Blog_Eng  %>% 
  sample_frac(P_Train, replace = FALSE) %>% 
  unnest_tokens(sixgram, value, token = "ngrams", n = 6) %>%
  count(sixgram, sort = TRUE)

library(tidyr)

SepSix<- SBlog_sixgrams %>%
  separate(sixgram, c("w1", "w2", "w3", "w4","w5","w6"), sep = "")







  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  # new bigram counts:
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  
  library(dplyr)
  library(tidytext)
  library(janeaustenr)
  
  austen_bigrams <- austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  library(tidyr)
  
  bigrams_separated <- austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  
    
######################## searching      ################################




search_phrase<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

word6 <- "of"
word5 <- "case"
word4 <- "a"
word3 <- "and"
word2 <- "bouquet"
word1 <- "a"

Trigram_count %>% filter(W1 == word1 & W2 == word2)#Subsetting for next word



SBlog_trigrams %>% filter(trigram == "a case of")
SBlog_fourgrams %>% filter(fourgram == "and a case of")
SBlog_fivegrams %>% filter(fivegram == "bouquet and a case of")
SBlog_sixgrams %>% filter(sixgram == "a bouquet and a case of")




################     Conteo de ocurrencias #####################
