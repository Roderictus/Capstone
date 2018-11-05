library(tm)
library(tidyverse)
library(tidyr)
library(tidytext)
library(dplyr)

####################    Cargar archivos     ######################################
Blog_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.blogs.txt"))
News_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.news.txt"))
Tweet_Eng<-readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
Tweet_Eng <- tbl_df(Tweet_Eng)

################        1% de las entradas de blog    ########################
P_Train <- .03

Blog_Eng  <- Blog_Eng  %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "Blog" ) 
News_Eng  <- News_Eng  %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "News" )
Tweet_Eng <- Tweet_Eng %>% sample_frac(P_Train, replace = FALSE) %>% mutate(Origin = "Tweet")

############      Un solo documento     ##############################
#All <- rbind(rbind(Blog_Eng, News_Eng),Tweet_Eng)
#write.csv(x = All, file = "D:/Proyectos R/Capstone/Capstone2/Docs/All_01.csv") # Guardar el archivo
############Diferentes documentos para propósito del corpus
All <- VCorpus(DirSource(directory = "D:/Proyectos R/Capstone/Capstone2/Docs")) #Usando tm

####################      Limpieza de los documentos
All <- tm_map(x = All,FUN = removePunctuation)
All <- tm_map(x = All,FUN = content_transformer(tolower))
All <- tm_map(x = All,FUN = PlainTextDocument)
All <- tm_map(x = All,FUN = removeNumbers)
All <- tm_map(x = All,FUN = stripWhitespace)
#review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
#myCorpus <- tm_map(myCorpus, removeWords,  stopwords("english"))
#ALLDF <- tidy(All)
cast_sparse(All)


library(tm)

data("AssociatedPress", package = "topicmodels")
AssociatedPress


####################      Document term matrix      ###############################
DTM_All <-DocumentTermMatrix(All)
inspect(DTM_All)
findFreqTerms(DTM_All, 1000)
###############     Otra matriz con frequencia inversa en lugar de ocurrencias      #################
IDF_DTM_ALL <- DocumentTermMatrix(All, control = list(weighting = weightTfIdf))

#IDF_DTM_ALLS = removeSparseTerms(IDF_DTM_ALL, 0.95)
review_dtm_tfidf










SBlog_bigrams <- SBlog %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2)
SBlog_trigrams <- SBlog %>%
  unnest_tokens(trigram, value, token = "ngrams", n = 3)
SBlog_fourgrams <- SBlog %>%
  unnest_tokens(fourgram, value, token = "ngrams", n = 4)
################     Conteo de ocurrencias #####################

##############    Bigram    #######################
Bigram_count <- SBlog_bigrams %>%
  count(bigram, sort = TRUE) 
Total_Bigram <- sum(Bigram_count$n)
unique_bigrams <- nrow(Bigram_count)
Bigram_count$freq <- (Bigram_count$n/Total_Bigram)*100
Bigram_count <- Bigram_count %>%
  separate(bigram, c("W1", "W2"), sep = " ")

##############    Trigram     #####################
Trigram_count <- SBlog_trigrams %>%
  count(trigram, sort = TRUE) 
Total_Trigram <- sum(Trigram_count$n)
unique_trigrams <- nrow(Trigram_count)
Trigram_count$freq <- (Trigram_count$n/Total_Trigram)*100
Trigram_count <- Trigram_count %>%
  separate(trigram, c("W1", "W2", "W3"), sep = " ")
#Absorbed % of first n terms
sum(Trigram_count[c(1:10),]$freq) #2.2 
sum(Trigram_count[c(1:100000),]$freq) #73.57

Trigram_count %>% filter(W1 == word1 & W2 == word2)#Subsetting for next word

##############    Fourgram     #####################
SBlog_fourgrams <- 
  SBlog %>%
  unnest_tokens(fourgram, value, token = "ngrams", n = 4) %>% 
  count(fourgram, sort = TRUE) %>%
  separate(fourgram, c("W1","W2","W3","W4"), sep = " ")
word1 <- "a"
word2 <- "case"
word3 <- "of"

#Limpieza


SBlog_fourgrams %>% 
  filter(W1 == word1 & W2 == word2 & W3 == word3) %>%
  select(W4)#Subsetting for next word

####    separar en líneas #####




####    Ngram óptimo


Blog_Eng[3,]

####    Tokenizing lines    ##########


####    10 grams    ###########




#The guy in front of me just bought a pound of bacon, a bouquet, and a case of
#You're the reason why I smile everyday. Can you follow me please? It would mean the





http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
http://www.bannedwordlist.com/lists/swearWords.txt 

