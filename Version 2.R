library(tm)
library(tidyverse)
library(tidyr)

######    Cargar archivos



################ 1% de las entradas de blog







SBlog  <- Blog_Eng %>% sample_frac(.01, replace = FALSE) %>% mutate(Origin = "Blog") 
SNews  <- News_Eng %>% sample_frac(.01, replace = FALSE) %>% mutate(Origin = "Blog")
STweet <- Tweet_Eng %>% sample_frac(.01, replace = FALSE) %>% mutate(Origin = "Blog")


# NOT RUN {
data("crude")
## Document access triggers the stemming function
## (i.e., all other documents are not stemmed yet)
tm_map(crude, stemDocument, lazy = TRUE)[[1]]
## Use wrapper to apply character processing function
tm_map(crude, content_transformer(tolower))
## Generate a custom transformation function which takes the heading as new content
headings <- function(x)
  PlainTextDocument(meta(x, "heading"),
                    id = meta(x, "id"),
                    language = meta(x, "language"))
inspect(tm_map(crude, headings))
# }
data_frame<- do.call('rbind', lapply(SBlog, as.data.frame))
myCorpus <- Corpus(VectorSource(data_frame))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus<- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)

head(data_frame)
head(SBlog)

myCorpus <- Corpus(VectorSource(SBlog))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus<- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, stripWhitespace)


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

