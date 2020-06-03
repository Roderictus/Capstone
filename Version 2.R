library(quanteda)
library(readtext)
require(quanteda.corpora)
library(tidyverse)
library(data.table)
library(tm)
library(stringr)
library(rtweet)
library(spacyr)
library(newsmap)
quanteda_options(threads = 5)

####################    Cargar archivos     ######################################
#Blog_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.blogs.txt"))
#News_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.news.txt"))
Blog_Eng<-read_lines(file = "Data/final/en_US/en_US.blogs.txt")
News_Eng<-read_lines(file = "Data/final/en_US/en_US.news.txt")
Tweet_Eng<-readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
#para la versión que no utiliza tbl df
#Tweet_Eng <- tbl_df(Tweet_Eng)
master <- c(Blog_Eng, News_Eng, Tweet_Eng)

rm(Blog_Eng)
rm(News_Eng)
rm(Tweet_Eng)

gc()  #Limpieza

####################      Quanteda      #####################
summary(master, n = 2)
Master_Tokens <- tokens(x = master[1:1000],
       remove_punct      = TRUE,
       remove_symbols    = TRUE,
       remove_numbers    = TRUE,
       remove_url        = TRUE,
       remove_separators = TRUE,
       split_hyphens     = TRUE)
#class(Master_Tokens)

Corpus <- dfm(Master_Tokens)  #Crear un Corpus
              #tolower = TRUE, # faltaba ponerlo en minúsculas
              #stem = TRUE,    # se podría poner TRUE para reducir el número de palabras distintas
              
Master_Tokens <- tokens_select(Master_Tokens, pattern = stopwords('en'), selection = 'remove') #removing stop words

Corpus <- dfm(Master_Tokens,  #Crear un Corpus
              tolower = TRUE, # faltaba ponerlo en minúsculas
              stem = TRUE,    # se podría poner TRUE para reducir el número de palabras distintas
              remove_punct =TRUE   )

#ncol(Corpus) 
#relative one gram density

WordD   <- colSums(Corpus)
WordD[order(colSums(-Corpus))]
head(WordD[order(colSums(-Corpus))])
NWords  <- sum(colSums(Corpus)) #5,588,342 palabras
TasaP <- (WordD/NWords)*100000
head(TasaP)
head(TasaP[order(-TasaP)]) #Ordered rate 
#First Word
# Relative frequency of one gram
sort(x = ((WordD/NWords)*100000),  decreasing = TRUE) #same
tail(Master_Tokens)
Master_Tokens[[1]][1]
length(Master_Tokens)
###########################
FirstW <- vector()
for (i in 1:length(Master_Tokens)) {
  wordtemp <- Master_Tokens[[i]][1]
  FirstW   <- c(FirstW, wordtemp)
  }
rm(wordtemp)

FWColSum <- FirstW %>% dfm() %>% colSums() 
FWDFM <- FirstW %>% dfm()
OFW <- FWColSum[order(colSums(-FWDFM))]

head(OFW/length(Master_Tokens))  #probabilistic frequency

#bigramsto fourgrams
bi_gram   <- tokens_ngrams(x = Master_Tokens, n = 2) #forming the bigram
tri_gram  <- tokens_ngrams(x = Master_Tokens, n = 3) 
four_gram <- tokens_ngrams(x = Master_Tokens, n = 4)

bi_gram

fcm(bi_gram)


x <- fcm(x = Corpus, size = 5, ordered = TRUE) #size es el numero de features antes y despu[es de la pregunta

#devolver una columna y una línea específicas




#droping features under a certain n 




tri_gram
four_gram

unigram_probs <- hacker_news_text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#Feature co-ocurrence matrix (fcm)
FCMCorp <- fcm(Corpus)

FCMCorp

grep(pattern = "in", x = FCMCorp)

#eliminar palabras con bajas frequencias




#se podría buscar la segunda palabra dentro de esta matriz



x <- fcm(data_corpus_inaugural, context = "window", size = 5)

#search for a  word, return the second word 

#vector para remover palabras con una ocurrencia de uno
##############################################################

#First word can be most common first word 
#Eliminate words with low ocurrence  
#SearchWord can be used to return the most common n-1 gram that has the word words 

bi_gram


uni_DFM  <- dfm(stemed_words)
bi_DFM   <- dfm(bi_gram)
tri_DFM  <- dfm(tri_gram)
four_DFM <- dfm(four_gram)

sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)

uni_words <- data.table(word_1 = names(sums_U), count = sums_U)

bi_words  <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
  count = sums_B)

tri_words <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
  count = sums_T)

setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
setkey(tri_words, word_1, word_2, word_3)

discount_value <- 0.75

######## Finding Bi-Gram Probability #################

# Finding number of bi-gram words
numOfBiGrams <- nrow(bi_words[by = .(word_1, word_2)])

# Dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
# ( Finding probability for a word given the number of times it was second word of a bigram)
ckn <- bi_words[, .(Prob = ((.N) / numOfBiGrams)), by = word_2]
setkey(ckn, word_2)

# Assigning the probabilities as second word of bigram, to unigrams
uni_words[, Prob := ckn[word_1, Prob]]
uni_words <- uni_words[!is.na(uni_words$Prob)]

# Finding number of times word 1 occurred as word 1 of bi-grams
n1wi <- bi_words[, .(N = .N), by = word_1]
setkey(n1wi, word_1)

# Assigning total times word 1 occured to bigram cn1
bi_words[, Cn1 := uni_words[word_1, count]]

# Kneser Kney Algorithm
bi_words[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * n1wi[word_1, N] * uni_words[word_2, Prob])]

######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 
tri_words[, Cn2 := bi_words[.(word_1, word_2), count]]

# Finding count of word1-word2 combination in trigram
n1w12 <- tri_words[, .N, by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
tri_words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * n1w12[.(word_1, word_2), N] *
            bi_words[.(word_1, word_2), Prob]]

uni_words <- uni_words[order(-Prob)][1:50]

# function to return highly probable previous word given two successive words
triWords <- function(w1, w2, n = 5) {
  pwords <- tri_words[.(w1, w2)][order(-Prob)]
  if (any(is.na(pwords)))
    return(biWords(w2, n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_3])
  count <- nrow(pwords)
  bwords <- biWords(w2, n)[1:(n - count)]
  return(c(pwords[, word_3], bwords))
}

# function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
  pwords <- bi_words[w1][order(-Prob)]
  if (any(is.na(pwords)))
    return(uniWords(n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_2])
  count <- nrow(pwords)
  unWords <- uniWords(n)[1:(n - count)]
  return(c(pwords[, word_2], unWords))
}

# function to return random words from unigrams
uniWords <- function(n = 5) {  
  return(sample(uni_words[, word_1], size = n))
}

getWords <- function(str){
  require(quanteda)
  tokens <- tokens(x = char_tolower(str))
  tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
  
  words <- triWords(tokens[1], tokens[2], 5)
  chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")
  
  print(words[1])
}

getWords("Shall")



#rm(Blog_Eng)
#rm(News_Eng)
#rm(Tweet_Eng)
rm(US_Blogs_Sample)
rm(US_News_Sample)
rm(US_Twitter_Sample)
# The guy in front of me just bought a pound of bacon, a bouquet, and a case of
# You're the reason why I smile everyday. Can you follow me please? It would mean the



################    Limpieza nueva versión con Quanteda   ####################
#removing stop words
#Tidy and Custom Tidy
#temp <- data.frame(c("1","2","3","4","5","6","7","8","9","0"), "Custom") #Here we can add custom words
#colnames(temp) <- c("word", "lexicon")
#data(stop_words)
#stop_words <- rbind(stop_words, temp) #Main words for removal

corp <- corpus(All)


master_tokens <- tokens(
  x = tolower(All),
  remove_punc = TRUE,
  remove_twitter = TRUE,
  remove_numbers = TRUE,
  remove_hyphens = TRUE,
  remove_symbols= TRUE,
  remove_url = TRUE)


head(master_tokens)











####################      Limpieza de los documentos
All <- tm_map(x = All,FUN = removePunctuation)
All <- tm_map(x = All,FUN = content_transformer(tolower))
All <- tm_map(x = All,FUN = PlainTextDocument)
All <- tm_map(x = All,FUN = removeNumbers)
All <- tm_map(x = All,FUN = stripWhitespace)



#review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
#myCorpus <- tm_map(myCorpus, removeWords,  stopwords("english"))
#ALLDF <- tidy(All)





##############    Bigram    #######################
Bigram_count <- Corpus %>%
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
All_fourgrams <-All %>%
  unnest_tokens(fourgram, value, token = "ngrams", n = 4) %>% 
  count(fourgram, sort = TRUE) %>%
  separate(fourgram, c("W1","W2","W3","W4"), sep = " ")
word1 <- "a"
word2 <- "case"
word3 <- "of"

All_fourgrams %>% 
  filter(W1 == word1 & W2 == word2 & W3 == word3) %>%
  select(W4,n)#Subsetting for next word


##############    Sixgram     ##################
All_sixgrams <- All %>%
  unnest_tokens(sixgram, value, token = "ngrams", n =6 ) %>%
  count(sixgram, sort = TRUE) %>%
  separate(sixgram, c("W1","W2","W3","W4","W5","W6"), sep = " ")

####Function, word a mizer
phrase <- "This is a very nice phrase"
strsplit(phrase, " ")[[1]][2]

phrase[][1]
class(phrase)


A1 <- "W1"
A2 <- "muy"
C  <- cbind(A1,A2)


df <- data.frame()

Phraser <- function(phrase) {
  Len <- lengths(strsplit(phrase, " "))
  for( i in 1+1: Len -1 ) {
    A <- str_c("Word", i)
    B <- strsplit(phrase, " ")[[1]][i]
    C <- cbind(A,B)
    print(C)
    df<-bind_rows(C, df)
  }
}



Phraser(phrase = "muy bonita y larga frase")

#####

word1 <- "bouquet"
word2 <- "and"
word3 <- "a"
word4 <- "case"
word5 <- "of"


All_sixgrams %>% 
  filter(W2 == word2 & W3 == word3 & W4 == word4 & W5 == word5) %>%
  select(W6,n)#Subsetting for next word

All_fourgrams %>% 
  filter(W1 ==  "a" & W2 == "case" & W3 == "of") 


#The guy in front of me just bought a pound of bacon, a bouquet, and a case of
#You're the reason why I smile everyday. Can you follow me please? It would mean the



####    separar en líneas #####
SBlog_sixgrams <-
  Blog_Eng %>%
  unnest_tokens(fourgram)







####    Ngram óptimo
####    Tokenizing lines    ##########
####    10 grams    ###########






http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
http://www.bannedwordlist.com/lists/swearWords.txt 





#While tokens_ngrams() generates n-grams or skip-grams in all possible combinations of tokens, tokens_compound() generates n-grams more selectively. For example, you can make negation bi-grams using phrase() and a wild card (*).


#toks_neg_bigram <- tokens_compound(toks, pattern = phrase('not *'))
#toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase('not_*'))
#head(toks_neg_bigram_select[[1]], 30)




#The guy in front of me just bought a pound of bacon, a bouquet, and a case of
#versión con readtext 

Blog_Eng  <- readtext(file = "Data/final/en_US/en_US.blogs.txt")
News_Eng  <- readtext(file = "Data/final/en_US/en_US.news.txt")
Tweet_Eng <- readtext(file = "Data/final/en_US/en_US.twitter.txt")

rm(Blog_Eng)
rm(News_Eng)
rm(Tweet_Eng)

Master <-  c(Blog_Eng, News_Eng, Tweet_Eng)

TMaster_Tokens <- tokens(x = Master[1:1000],
                         remove_punct      = TRUE,
                         remove_symbols    = TRUE,
                         remove_numbers    = TRUE,
                         remove_url        = TRUE,
                         remove_separators = TRUE,
                         split_hyphens     = TRUE, 
                         verbose = TRUE)

head(Master_Tokens)
