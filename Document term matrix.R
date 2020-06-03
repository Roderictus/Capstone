######      Versión con Document Term Matrix      ##############
library(tm)
library(tidyverse)
library(tidyr)
library(tidytext)
library(dplyr)
library(tm)
library(quanteda)


############      Un solo documento     ##############################
#All <- rbind(rbind(Blog_Eng, News_Eng),Tweet_Eng)
#write.csv(x = All, file = "D:/Proyectos R/Capstone/Capstone2/Docs/All_01.csv") # Guardar el archivo
############Diferentes documentos para propósito del corpus
#All <- VCorpus(DirSource(directory = "D:/Proyectos R/Capstone/Capstone2/Docs")) #Usando tm
#head(All)

##################### versión alternatiava usando quanteda    ##########
library(quanteda)

master_tokens <- tokens(
  x = tolower(All),
  remove_punc = TRUE,
  remove_twitter = TRUE,
  remove_numbers = TRUE,
  remmove_hyphens = TRUE,
  remove_symbols= TRUE,
  remove_url = TRUE)









head(master_tokens)
master_tokens[1]
tri_gram[[1]]

Stemed_All <- tokens_wordstem(master_tokens, language = "english")

#################n gramas

bi_gram <- tokens_ngrams(Stemed_All, n = 2)
tri_gram <- tokens_ngrams(Stemed_All, n = 3)

unigram_DFM <- dfm(Stemed_All)
bigram_DFM  <- dfm(bi_gram)
trigram_DFM <- dfm(tri_gram)

################# Trimming n grams

uni_DFM <- dfm_trim(unigram_DFM,3)
bi_DFM  <- dfm_trim(bigram_DFM,3)
tri_DFM <- dfm_trim(trigram_DFM,3)

sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)

#Create data tables with individual words as columns

library("data.table")

uni_words <- data.table(word_1 = names(sums_U), count = sums_U)

bi_words <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[',1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[',2),
  count = sums_B)
  
tri_words <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[',1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[',2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[',3),
  count = sums_T
)

#########     Indexing the n-grams

setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
setkey(tri_words, word_1, word_2, word_3)


##########    Kneser-Kney Smoothing

discount_value <- 0.75

##########    Bigram Probability

numofbigrams <- nrow(bi_words[by = .(word_1, word_2)])

ckn <- bi_words[, .(Prob = ((.N)/numofbigrams)), by = word_2]
setkey(ckn, word_2)

#Assigning the probabilities as second word of bigram, to unigrams
uni_words[,Prob := ckn[word_1, Prob]]
uni_words <- uni_words[!is.na(uni_words$Prob)]

#finding number of times word 1 ocurred as word 1 of bi-grams
n1wi <- bi_words[, .(N = .N), by = word_1]


length(tri_words)

####################      Document term matrix      ###############################
DTM_All <-DocumentTermMatrix(All)
inspect(DTM_All)
findFreqTerms(DTM_All, 1000)

################     Conteo de ocurrencias #####################
