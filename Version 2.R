library(quanteda)
library(readtext)
library(tidyverse)
library(data.table)
library(tm)
library(stringr)
library(tidyr)
library(tidytext)
quanteda_options(threads = 5)

#require(quanteda.corpora)
#library(rtweet)
#library(spacyr)
#library(newsmap)
###versión sólo con twitter
library(tm)
#library(RWeka)

####################    Cargar archivos     ######################################
#Blog_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.blogs.txt"))
#News_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.news.txt"))
Blog_Eng<-read_lines(file = "Data/final/en_US/en_US.blogs.txt") #200 MB
News_Eng<-read_lines(file = "Data/final/en_US/en_US.news.txt")
Tweet_Eng<-readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
#para la versión que no utiliza tbl df
#Tweet_Eng <- tbl_df(Tweet_Eng)
master <- c(Blog_Eng, News_Eng, Tweet_Eng)  #vector, texto,  que junta todo
rm(Blog_Eng)
rm(News_Eng)
rm(Tweet_Eng)
gc()  #Limpieza
#temp <- VectorSource(readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE))
#CorpusTemp <- Corpus(temp)

################################################################################
master <-tolower(master)
tkns <- tokens(x = unlist(master), 
       remove_punct = TRUE,
       remove_symbols = TRUE, 
       remove_numbers = TRUE,
       remove_url = TRUE,
       remove_separators = TRUE) #tokens con un poco de limpieza
Subset <- sample(x = 1:length(tkns), size = (length(tkns)/10))
TKNS <- tkns[Subset] #utilizar una muestra
##################        N-GRAMAS        ###################
##########################     Dos Palabras

bi_gram   <- tokens_ngrams(x = TKNS, n = 2, concatenator = " ") #forming the bigram
head(TKNS)

temp <- table(unlist(bi_gram)) %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) 
temp <- filter(temp , Freq > 2 ) #sólo ocurrencias de 3 o más

mp2 <- unlist(str_split(string = temp$Var1, pattern = " ")) 

P2 <- c(1:nrow(temp))*2 #segunda palabra, pares 
P1 <- P2-1  #primer palabra, impares

colnames(temp) <- c("frase", "freq")
temp$p1 <- mp2[P1]
temp$p2 <- mp2[P2]
bi_gram <- temp

rm(temp)
rm(P1)
rm(P2)
rm(mp2)
rm(Subset)

bi_gram

#######################    Tres Palabras    ######################
tri_gram  <- tokens_ngrams(x = TKNS, n = 3, concatenator = " ") 

temp <- table(unlist(tri_gram)) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

mp3 <- temp$Var1 %>% 
  str_split(pattern = " ") %>% 
  unlist()#2,306,744

colnames(temp) <- c("frase", "freq")

P3 <- c(1:nrow(temp))*3  #3,6,9
P2 <- P3-1 #2,5,8
P1 <- P3-2 #1,4,7

temp$p1 <- mp3[P1]
temp$p2 <- mp3[P2]
temp$p3 <- mp3[P3]

#subsetear a una frecuencia mínima
temp <- filter(temp, freq > 1) #64403
tri_gram <- temp
rm(temp)
####################     FOUR   #################
four_gram  <- tokens_ngrams(x = TKNS, n = 4, concatenator = " ") 

temp <- table(unlist(four_gram)) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

temp <- temp %>% filter(Freq > 1)

mp3 <- temp$Var1 %>% 
  str_split(pattern = " ") %>% 
  unlist()
colnames(temp) <- c("frase", "freq")

P4 <- c(1:nrow(temp))*4 # 
P3 <- P4-1 
P2 <- P4-2 
P1 <- P4-3 

temp$p1 <- mp3[P1]
temp$p2 <- mp3[P2]
temp$p3 <- mp3[P3]
temp$p4 <- mp3[P4]

four_gram <-temp

#head(tri_gram)
tri_gram %>% filter(p1 =="case", p2 =="of")
four_gram %>% filter(p1 =="a", p2 =="case", p3 == "of")

#alternativamente buscar una oración 
gc()

The guy in front of me just bought a pound of bacon, a bouquet, and a case of

#tri_gram %>% filter(frase == "a case of")
#four_gram %>% filter(frase == "and a case of")

#######################################################################
##################  
temp <- grep(pattern = "and a case of", x = master) #tal vez en un subset, computacionalmente intensivo 

master[temp]

head(master)

#####################

library(ngram)
Tweet_Eng <- unlist(Tweet_Eng)
#Función para limpieza de caractéres
subset <- nchar(Tweet_Eng) > 40
Tweet_Eng <- gsub("\\d", "", Tweet_Eng[subset])
Tweet_Eng <- gsub("[[:punct:]]", "", Tweet_Eng)
#Tweet_Eng <- gsub("rt", "", Tweet_Eng)
#temp <-ngram(gsub(c("[^\x20-\x7E][\\D][[:punct:]][#]"), "", Tweet_Eng[subset][1:100]), 
#             n = 2)
#limpieza con gsub 
temp <-ngram(gsub("\\d", "", Tweet_Eng[subset][1:100]), 
             n = 2)
temp <- matrix(vctr) #el largo del maximo vector y n la entrada n 
BigramTokenizer <-   function(x) unlist(lapply(ngrams(words(x),2), paste, collapse = " "), use.names = FALSE)
temp[1]

ngram(temp[1],2)



for (j in 2:2) {
  for ( i in 1:length(tkns[[j]])) {
    tkns[[j]][i]
    vctr <-c(vctr, tkns[[j]][i])  
  }
  print(vctr)
  } 



#Transformar a un sólo data frame
class(bi_gram)
as.data.frame(bi_gram)
corpus(bi_gram)
as_tibble(bi_gram)
String_temp <- as.String(bi_gram) #de tokens a string a tibble, a separar en dos columnas y buscar en una 
tibble(String_temp)
String_temp[[1]]

#en dos columnas más subset

Ubigram <-unlist(bi_gram)




#limpieza y tokens
Master_Tokens <- tokens(x = master[1:100000],
                        remove_punct      = TRUE,
                        remove_symbols    = TRUE,
                        remove_numbers    = TRUE,
                        remove_url        = TRUE,
                        remove_separators = TRUE,
                        split_hyphens     = TRUE)

head(Master_Tokens)
library(tidytext)
temp <- Master_Tokens %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)



  
text_df %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
head(text_df)
Corpus










austen_books()
class(text_df)
austen_books() %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
Bi_grams <- unnest_ngrams(tbl = text_df,output = "bi_gram", input = "text",n = 2)
head(Bi_grams)
#contar los bigrams

temp <- Bi_grams %>% 
  count(bi_gram, sort = TRUE) %>%
  filter(n >2) %>% 
  separate(bi_gram, c("p1", "p2"), sep = " ") 


temp %>% 
  
w1 = "of"

temp %>% filter(p1 == w1)






bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts







Master_Tokens <- tokens(x = master[1:100000],
       remove_punct      = TRUE,
       remove_symbols    = TRUE,
       remove_numbers    = TRUE,
       remove_url        = TRUE,
       remove_separators = TRUE,
       split_hyphens     = TRUE)
#class(Master_Tokens)

#Master_Tokens <- tokens_select(Master_Tokens, pattern = stopwords('en'), selection = 'remove') #removing stop words
Corpus <- dfm(Master_Tokens[1:10000],  #Crear un Corpus
              tolower = TRUE, # faltaba ponerlo en minúsculas
              stem = TRUE,    # se podría poner TRUE para reducir el número de palabras distintas
              remove_punct =TRUE   )

ncol(Corpus) #número de palabras distintas
rm(PATH)
#relative one gram density

WordD   <- colSums(Corpus)
WordD[order(colSums(-Corpus))]
head(WordD[order(colSums(-Corpus))])
tail(WordD[order(colSums(-Corpus))])
NWords  <- sum(colSums(Corpus)) #5,588,342 palabras
TasaP <- (WordD/NWords)*100000
head(TasaP)
head(TasaP[order(-TasaP)]) #Ordered rate 

#######################################################
#################   First Word    #####################
#######################################################


# Relative frequency of one gram
class(TasaP)
max(TasaP)
TasaP[]


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

#dropping the n grams below a certain threshold

fcm(bi_gram)
x <- fcm(x = Corpus, size = 5, ordered = TRUE) #size es el numero de features antes y despu[es de la pregunta

#devolver una columna y una línea específicas
#droping features under a certain n 

##########################GREP de n-1 

"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"


class(bi_gram)
####Colapse bigram to term frequency

library(dplyr)
library(janeaustenr)
library(tidytext)

##########      Otra versión de los n gramas      ################# 
head(Corpus)

head(Master_Tokens)



Master_Tokens %>% unnest_tokens(bigram, docs, tokens = "ngrams", n=2 )



austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
book_bigram <- bi_gram %>%
  count(sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))



book_words <- left_join(book_words, total_words)








gregexpr(pattern = "case_of", text = bi_gram[1:1000])






unigram_probs <- hacker_news_text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))
#Feature co-ocurrence matrix (fcm)
FCMCorp <- fcm(Corpus)
FCMCorp
grep(pattern = "in", x = FCMCorp)
#eliminar palabras con bajas frequencias
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
