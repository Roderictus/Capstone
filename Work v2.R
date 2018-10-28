library(tidyverse)
library(igraph)
library(tidytext)
library(tidyr)

#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera_Swiftkey.zip")
#list.files("./final/en_US")
#unzip("Coursera_Swiftkey.zip")
##Reading files

Blog_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.blogs.txt"))
News_Eng<-tbl_df(read_lines(file = "Data/final/en_US/en_US.news.txt"))
Tweet_Eng<-readLines(con =  "Data/final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
Tweet_Eng <- tbl_df(Tweet_Eng)
#Basic data Table 

###Tokenizing into sentences and words
#Transforming from character to data frame 

#Tokenizing
Token_Blog <- Blog_Eng %>%
  mutate(Line = row_number(),source = "Blog_Eng") %>%
  unnest_tokens(output = word, input = value)
Token_News <- News_Eng %>%
  mutate(Line = row_number(), source = "News_Eng") %>%
  unnest_tokens(output = word, input = value)
Token_Tweet <- Tweet_Eng %>%
  mutate(Line = row_number(), source = "Tweet_Eng") %>%
  unnest_tokens(output = word, input = value)

#Tidy and Custom Tidy
c("1","2","3","4","5","6","7","8","9","0")
temp <- data.frame(c("1","2","3","4","5","6","7","8","9","0"), "Custom")
colnames(temp) <- c("word", "lexicon")
data(stop_words)
stop_words <- rbind(stop_words, temp)

tidy_Blog <- Token_Blog %>%
       mutate(source = "English Blog") %>%
      anti_join(stop_words)
tidy_News <- Token_News %>%
      mutate(source = "English News") %>%
      anti_join(stop_words)
tidy_Tweet <- Token_Tweet %>%
      mutate(source = "English News") %>%
      anti_join(stop_words)

A <- c("", "Blog", "News", "Tweet")
B <- c("N_Entries", nrow(Blog_Eng), nrow(News_Eng), nrow(Tweet_Eng))
C <- c("N_Words", nrow(Token_Blog), nrow(Token_News), nrow(Token_Tweet))
D <- c("Tidy_Words", nrow(tidy_Blog), nrow(tidy_News), nrow(tidy_Tweet))

T1<-cbind(cbind(cbind(A, B),C),D)
T1 <- T1[-1,]
colnames(T1) <- c("Source", "N_Entries", "N_Words", "Tidy_Words")
write.table(x = T1, file = "Table1.csv")
T1 <- read.table(file = "Table1.csv")
#rm(Blog_Eng)
#rm(News_Eng)
#rm(Tweet_Eng)
#gc()

################################################################################

frequency <- bind_rows(mutate(tidy_Blog, Source = "Blog"),
                       mutate(tidy_News, Source = "News"),
                       mutate(tidy_Tweet, Source = "Tweet")) %>%
  mutate(word = str_extract(word, "[a-z`]+")) %>%
  count(Source, word) %>%
  group_by(Source) %>%
  mutate(proportion = n/ sum(n)) %>%
  select(-n) %>%
  spread(Source, proportion) %>%
  gather(Source, proportion, "Blog":"Tweet" )
frequency %>%
  arrange(-proportion)

#################################################################################

Blog_count <- tidy_Blog %>% 
  count(word, sort = TRUE) %>%
  top_n(10,n) 
Blog_count$total <- nrow(tidy_Blog)
Blog_count$freq <- (Blog_count$n/Blog_count$total)*10000 #ocurrences for every 10,000 words
Blog_count <- Blog_count[,-3] 

News_count <- tidy_News %>% 
  count(word, sort = TRUE) %>%
  top_n(10,n)
News_count$total <- nrow(tidy_News)
News_count$freq <- (News_count$n/News_count$total)*10000 #ocurrences for every 10,000 words
News_count<-News_count[,-3]  


Tweet_count <- tidy_Tweet %>% 
  count(word, sort = TRUE) %>%
  top_n(10,n) 
Tweet_count$total <- nrow(tidy_Tweet)
Tweet_count$freq <- (Tweet_count$n/Tweet_count$total)*10000 #ocurrences for every 10,000 words
Tweet_count <- Tweet_count[,-3]  

colnames(Blog_count) <- c("Top_Blog", "N_Blog", "Freq_Blog")
colnames(News_count) <- c("Top_News", "N_News", "Freq_News")
colnames(Tweet_count) <- c("Top_Tweet", "N_Tweet", "Freq_Tweet")

T2 <- cbind(cbind(Blog_count, News_count), Tweet_count)
write.csv(x = T2, file = "Table2.csv")
T2<-read.csv(file = "Table2.csv")
T2

###########################Bigrams###########################################

Blog_Bigram <- Blog_Eng %>% unnest_tokens(bigram, value, token = "ngrams", n = 2)

CB <- Blog_Bigram %>%
  count(bigram, sort = TRUE)

CB %>% top_n(15) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(bigram,n)) +
  geom_col(show.legend = FALSE) + 
  labs(y = "Bigram", x = NULL) + 
  ggtitle("Bigram Ocurrence in Blog Data Set, With Stop Words") +
  coord_flip()

CB_tidy <- Blog_Bigram %>%
    count(bigram, sort = TRUE)
  

#deleting and models subsetting
#number of diferent words, numbr of diferent bigrams, trigrams, computational efficiency






