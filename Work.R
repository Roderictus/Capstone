 #Packages
#library(dplyr)
#library(tidyr)
#library(tidytext)
library(tidyverse)
#library(ggplot2)
library(igraph)
library(ggraph)
library(tidytext)

#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera_Swiftkey.zip")
#list.files("./final/en_US")
#unzip("Coursera_Swiftkey.zip")


##Reading files
Blog_Eng<-tbl_df(read_lines(file = "./final/en_US/en_US.blogs.txt"))
News_Eng<-tbl_df(read_lines(file = "./final/en_US/en_US.news.txt"))
Tweet_Eng<-readLines(con =  "./final/en_US/en_US.twitter.txt",encoding = "UTF16-LE", skipNul = TRUE)
Tweet_Eng <- tbl_df(Tweet_Eng)
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

rm(Blog_Eng)
rm(News_Eng)
rm(Tweet_Eng)
gc()


All <- rbind(Token_Blog, Token_News)
All <- rbind(All, Token_Tweet) #posiblemente demasiado grande para r

rm(Token_Tweet)
gc()

write.table(All, file = "Token_All.csv", sep = ",")
gc()


#removing stop words
data(stop_words)

Tidy_All <- All %>%
      anti_join(stop_words)
rm(All)
gc()

#tidy_Blog <- Token_Blog %>%
#      mutate(source = "English Blog") %>%
#      anti_join(stop_words)

#tidy_News <- Token_News %>%
#      mutate(source = "English News") %>%
#      anti_join(stop_words)

#tidy_Tweet <- Token_Tweet %>%
#      mutate(source = "English News") %>%
#      anti_join(stop_words)

#joining data sets
#Tidy_All<- bind_rows(tidy_Blog, tidy_News)
#Tidy_All <- bind_rows(Tidy_All,tidy_Tweet)
write.table(Tidy_All, "./Tidy_All_English2.txt", sep = "\t")
#gc()
#Tidy_all<-read.table("Tidy_All_English.txt")

####Most common non stop-words all DataSets



#characteristic words for the texts inverse word frequency

Tidy_All %>%
      count(word, sort = TRUE) %>%
      filter(n >40000) %>%
      mutate(word = reorder(word, n )) %>%
      ggplot(aes(word,n)) +
      geom_col() + 
      coord_flip() 


Source_Words <- Tidy_All %>%
      count(source, word, sort = TRUE) %>%
      ungroup()

Total_Words <- Source_Words %>%
      group_by(source) %>%
      summarize(total = sum(n))




