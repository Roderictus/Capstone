#Data
library(tidyverse)
#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera_Swiftkey.zip")
#list.files("./final/en_US")
#unzip("Coursera_Swiftkey.zip")

Blog_Eng<-read_lines(file = "./final/en_US/en_US.blogs.txt")
News_Eng<-read_lines(file = "./final/en_US/en_US.news.txt")
Tweet_Eng<-read_lines(file = "./final/en_US/en_US.twitter.txt")

#Internet resource with "bad words"
#download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", "profanity.txt")

#All the dirty words from google[s what do you love project
#download.file("https://gist.githubusercontent.com/jamiew/1112488/raw/7ca9b1669e1c24b27c66174762cb04e14cf05aa7/google_twunter_lol", "Google_bad_words.txt")

