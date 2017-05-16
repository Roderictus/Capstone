library(dplyr)
library(tidytext)
library(ggplot2)

library(tidyr)
############################################################
#################################Tokenization###############
############################################################
Blog_Eng_df <- data_frame(line = 1:100, text = Blog_Eng) #transforming from character to data frame

SentenceBlog <- Blog_Eng_df %>% 
      mutate(PostNumber = row_number()) %>%
      unnest_tokens(output = sentence, input = text,token = "sentences")
SentenceBlog <- SentenceBlog %>%
      mutate(line = row_number())
WordBlog <- SentenceBlog %>%
      unnest_tokens(output = word, input = sentence)
WordBlog<-mutate(WordBlog, word_number = row_number())

########Removing stop Words, and counting 
#data("stop_words")
WordBlogSW<- WordBlog %>%
      count(word, sort = TRUE) %>%
      anti_join(stop_words)

############Counting words
WordBlog %>%
      count(word, sort = TRUE)
############A graph of most common words

WordBlog %>%
      count(word, sort = TRUE) %>%
      filter(n >100) %>%
      mutate(word = reorder(word, n )) %>%
      ggplot(aes(word,n)) + geom_bar(stat = "identity") + xlab(NULL) + coord_flip()

#############looking at other novels
#install.packages("gutenbergr")
library(gutenbergr)
hgwells <- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words)

tidy_hgwells %>%
      count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words)

tidy_bronte %>%
      count(word, sort = TRUE)

##########################################################
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
      group_by(book) %>%
      mutate(linenumber = row_number(),
             chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                     ignore_case = TRUE)))) %>%
      ungroup() 


original_books
##########################################################

library(tidytext)
tidy_books <- original_books %>%
      unnest_tokens(word, text)

tidy_books
##########################################################
data(stop_words)

tidy_books <- tidy_books %>%
      anti_join(stop_words)


#########################################################
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
      mutate(word = str_extract(word, "[a-z']+")) %>%
      count(author, word) %>%
      group_by(author) %>%
      mutate(proportion = n / sum(n)) %>% 
      select(-n) %>% 
      spread(author, proportion) %>% 
      gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

##################################################################

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      facet_wrap(~author, ncol = 2) +
      theme(legend.position="none") +
      labs(y = "Jane Austen", x = NULL)

###################################################################

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

############################Sentiment analysis with tidy data

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
      group_by(book) %>%
      mutate(linenumber = row_number(),
             chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                     ignore_case = TRUE)))) %>%
      ungroup() %>%
      unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>% 
      filter(sentiment == "joy")
#

tidy_books %>%
      filter(book == "Emma") %>%
      inner_join(nrcjoy) %>%
      count(word, sort = TRUE)

########sentimiento neto

janeaustensentiment <- tidy_books %>%
      inner_join(get_sentiments("bing")) %>%
      count(book, index = linenumber %/% 80, sentiment) %>%
      spread(sentiment, n , fill = 0) %>%
      mutate(sentiment = positive - negative)
#############################

library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
      geom_bar(show.legend = FALSE, stat = "identity") +
      facet_wrap(~book, ncol = 2, scales = "free_x")

###############################

pride_prejudice <- tidy_books %>%
      filter(book == "Pride & Prejudice")

pride_prejudice
########################## Using afinn
afinn <- pride_prejudice %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(index = linenumber %/% 80 ) %>%
      summarise(sentiment = sum(score)) %>%
      mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>%
                                inner_join(get_sentiments("bing")) %>%
                                mutate(method = "Bing et al."),
                          pride_prejudice %>%
                                inner_join(get_sentiments("nrc") %>%
                                                 filter(sentiment %in% c("positive", 
                                                                         "negative"))) %>%
                                mutate(method = "NRC")) %>%
      count(method, index = linenumber %/% 80, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
###bind and visualize
bind_rows(afinn, bing_and_nrc) %>%
      ggplot(aes(index, sentiment, fill = method)) + geom_bar(stat = "identity",show.legend = FALSE) + facet_wrap(~method, ncol = 1, scales = "free_y")
##

get_sentiments("nrc") %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(sentiment)

get_sentiments("bing") %>%
      count(sentiment)

#most common positive and negative words

bing_word_counts <- tidy_books %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()

######
bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word,n,fill = sentiment)) +
      geom_bar(stat= "identity", show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_x") + labs(y = "Contribution to sentiment", 
                                                       x = NULL) 
#################

custom_stop_words <- bind_rows(data_frame(word = c("miss"),
                                          lexicon = c("custom")),
                               stop_words)
custom_stop_words
#############################
install.packages("wordcloud")
library(wordcloud)
tidy_books %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
################################
library(reshape2)
tidy_books %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)
#########################
PandP_sentences <- data_frame(text = prideprejudice) %>%
      unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]


austen_chapters <- austen_books() %>%
      group_by(book) %>%
      unnest_tokens(chapter, text, token = "regex", 
                    pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
      ungroup()

austen_chapters %>% 
      group_by(book) %>% 
      summarise(chapters = n())

###Proportion of negative words
bingnegative <- get_sentiments("bing") %>%
      filter(sentiment == "negative")

wordcounts <- tidy_books %>%
      group_by(book, chapter) %>%
      summarize(words = n())

tidy_books %>%
      semi_join(bingnegative) %>%
      group_by(book, chapter) %>%
      summarize(negativewords = n()) %>%
      left_join(wordcounts, by = c("book", "chapter")) %>%
      mutate(ratio = negativewords/words) %>%
      filter(chapter != 0) %>%
      top_n(1) %>%
      ungroup()
####################
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
      unnest_tokens(word, text) %>%
      count(book, word, sort = TRUE) %>%
      ungroup()

total_words <- book_words %>%
      group_by(book) %>%
      summarize( total = sum(n))
book_words <- left_join(book_words, total_words)
book_words
##################

library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) + geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
      facet_wrap(~book, ncol =2, scales = "free_y")

#######

freq_by_rank <-book_words %>%
      group_by(book) %>%
      mutate(rank = row_number(), 'term frequency' = n/total)

freq_by_rank
##########################

freq_by_rank %>% 
      ggplot(aes(rank, `term frequency`, color = book)) + 
      geom_line(size = 1.2, alpha = 0.8) + 
      scale_x_log10() +
      scale_y_log10()

####
rank_subset <- freq_by_rank %>%
      filter(rank < 500,
             rank > 10)
lm(log10(`term frequency`)~log10(rank), data = rank_subset)
##########################
freq_by_rank %>% 
      ggplot(aes(rank, `term frequency`, color = book)) + 
      geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
      geom_line(size = 1.2, alpha = 0.8) + 
      scale_x_log10() +
      scale_y_log10()

#################################
book_words <- book_words %>%
      bind_tf_idf(word, book, n) 
book_words


book_words %>%
      select(-total) %>%
      arrange(desc(tf_idf))

#################################

plot_austen <- book_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))

plot_austen %>%
      top_n(20) %>%
      ggplot(aes(word, tf_idf, fill = book)) +
      geom_bar(stat = "identity") +
      labs(x = NULL, y = "tf-df") +
      coord_flip()
###################################

plot_austen %>% 
      group_by(book) %>% 
      top_n(15) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = book)) +
      geom_bar(show.legend = FALSE, stat ="identity") +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~book, ncol = 2, scales = "free") +
      coord_flip() + 0coord_cartesian(ylim = c(0,.009))
###############################################################
library(gutenbergr)
physics <- gutenberg_download(c(37729,14725,13476,5001), meta_fields = "author")

physics_words <- physics %>%
      unnest_tokens(word, text) %>%
      count(author, word, sort = TRUE) %>%
      ungroup()

physics_words <- physics_words %>%
      bind_tf_idf(word, author, n)

plot_physics <- physics_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                "Huygens, Christiaan", 
                                                "Tesla, Nikola",
                                                "Einstein, Albert")))
library(ggplot2)

plot_physics %>%
      top_n(20) %>%
      ggplot(aes(word, tf_idf, fill = author)) +
      geom_col() +
      labs(x = NULL, y = "tf-idf") +
      coord_flip()

#########################
plot_physics %>%
      group_by(author) %>%
      top_n(15, tf_idf) %>%
      ungroup() %>%
      mutate(word = reorder (word, tf_idf)) %>%
      ggplot(aes(word, tf_idf, fill =author)) +
      geom_col(show.legend = FALSE) + 
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~author, ncol = 2, scales = "free")+ 
      coord_flip()
##
library(stringr)

physics %>%
      filter(str_detect(text, "eq\\.")) %>%
      select(text)

physics %>%
      filter(str_detect(text, "K1")) %>%
      select(text)

physics %>% 
      filter(str_detect(text, "AK")) %>% 
      select(text)

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")
plot_physics <- physics_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      group_by(author) %>% 
      top_n(15, tf_idf) %>%
      ungroup %>%
      mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                "Huygens, Christiaan",
                                                "Tesla, Nikola",
                                                "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~author, ncol = 2, scales = "free") +
      coord_flip()

#################################################################################
#########################Relationships between words: N-grams and correlations 
#################################################################################

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
austen_bigrams

######removing uninteresting bigrams
library(tidyr)

bigrams_separated <- austen_bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
      
bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
      unite(bigram, word1, word2, sep = " ")

bigrams_united

#trigrams
austen_books() %>%
      unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word,
             !word3 %in% stop_words$word) %>%
      count(word1, word2, word3, sort = TRUE)
      
bigrams_filtered %>%
      filter(word2 == "street") %>%
      count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
      count(book, bigram) %>%
      bind_tf_idf(bigram, book, n) %>%
      arrange(desc(tf_idf))

bigram_tf_idf

AFINN <- get_sentiments("afinn")
AFINN

not_words <- bigrams_separated %>%
      filter(word1 == "not") %>%
      inner_join(AFINN, by = c(word2 = "word")) %>%
      count(word2, score, sort = TRUE) %>%
      ungroup()
not_words

library(ggplot2)
library(dplyr)

not_words %>%
      mutate(contribution = n * score) %>%
      arrange(desc(abs(contribution))) %>%
      head(20) %>%
      mutate(word2 = reorder(word2, contribution)) %>%
      ggplot(aes(word2, n * score, fill = n * score > 0)) +
      geom_col(show.legend = FALSE) +
      xlab("Words preceded by \"not\"") + 
      ylab("Sentiment score * number of occurences") +
      coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
      filter(word1 %in% negation_words) %>%
      inner_join(AFINN, by = c(word2 = "word")) %>%
      count(word1, word2, score, sort = TRUE) %>%
      ungroup()

install.packages("igraph")
library(igraph)
bigram_counts

bigram_graph <- bigram_counts %>%
      filter(n > 20) %>%
      graph_from_data_frame()
bigram_graph


install.packages("ggraph")
library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                     arrow =a, end_cap = circle(.07, "inches")) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label=name), vjust =1, hjust =1 ) +
      theme_void()

################################Visualizing bigrams in other texts 

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
      dataset %>%
            unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>%
            filter(!word1 %in% stop_words$word, 
                   !word2 %in% stop_words$word) %>%
            count(word1, word2, sort = TRUE) 
}

visualize_bigrams <- function(bigrams) {
      set.seed(2016)
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

      bigrams %>%
            graph_from_data_frame() %>%
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a ) +
            geom_node_point(color = "lightblue", size =5) +
            geom_node_text(aes(label = name), vjust =1, hjust =1) +
            theme_void()
      }

###################King James version of the bible
library(gutenbergr)
library(stringr)
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>%
      count_bigrams()

kjv_bigrams %>%
      filter( n > 40, !str_detect(word1, "//d"),
              !str_detect(word2, "//d")) %>%
      visualize_bigrams()

######Counting and correlating pairs of words with the widyr package

library(janeaustenr)
library(tidyverse)
library(dplyr)

austen_section_words <- austen_books() %>%
      filter(book == "Pride & Prejudice") %>%
      mutate(section = row_number() %/% 10) %>%
      filter(section > 0) %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word)

library(devtools)
install.packages("devtools")

install_github("dgrtwo/widyr")

install.packages("widyr")
library(widyr)


word_pairs <- austen_section_words %>%
      pairwise_count(word, section, sort = TRUE)

word_pairs %>%
      filter(item1 == "darcy")

#Pairwise correlation

word_cors <- austen_section_words %>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, section, sort = TRUE)
library(stringr)
word_cors


word_cors %>%
      filter(item1 == "pounds")

word_cors %>%
      filter(item1 %in% c("elizabeth", "punds", "married", "pride")) %>%
      group_by(item1) %>%
      top_n(6) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat ="identity") +
      facet_wrap(~item, scales = "free") +
      coord_flip()

set.seed(2016)

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)

word_cors %>%
      filter(correlation > .15) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()


#Converting to and non tidy formats
#CRAN task VIew for Natural Language Processing
