# Begin


library(tidytext)
library(tidyverse)
library(textstem)
library(ggwordcloud)
library(gutenbergr)


# This function allows for quick calculation of Term Frequency - Inverse Document Frequency
lazytf <- function (data, word = "word", grouping_factor) {
  qgv <- enquo (grouping_factor)
  word <- enquo (word)
  data %>%
    group_by(!!qgv) %>%
    count (!!qgv, !!word, sort = TRUE) %>%
    ungroup() %>%
    mutate (total = sum(n)) %>%
    bind_tf_idf (., !!word, !!qgv, n)
  
}



# Want to analyse some essays on education:
gutenberg_metadata %>%
  filter(gutenberg_id == "36774")

gutenberg_get_mirror()



# Unsure why gutenberg_download isn't working
# freetext <- gutenberg_download(36774)



# These lines of code read the text file and split it into three essay collections.

rawtext <-RCurl::getURL("http://www.gutenberg.org/cache/epub/36774/pg36774.txt") %>% 
  as.tibble() %>% 
  rename(text = value) %>% 
  mutate(text = strsplit(as.character(text), "PROBLEMS OF SCHOOL AND COLLEGE")) %>% 
  unnest(text) %>% 
  mutate(text = strsplit(as.character(text), "HARVARD PAPERS")) %>% 
  unnest(text) %>% 
  mutate(text = strsplit(as.character(text), "PAPERS BY ALICE FREEMAN PALMER")) %>% 
  unnest(text) %>% 
  mutate(text = strsplit(as.character(text), "The Riverside Press")) %>% 
  unnest(text) %>% 
  mutate(collection = c("prelim", "prelim", "prelim", "prelim", "prelim", "PROBLEMS OF SCHOOL AND COLLEGE",
                        "HARVARD PAPERS", "PAPERS BY ALICE FREEMAN PALMER", "prelim")) %>% 
  filter(!collection == "prelim") 


my_stops <- tibble(word = c("gutenberg", "project"),
                   lexicon = c("my_stops", "my_stops"))
my_stops<-full_join(stop_words, my_stops)



# Begin textmining process


freetext <- rawtext %>% 
  unnest_tokens(word, text) %>%
  mutate (lemma = (lemmatize_strings(word))) %>%
  anti_join(my_stops)


# What words do we see most often in these collections?


freetext %>% 
  count(lemma, sort = T) %>% 
  top_n(20) %>% 
  ggplot(aes(x = reorder(lemma,n), y = n, fill = as.factor(n))) +
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  labs(y = "Count of Word", x = "Word (Lemmatised)",
       title = "Frequency of words across\n'The Teacher: Essays and Addresses on Education by Palmer and Palmer'",
       subtitle = "Words are lemmatised") +
  theme(legend.position = "none")


# Differences between the essay collections?

freetext %>% 
  count(collection, lemma, sort = T) %>% 
  top_n(20) %>% 
  ggplot(aes(x = reorder(lemma,n), y = n, fill = as.factor(n))) +
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  facet_wrap(facets = ~ collection, scales = "free", nrow = 3) +
  labs(y = "Count of Word", x = "Word (Lemmatised)",
       title = "Frequency of words across\n'The Teacher: Essays and Addresses on Education by Palmer and Palmer'",
       subtitle = "Words are lemmatised") +
  theme(legend.position = "none")



# What about TF-IDF

freetf <- lazytf(freetext, word = lemma, grouping_factor = collection)


freetf %>% 
  arrange(desc (tf_idf)) %>%
  mutate (lemma = factor (lemma, levels = rev(unique(lemma)))) %>%
  group_by (collection) %>%
  top_n(7, tf_idf) %>%
  ungroup()  %>% 
  ggplot(aes(lemma, tf_idf, fill = lemma)) +
  geom_col(show.legend = FALSE) +
  labs (x = NULL, y = "Term Frequency - Inverse Document (Question) Frequency") +
  facet_wrap(~collection, nrow = 3, scales = "free") +
  theme_classic() +
  coord_flip()
