# Begin


library(tidytext)
library(tidyverse)
library(textstem)
library(ggwordcloud)
library(gutenbergr)

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
  mutate(collection = c("prelim", "prelim", "prelim", "prelim", "PROBLEMS OF SCHOOL AND COLLEGE",
                        "HARVARD PAPERS", "PAPERS BY ALICE FREEMAN PALMER")) %>% 
  filter(!collection == "prelim")


my_stops <- tibble(word = c("gutenberg", "project"),
                   lexicon = c("my_stops", "my_stops"))
my_stops<-full_join(stop_words, my_stops)



# Begin textmining process


freetext <- rawtext %>% 
  unnest_tokens(word, text) %>%
  mutate (lemma = (lemmatize_strings(word))) %>%
  anti_join(my_stops)



freetext %>% 
  group_by(collection, lemma) %>% 
  summarise(n=n()) %>% 
  group_by(collection) %>% 
  summarise(total = sum(n))

freetext %>% 
  group_by(collection) %>% 
  mutate(coll_total = n()) %>% 
  ungroup() %>% 
  group_by(collection, lemma) %>% 
  summarise(n = n(),
            perc = (round(100 * n/coll_total, 0))) %>% 
  View()



freetext %>% 
  group_by(collection) %>% 
  count(lemma)


freetext %>% 
  count(collection, lemma, sort = T) %>% 
  top_n(20) %>% 
  ggplot(aes(x = reorder(lemma,n), y = n, fill = as.factor(n))) +
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  facet_wrap(facets = ~ collection, scales = "free", nrow = 3)



freetext %>% 
  group_by(collection) %>% 
  mutate(coll_total = n()) %>% 
  ungroup() %>% 
  group_by(collection, lemma) %>% 
  summarise(n = n(),
            perc = (round(100 * n/coll_total, 0))) %>%
  top_n(20) %>% 
  ggplot(aes(x = reorder(lemma,perc), y = perc, fill = perc)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  facet_wrap(facets = ~ collection, scales = "free", nrow = 3)

