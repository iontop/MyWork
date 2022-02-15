rm(list = ls())
gc()

library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(themis)

raw_train <- read_csv("D:/R/competitions/nlp-getting-started/train.csv")
raw_test <- read_csv("D:/R/competitions/nlp-getting-started/test.csv")

raw_test$target <- NA
raw_test %>% mutate_if(is.logical, as.double)

raw_train$set <- "train"
raw_test$set <- "test"

raw_full <- bind_rows(raw_train, raw_test) %>% arrange(id)

raw_full <- raw_full %>% unnest_tokens(word, keyword)

raw_full <- raw_full %>% unnest_tokens(word, location)

raw_full <- raw_full %>% unnest_tokens(word, text)

data("stop_words")

raw_full <- raw_full %>% anti_join(stop_words)

raw_full <- raw_full %>% filter(!str_detect(word, "t.co"))

raw_full <- raw_full %>% filter(!str_detect(word, "http"))

raw_full <- raw_full %>% filter(!str_detect(word, "amp"))

raw_full <- raw_full %>% filter(!str_detect(word, "\\d+"))

raw_full <- raw_full %>% filter(!str_detect(word, "[^0-9A-Za-z///' ]"))

raw_full %>% count(word) %>% filter(n >= 10)

disaster_words <- 
  raw_full %>% 
  filter(target == 1) %>% 
  count(word) %>% 
  filter(n >= 10) %>% 
  arrange(-n)

nondisaster_words <- 
  raw_full %>% 
  filter(target == 0) %>% 
  count(word) %>% 
  filter(n >= 10) %>%  
  arrange(-n)

risky_word <- 
  disaster_words %>% 
  anti_join(nondisaster_words, by = "word")

neutral_word <- 
  disaster_words %>% 
  inner_join(nondisaster_words, by = "word")

safe_word <- 
  nondisaster_words %>% 
  anti_join(disaster_words, by = "word")

count(risky_word)
count(neutral_word)
count(safe_word)

risky_word %>% 
  ggplot(aes(n)) + geom_histogram()

neutral_word %>% 
  pivot_longer(-word, names_to = "tag", values_to = "n") %>% 
  ggplot(aes(n, fill = tag)) + geom_histogram(alpha = 0.4)

safe_word %>% 
  ggplot(aes(n)) + geom_histogram()

