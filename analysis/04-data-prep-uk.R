# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(lubridate)
library(here)

# read data --------------------------------------------------------------------

covid_speeches_uk <- read_rds(here::here("data", "covid-speeches-uk.rds")) %>%
  rowid_to_column(var = "speech_no")

# new columns ------------------------------------------------------------------

covid_speeches_uk <- covid_speeches_uk %>%
  mutate(
    # name of day
    wday = wday(date, label = TRUE),
    # weekend/weekday
    weekend = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  ) %>%
  # count words
  rowwise() %>%
  mutate(n_words = text %>% str_count("\\w+") %>% sum()) %>%
  ungroup()

# tokenize words ---------------------------------------------------------------

covid_speeches_uk_words <- covid_speeches_uk %>%
  # make sure COVID-19 (and all its various spellings) don't get split
  # tidytext doesn't remove underscores
  # https://stackoverflow.com/questions/58281091/preserve-hyphenated-words-in-ngrams-analysis-with-tidytext
  mutate(
    text = str_replace_all(text, "COVID-19", "COVID_19"),
    text = str_replace_all(text, "COVID 19", "COVID_19"),
    text = str_replace_all(text, "Covid-19", "COVID_19"),
    text = str_replace_all(text, "Covid 19", "COVID_19")
  ) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# tokenize 2-grams -------------------------------------------------------------

covid_speeches_uk_bigrams <- covid_speeches_uk %>%
  # make sure COVID-19 (and all its various spellings) don't get split
  # tidytext doesn't remove underscores
  # https://stackoverflow.com/questions/58281091/preserve-hyphenated-words-in-ngrams-analysis-with-tidytext
  mutate(
    text = str_replace_all(text, "COVID-19", "COVID_19"),
    text = str_replace_all(text, "COVID 19", "COVID_19"),
    text = str_replace_all(text, "Covid-19", "COVID_19"),
    text = str_replace_all(text, "Covid 19", "COVID_19")
  ) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # drop bigrams with stopwords
  mutate(i = row_number()) %>%    # add index for later grouping
  unnest_tokens(word, bigram, drop = FALSE) %>%    # tokenize bigrams into words
  anti_join(stop_words) %>%    # drop rows with stop words
  group_by(i) %>%    # group by bigram index
  filter(n() == 2) %>%    # drop bigram instances where only one word left
  summarise(bigram = unique(bigram), .groups = "drop")

# save data --------------------------------------------------------------------

write_rds(covid_speeches_uk, file = "processed-data/covid-speeches-uk.rds")
write_rds(covid_speeches_uk_bigrams, file = "processed-data/covid-speeches-uk-bigrams.rds")
write_rds(covid_speeches_uk_words, file = "processed-data/covid-speeches-uk-words.rds")
