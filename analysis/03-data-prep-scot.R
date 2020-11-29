# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(lubridate)
library(here)

# read data --------------------------------------------------------------------

covid_speeches_scot <- read_rds(here::here("data", "covid-speeches-scot.rds"))

# new columns ------------------------------------------------------------------

covid_speeches_scot <- covid_speeches_scot %>%
  mutate(
    # name of day
    wday = wday(date, label = TRUE),
    # weekend/weekday
    weekend = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    # speaker
    speaker = case_when(
      str_detect(abstract, "First Minister")   ~ "First Minister",
      str_detect(abstract, "Health Secretary") ~ "Health Secretary",
      TRUE                                     ~ NA_character_
    )
  ) %>%
  # count words
  rowwise() %>%
  mutate(n_words = text %>% str_count("\\w+") %>% sum()) %>%
  ungroup()

# keep only speeches by the FM -------------------------------------------------

covid_speeches_scot <- covid_speeches_scot %>%
  # only speeches by the FM (exclude ministerial statement & health secretary)
  filter(speaker == "First Minister") %>%
  # tag speech number
  rowid_to_column(var = "speech_no")

# tokenize words ---------------------------------------------------------------

covid_speeches_scot_words <- covid_speeches_scot %>%
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

covid_speeches_scot_bigrams <- covid_speeches_scot %>%
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

write_rds(covid_speeches_scot, file = "processed-data/covid-speeches-scot.rds")
write_rds(covid_speeches_scot_bigrams, file = "processed-data/covid-speeches-scot-bigrams.rds")
write_rds(covid_speeches_scot_words, file = "processed-data/covid-speeches-scot-words.rds")
