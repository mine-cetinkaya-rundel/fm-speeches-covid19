# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(lubridate)
library(here)
library(scales)
library(glue)

# set theme for plots to minimal -----------------------------------------------

theme_set(theme_minimal())

# set color --------------------------------------------------------------------
# https://www.schemecolor.com/flag-of-scotland-colors.php

scotblue <- "#0065BF"

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
  # count paragraphs and words
  rowwise() %>%
  mutate(
    n_paragraphs = unlist(text) %>% length(),
    n_words      = unlist(text) %>% str_count("\\w+") %>% sum()
  ) %>%
  ungroup()

# keep only speeches by the FM -------------------------------------------------

covid_speeches_scot <- covid_speeches_scot %>%
  # only speeches by the FM (exclude ministerial statement & health secretary)
  filter(speaker == "First Minister") %>%
  # tag speech number
  rowid_to_column(var = "speech_no")

# how long do they speak? ------------------------------------------------------

# number of paragraphs ----

ggplot(covid_speeches_scot, aes(x = n_paragraphs)) +
  geom_density(color = scotblue, fill = scotblue, alpha = 0.5) +
  labs(
    title = "Distribution of number of paragphs",
    subtitle = "of First Minister's daily briefings",
    x = "Number of paragraphs",
    y = "Density"
  )

ggplot(covid_speeches_scot, aes(x = n_words)) +
  geom_density(color = scotblue, fill = scotblue, alpha = 0.5) +
  labs(
    title = "Distribution of number of words",
    subtitle = "of First Minister's daily briefings",
    x = "Number of words",
    y = "Density"
  )

# number of paragraphs vs. date ----

lm_paragraphs <- lm(n_paragraphs ~ date, data = covid_speeches_scot)
lm_paragraphs_rsq <- glance(lm_paragraphs)$r.squared

covid_speeches_scot %>%
  ggplot(aes(x = date, y = n_paragraphs)) +
  geom_point(color = scotblue, alpha = 0.7) +
  geom_smooth(aes(x = date, y = n_paragraphs), method = lm, 
              formula = y ~ x, color = "darkgray") +
  labs(
    title = "Length of First Minister's COVID-19 speeches",
    subtitle = glue("Measured in number of paragraphs, R-squared = {percent(lm_paragraphs_rsq)}"),
    x = NULL, y = "Number of paragraphs", color = NULL, shape = NULL
  )

# number of words vs. date ----

lm_words <- lm(n_words ~ date, data = covid_speeches_scot)
lm_words_rsq <- glance(lm_words)$r.squared

covid_speeches_scot %>%
  ggplot(aes(x = date, y = n_words)) +
  geom_point(color = scotblue, alpha = 0.7) +
  geom_smooth(aes(x = date, y = n_words), method = lm, formula = y ~ x, color = "darkgray") +
  labs(
    title = "Length of First Minister's COVID-19 speeches",
    subtitle = glue("Measured in number of words, R-squared = {percent(lm_words_rsq)}"),
    x = NULL, y = "Number of words", color = NULL, shape = NULL
  )

# word analysis ----------------------------------------------------------------

covid_speeches_scot_paragraphs <- covid_speeches_scot %>%
  unnest_longer(
    col = text,
    values_to = "paragraph",
    indices_to = "paragraph_no"
  ) %>%
  group_by(speech_no) %>%
  mutate(paragraph_no = row_number()) %>%
  ungroup()

covid_speeches_scot_words <- covid_speeches_scot_paragraphs %>%
  # make sure COVID-19 (and all its various spellings) don't get split
  # tidytext doesn't remove underscores
  # https://stackoverflow.com/questions/58281091/preserve-hyphenated-words-in-ngrams-analysis-with-tidytext
  mutate(
    paragraph = str_replace_all(paragraph, "COVID-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "COVID 19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid 19", "COVID_19")
  ) %>%
  unnest_tokens(word, paragraph) %>%
  anti_join(stop_words)

covid_speeches_scot_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of words in First Minister's COVID-19 briefings",
    subtitle = "Words occurring more than 200 times",
    y = NULL, x = NULL
  )

# sentiment analysis -----------------------------------------------------------

## bing: positive / negative ----

covid_speeches_scot_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 20) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(
    title = "Sentiment and frequency of words in First Minister's COVID-19 briefings",
    subtitle = "Sentiment evaluated using the Bing lexicon",
    y = NULL, x = NULL
  )

# remove the word "positive" since it's not really positive

covid_speeches_scot_words <- covid_speeches_scot_words %>%
  filter(word != "positive")

# plot again

covid_speeches_scot_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 20) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(
    title = "Sentiment and frequency of words in First Minister's COVID-19 briefings",
    subtitle = "Sentiment evaluated using the Bing lexicon",
    y = NULL, x = NULL
  )


covid_speeches_scot_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(
    sentiment = positive - negative,
    wednesday = if_else(wday(date, label = TRUE) == "Wed", "Wednesday", "Non-Wednesday")
    ) %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(color = "gray") +
  geom_point(aes(shape = wednesday, color = sentiment > 0), size = 2) +
  #scale_color_manual(values = c("gray", "red")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
  guides(color = FALSE) +
  labs(
    title = "Sentiment score of words in First Minister's COVID-19 briefings over time",
    subtitle = "Sentiment score calculated as the difference between the number of words with positive and negative sentiments according to the Bing lexicon",
    caption = "On Wednesdays, National Records of Scotland report that captures all deaths with a confirmed 
    laboratory diagnosis of COVID-19 well as deaths which are presumed to be linked to the virus.",
    x = "Date", y = "Sentiment score", shape = NULL
  ) +
  theme(legend.position = "bottom")

# nrc: trust, fear, negative, sadness, anger, surprise, positive, disgust, joy, anticipation ----

covid_speeches_scot_words %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(
    sentiment = fct_relevel(sentiment, "positive", "anticipation", "joy", "surprise", "trust",
                            "negative", "anger", "disgust", "fear", "sadness"),
    sentiment_binary = if_else(sentiment %in% c("positive", "anticipation", "joy", "surprise", "trust"), "positive", "negative")
  ) %>%
  count(sentiment_binary, sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment_binary)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 5) +
  labs(
    title = "Sentiment and frequency of words in First Minister's COVID-19 briefings",
    subtitle = "Sentiment evaluated using the NRC lexicon",
    y = NULL, x = NULL
  )

covid_speeches_scot_words %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(
    sentiment = fct_relevel(sentiment, "positive", "anticipation", "joy", "surprise", "trust",
                            "negative", "anger", "sadness", "disgust", "fear"),
    sentiment_binary = if_else(sentiment %in% c("positive", "anticipation", "joy", "surprise", "trust"), "positive", "negative")
    ) %>%
  count(date, sentiment_binary, sentiment) %>%
  ggplot(aes(x = date, y = n, color = sentiment_binary)) +
  geom_line(size = 0.3) +
  guides(color = FALSE) +
  facet_wrap(~ sentiment, ncol = 5) +
  labs(
    title = "Sentiment score of words in First Minister's COVID-19 briefings over time",
    subtitle = "Sentiment score calculated as number of words associated with a given sentiment according to the NRC lexicon",
    x = "Date", y = "Sentiment score", shape = NULL, color = NULL
  ) +
  theme_bw()

# 2-grams ----------------------------------------------------------------------

covid_speeches_scot_bigrams <- covid_speeches_scot_paragraphs %>%
  # make sure COVID-19 (and all its various spellings) don't get split
  # tidytext doesn't remove underscores
  # https://stackoverflow.com/questions/58281091/preserve-hyphenated-words-in-ngrams-analysis-with-tidytext
  mutate(
    paragraph = str_replace_all(paragraph, "COVID-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "COVID 19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid 19", "COVID_19")
  ) %>%
  unnest_tokens(bigram, paragraph, token = "ngrams", n = 2) %>%
  # drop bigrams with stopwords
  mutate(i = row_number()) %>%    # add index for later grouping
  unnest_tokens(word, bigram, drop = FALSE) %>%    # tokenize bigrams into words
  anti_join(stop_words) %>%    # drop rows with stop words
  group_by(i) %>%    # group by bigram index
  filter(n() == 2) %>%    # drop bigram instances where only one word left
  summarise(bigram = unique(bigram))

covid_speeches_scot_bigrams %>%
  mutate(
    bigram = if_else(bigram == "care home", "care home(s)", bigram),
    bigram = if_else(bigram == "care homes", "care home(s)", bigram)
  ) %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 30) %>%
  ggplot(aes(y = fct_reorder(bigram, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of bigrams in First Minister's COVID-19 briefings",
    subtitle = "Bigrams occurring more than 30 times",
    y = NULL, x = NULL
  )

# social to physical distancing

covid_speeches_scot_paragraphs %>%
  # make sure COVID-19 (and all its various spellings) don't get split
  # tidytext doesn't remove underscores
  # https://stackoverflow.com/questions/58281091/preserve-hyphenated-words-in-ngrams-analysis-with-tidytext
  mutate(
    paragraph = str_replace_all(paragraph, "COVID-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "COVID 19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid-19", "COVID_19"),
    paragraph = str_replace_all(paragraph, "Covid 19", "COVID_19")
  ) %>%
  unnest_tokens(bigram, paragraph, token = "ngrams", n = 2) %>%
  filter(str_detect(bigram, "social dist|physical dist")) %>%
  mutate(soc_phys = if_else(str_detect(bigram, "social"), "S", "P")) %>%
  count(date, soc_phys) %>%
  ggplot(aes(x = date, y = n, color = soc_phys)) +
  geom_text(aes(label = soc_phys)) +
  guides(color = FALSE) +
  labs(x = "Date", y = "Frequency",
       title = "Social (S) vs. physical (P) distancing",
       subtitle = "Number of mentions over time") +
  scale_color_manual(values = c(scotblue, "darkgray"))

# save calculated objects ------------------------------------------------------

write_rds(covid_speeches_scot, path = "processed-data/covid_speeches_scot.rds")
write_rds(covid_speeches_scot_bigrams, path = "processed-data/covid_speeches_scot_bigrams.rds")
write_rds(covid_speeches_scot_paragraphs, path = "processed-data/covid_speeches_scot_paragraphs.rds")
write_rds(covid_speeches_scot_words, path = "processed-data/covid_speeches_scot_words.rds")
