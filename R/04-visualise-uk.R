# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(lubridate)
library(here)
library(scales)
library(glue)
library(wordcloud)

# set theme for plots to minimal -----------------------------------------------

theme_set(theme_minimal())

# read data --------------------------------------------------------------------

covid_speeches_uk <- read_rds(here::here("data", "covid-speeches-uk.rds")) %>%
  rowid_to_column(var = "speech_no")

# speakers ---------------------------------------------------------------------

speakers <- covid_speeches_uk %>%
  unnest_longer(text) %>%
  group_by(speech_no) %>%
  slice_head(1) %>%
  ungroup() %>%
  mutate(speaker = text %>% str_match(".*?:") %>% str_remove(":")) %>%
  select(speech_no, speaker)

# remove paragraphs not spoken by main speaker ---------------------------------

covid_speeches_uk <- covid_speeches_uk %>%
  left_join(speakers, by = "speech_no") %>%
  unnest_longer(text) %>%
  mutate(
    speaker = text %>% str_match(".*?:") %>% str_remove(":"),
    text    = str_remove(text, ".*?\\) ")
    ) %>%
  group_by(speech_no) %>%
  mutate(first_speaker = cumall(speaker == lag(speaker, default = speaker[1]))) %>%
  filter(first_speaker) %>%
  select(-first_speaker) %>%
  nest(text = c(text))

# determine day of week --------------------------------------------------------

covid_speeches_uk <- covid_speeches_uk %>%
  mutate(
    # name of day
    wday = wday(date, label = TRUE),
    # weekend/weekday
    weekend = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday") 
  )

# count paragraphs and words ---------------------------------------------------

covid_speeches_uk <- covid_speeches_uk %>%
  rowwise() %>%
  mutate(
    n_paragraphs = unlist(text) %>% length(),
    n_words      = unlist(text) %>% str_count("\\w+") %>% sum()
  ) %>%
  ungroup()

# how many speeches per day ----------------------------------------------------

covid_speeches_uk %>%
  add_count(date) %>%
  ggplot(aes(x = date, y = n, color = weekend, shape = weekend)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = c(1, 2)) +
  scale_color_viridis_d(begin = 0.4, end = 0.9) +
  facet_grid(speaker ~ .) +
  labs(
    title = "Number of speeches per day",
    x = NULL, y = NULL, color = NULL, shape = NULL
  )

# how long do they speak? ------------------------------------------------------

# number of paragraphs ----

ggplot(covid_speeches_uk, aes(x = n_paragraphs)) +
  geom_boxplot() +
  labs(
    title = "Distribution of number of paragphs",
    subtitle = "of UK daily briefings",
    x = "Number of paragraphs"
  )

ggplot(covid_speeches_uk, aes(x = n_words)) +
  geom_boxplot() +
  labs(
    title = "Distribution of number of words",
    subtitle = "of UK daily briefings",
    x = "Number of paragraphs"
  )

# number of paragraphs vs. date ----

lm_paragraphs <- lm(n_paragraphs ~ date, data = covid_speeches_uk)
lm_paragraphs_rsq <- glance(lm_paragraphs)$r.squared

covid_speeches_uk %>%
  ggplot(aes(x = date, y = n_paragraphs)) +
  geom_point(aes(color = weekend, shape = weekend)) +
  geom_smooth(aes(x = date, y = n_paragraphs), method = lm, formula = y ~ x, color = "darkgray") +
  scale_color_viridis_d(begin = 0.4, end = 0.9) +
  labs(
    title = "Length of UK COVID-19 speeches",
    subtitle = glue("Length measured in number of paragraphs, R-squared = {percent(lm_paragraphs_rsq)}"),
    x = NULL, y = "Number of paragraphs", color = NULL, shape = NULL
  )

# number of words vs. date ----

lm_words <- lm(n_words ~ date, data = covid_speeches_uk)
lm_words_rsq <- glance(lm_words)$r.squared

covid_speeches_uk %>%
  ggplot(aes(x = date, y = n_words)) +
  geom_point(aes(color = weekend, shape = weekend)) +
  geom_smooth(aes(x = date, y = n_words), method = lm, formula = y ~ x, color = "darkgray") +
  scale_color_viridis_d(begin = 0.4, end = 0.9) +
  labs(
    title = "Length of UK COVID-19 speeches",
    subtitle = glue("Length measured in number of words, R-squared = {percent(lm_words_rsq)}"),
    x = NULL, y = "Number of words", color = NULL, shape = NULL
  )

# word analysis ----------------------------------------------------------------

covid_speeches_uk_paragraphs <- covid_speeches_uk %>%
  unnest_longer(
    col = text,
    values_to = "text2",
    indices_to = "paragraph_no"
    ) %>%
  group_by(speech_no) %>%
  mutate(paragraph_no = row_number()) %>%
  ungroup()

covid_speeches_uk_words <- covid_speeches_uk_paragraphs %>%
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

covid_speeches_uk_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 25000) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of words in UK COVID-19 briefings",
    subtitle = "Words occurring more than 25,000 times",
    y = NULL, x = NULL
  )
  
# wordcloud ----

covid_speeches_uk_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer_pal(palette = "Blues")(9)))

# remove the word "positive" since it's not really positive --------------------

covid_speeches_uk_words <- covid_speeches_uk_words %>%
  filter(word != "positive")

# sentiment analysis -----------------------------------------------------------

## bing: positive / negative ----

covid_speeches_uk_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 20) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(
    title = "Sentiment and frequency of words in UK COVID-19 briefings",
    subtitle = "Sentiment evaluated using the Bing lexicon",
    y = NULL, x = NULL
  )

covid_speeches_uk_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment = positive - negative,) %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(color = "gray") +
  geom_point(aes(color = sentiment > 0), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
  guides(color = FALSE) +
  labs(
    title = "Sentiment score of words in UK COVID-19 briefings over time",
    subtitle = "Sentiment score calculated as the difference between the number of words with positive and negative sentiments according to the Bing lexicon",
    x = "Date", y = "Sentiment score", shape = NULL
  ) +
  theme(legend.position = "bottom")

# nrc: trust, fear, negative, sadness, anger, surprise, positive, disgust, joy, anticipation ----

covid_speeches_uk_words %>%
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
    title = "Sentiment and frequency of words in UK COVID-19 briefings",
    subtitle = "Sentiment evaluated using the NRC lexicon",
    y = NULL, x = NULL
  )

covid_speeches_uk_words %>%
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
    title = "Sentiment score of words in UK COVID-19 briefings over time",
    subtitle = "Sentiment score calculated as number of words associated with a given sentiment according to the NRC lexicon",
    x = "Date", y = "Sentiment score", shape = NULL, color = NULL
  ) +
  theme_bw()

# 2-grams ----------------------------------------------------------------------

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
  summarise(bigram = unique(bigram))

covid_speeches_uk_bigrams %>%
  mutate(
    bigram = if_else(bigram == "care home", "care home(s)", bigram),
    bigram = if_else(bigram == "care homes", "care home(s)", bigram)
  ) %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 20) %>%
  ggplot(aes(y = fct_reorder(bigram, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of bigrams in UK COVID-19 briefings",
    subtitle = "Bigrams occurring more than 20 times",
    y = NULL, x = NULL
  )

# social to physical distancing

covid_speeches_uk %>%
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
  filter(str_detect(bigram, "social dist|physical dist")) %>%
  mutate(soc_phys = if_else(str_detect(bigram, "social"), "S", "P")) %>%
  count(date, soc_phys) %>%
  ggplot(aes(x = date, y = n, color = soc_phys)) +
  geom_text(aes(label = soc_phys)) +
  guides(color = FALSE) +
  labs(x = "Date", y = "Frequency",
       title = "Social (S) vs. physical (P) distancing",
       subtitle = "Number of mentions over time")

