# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(topicmodels)
library(here)

# set theme for plots to minimal -----------------------------------------------

theme_set(theme_minimal(base_size = 16))

# set color --------------------------------------------------------------------

scotblue <- "#0065BF"
ukred <- "#D00C27"

# read data --------------------------------------------------------------------

covid_speeches_scot_words <- read_rds(here::here("processed-data", "covid_speeches_scot_words.rds"))
covid_speeches_scot_words <- covid_speeches_scot_words %>%
  mutate(origin = "Scotland")

covid_speeches_uk_words <- read_rds(here::here("processed-data", "covid_speeches_uk_words.rds"))
covid_speeches_uk_words <- covid_speeches_uk_words %>%
  mutate(origin = "UK")

uk_speakers <- covid_speeches_uk_words %>% 
  count(speaker, sort = TRUE) %>%
  select(speaker) %>%
  unnest_tokens(word, speaker)

covid_speeches_uk_words <- covid_speeches_uk_words %>%
  anti_join(uk_speakers, by = "word")

# bind data& calculate tf-idf --------------------------------------------------

covid_speeches_words <- bind_rows(covid_speeches_scot_words, covid_speeches_uk_words) %>%
  count(origin, word, sort = TRUE) %>%
  group_by(origin) %>%
  mutate(total = sum(n)) %>%
  bind_tf_idf(word, origin, n)

# terms with high tf-idf -------------------------------------------------------

covid_speeches_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# visualise tf-idf -------------------------------------------------------------

covid_speeches_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(origin) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(y = word, x = tf_idf, fill = origin)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = "tf-idf", title = "Common words in COVID briefings") +
  facet_wrap(~origin, ncol = 2, scales = "free") +
  scale_fill_manual(values = c(scotblue, ukred))

# save data --------------------------------------------------------------------

write_rds(covid_speeches_words, "processed-data/covid_speeches_words.rds")
