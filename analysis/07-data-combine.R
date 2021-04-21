# load packages ----------------------------------------------------------------

library(tidyverse)
library(here)

# read data --------------------------------------------------------------------

covid_speeches_scot_words <- read_rds(here::here("processed-data", "covid-speeches-scot-words.rds"))

covid_speeches_uk_words <- read_rds(here::here("processed-data", "covid-speeches-uk-words.rds"))

# assign origin ----------------------------------------------------------------

covid_speeches_scot_words <- covid_speeches_scot_words %>%
  mutate(origin = "Scotland")

covid_speeches_uk_words <- covid_speeches_uk_words %>%
  mutate(origin = "UK")

# bind data --------------------------------------------------------------------

covid_speeches_words <- covid_speeches_scot_words %>%
  select(-location, -speaker) %>% # uk data doesn't have these
  bind_rows(covid_speeches_uk_words)

# save data --------------------------------------------------------------------

write_rds(covid_speeches_words, file = "processed-data/covid-speeches-words.rds", compress = "xz")
