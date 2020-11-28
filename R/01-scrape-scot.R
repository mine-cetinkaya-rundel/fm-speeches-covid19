# check we can do this ---------------------------------------------------------

robotstxt::paths_allowed("https://www.gov.scot/publications")

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(here)

# covid speech urls  -----------------------------------------------------------

all_speeches_page_scot <- read_html("https://www.gov.scot/collections/first-ministers-speeches/")

covid_speech_urls_uk_scot <- all_speeches_page_scot %>%
  html_nodes(".collections-list a") %>%
  html_attr("href") %>%
  str_subset("covid-19") %>%
  str_c("https://www.gov.scot", .)

# function to scrape each speech -----------------------------------------------

scrape_speech_scot <- function(url){
  
  speech_page <- read_html(url)
  
  title <- speech_page %>%
    html_node(".article-header__title") %>%
    html_text()
  
  date <- speech_page %>%
    html_node(".content-data__list:nth-child(1) strong") %>%
    html_text() %>%
    dmy()
  
  location <- speech_page %>%
    html_node(".content-data__list+ .content-data__list strong") %>%
    html_text()
  
  abstract <- speech_page %>%
    html_node(".leader--first-para p") %>%
    html_text()
  
  text <- speech_page %>% 
    html_nodes("#preamble p") %>%
    html_text() %>%
    glue_collapse(sep = " ") %>%
    as.character()
  
  tibble(
    title    = title,
    date     = date,
    location = location,
    abstract = abstract,
    text     = text,
    url      = url
  )
  
}

# scrape all covid speeches ----------------------------------------------------

covid_speeches_scot <- map_dfr(covid_speech_urls_uk_scot, scrape_speech_scot)

# write scraped data -----------------------------------------------------------

write_rds(covid_speeches_scot, file = here::here("data", "covid-speeches-scot.rds"))
