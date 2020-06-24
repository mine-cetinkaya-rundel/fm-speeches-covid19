# check we can do this ---------------------------------------------------------

robotstxt::paths_allowed("https://www.rev.com/blog/transcript-tag/united-kingdom-coronavirus-briefing-transcripts")

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(here)

# uk covid speech urls  --------------------------------------------------------

all_speeches_page_uk_p1 <- read_html("https://www.rev.com/blog/transcript-tag/united-kingdom-coronavirus-briefing-transcripts")
all_speeches_page_uk_p2 <- read_html("https://www.rev.com/blog/transcript-tag/united-kingdom-coronavirus-briefing-transcripts/page/2")
all_speeches_page_uk_p3 <- read_html("https://www.rev.com/blog/transcript-tag/united-kingdom-coronavirus-briefing-transcripts/page/3")
all_speeches_page_uk_p4 <- read_html("https://www.rev.com/blog/transcript-tag/united-kingdom-coronavirus-briefing-transcripts/page/4")

all_speeches_page_uk <- list(
  all_speeches_page_uk_p1,
  all_speeches_page_uk_p2,
  all_speeches_page_uk_p3,
  all_speeches_page_uk_p4
)

get_speech_urls_uk <- function(page){
  
  page %>%
    html_nodes(xpath = '//meta[@itemprop="mainEntityOfPage"]') %>% 
    html_attr("itemid")
  
}

covid_speech_urls_uk <- map(all_speeches_page_uk, get_speech_urls_uk) %>% unlist()

# function to scrape each speech -----------------------------------------------

scrape_speech_uk <- function(url){
  
  speech_page <- read_html(url)
  
  title <- speech_page %>%
    html_node("#fl-main-content .fl-heading-text") %>%
    html_text()
  
  date <- speech_page %>%
    html_node(".fl-node-5deef52516c90 p") %>%
    html_text() %>%
    mdy()
  
  abstract <- speech_page %>%
    html_node(".fl-node-5e186cbbd29ea .fl-rich-text > p:nth-child(1)") %>%
    html_text()
  
  text <- speech_page %>% 
    html_nodes("#transcription p") %>%
    html_text() %>%
    list()
  
  tibble(
    title    = title,
    date     = date,
    abstract = abstract,
    text     = text,
    url      = url
  )
  
}

# scrape all covid speeches ----------------------------------------------------

covid_speeches_uk <- map_dfr(covid_speech_urls_uk, scrape_speech_uk)

# write scraped data -----------------------------------------------------------

write_rds(covid_speeches_uk, path = here::here("data", "covid-speeches-uk.rds"))
