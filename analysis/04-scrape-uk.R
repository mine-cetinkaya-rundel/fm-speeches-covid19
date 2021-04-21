# check we can do this ---------------------------------------------------------

robotstxt::paths_allowed("https://www.gov.uk/")

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(here)
library(glue)

# uk covid speech urls  --------------------------------------------------------

all_speeches_page_uk_p1 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=1&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p2 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=2&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p3 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=3&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p4 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=4&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p5 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=5&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p6 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=6&parent=prime-ministers-office-10-downing-street")
all_speeches_page_uk_p7 <- read_html("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&organisations%5B%5D=prime-ministers-office-10-downing-street&page=7&parent=prime-ministers-office-10-downing-street")

all_speeches_page_uk <- list(
  all_speeches_page_uk_p1,
  all_speeches_page_uk_p2,
  all_speeches_page_uk_p3,
  all_speeches_page_uk_p4,
  all_speeches_page_uk_p5
)

get_speech_urls_uk <- function(page){
  
  titles <- page %>%
    html_nodes(".gem-c-document-list__item-link") %>% 
    html_text()
  
  urls <- page %>%
    html_nodes(".gem-c-document-list__item-link") %>% 
    html_attr("href") %>%
    paste0("https://www.gov.uk/", .)
  
  tibble(
    title = titles,
    url   = urls
  )
  
}

speech_urls_uk <- map_dfr(all_speeches_page_uk, get_speech_urls_uk)

# only PM covid speeches -------------------------------------------------------

covid_speech_urls_uk <- speech_urls_uk %>%
  filter(str_detect(title, "coronavirus")) %>%
  filter(str_detect(title, "statement")) %>%
  filter(str_detect(title, "Prime Minister|PM"))

# function to scrape each speech -----------------------------------------------

scrape_speech_uk <- function(url){
  
  speech_page <- read_html(url)
  
  title <- speech_page %>%
    html_node(".gem-c-title__text--long") %>%
    html_text() %>%
    str_remove_all("\\n") %>%
    str_trim()
  
  date <- speech_page %>%
    html_node(".gem-c-metadata__definition~ .gem-c-metadata__definition") %>%
    html_text() %>%
    str_remove("Published ") %>%
    str_remove_all("\\n") %>%
    str_trim() %>%
    dmy()
  
  abstract <- speech_page %>%
    html_node(".gem-c-lead-paragraph") %>%
    html_text()
  
  text <- speech_page %>% 
    html_nodes(".govspeak li , .govspeak p") %>%
    html_text() %>%
    glue_collapse(sep = " ") %>%
    as.character()
  
  tibble(
    title    = title,
    date     = date,
    abstract = abstract,
    text     = text,
    url      = url
  )
  
}

# scrape all covid speeches ----------------------------------------------------

covid_speeches_uk <- map_dfr(covid_speech_urls_uk$url, scrape_speech_uk)

# write scraped data -----------------------------------------------------------

write_rds(covid_speeches_uk, file = here::here("data", "covid-speeches-uk.rds"))
