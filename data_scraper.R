# Scraper for lastfm data given a username

library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)

# You can specify how many pages you want to go back to with page_limit
# The default is to scrape everything

scrape_lastfm <- function(username, page_limit = "ALL") {
  lastfm <- str_c("https://www.last.fm/user/",
                  username,
                  "/library?page=")
  
  pages <- str_c(lastfm, 1) %>%
    read_html() %>%
    html_nodes(xpath = "//ul[@class = 'pagination-list']") %>%
    html_nodes("a") %>%
    html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  if (page_limit != "ALL") {pages <- page_limit}
  
  lapply(1:pages, function(i) {
    page <- read_html(str_c(lastfm, i)) 
    
    songs <- page %>%
      html_nodes(xpath = "//td[@class = 'chartlist-name']") %>%
      html_nodes("a") %>%
      html_text() 
    
    artists <- page %>%
      html_nodes(xpath = "//td[@class = 'chartlist-artist']") %>%
      html_nodes("a") %>%
      html_text() 
    
    dates <- page %>%
      html_nodes(xpath = "//td[@class = '
                chartlist-timestamp
                chartlist-timestamp--lang-en
            ']") %>%
      html_nodes("span") %>%
      html_attr("title") %>%
      str_extract("\\d.*") %>%
      dmy_hm()
      
    tibble(Song = songs, Artist = artists, Date = dates) 
  }) %>%
    reduce(rbind)
} 
