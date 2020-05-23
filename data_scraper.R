library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)

scrape_lastfm <- function(username, scrape = "ALL") {
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
  
  if (scrape != "ALL") {pages <- scrape}
  
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
      str_extract(".*,") %>%
      str_sub(end = -2)
    
    
    tibble(Song = songs, Artist = artists, Date = dates) %>%
      mutate(Date = dmy(Date))
  }) %>%
    reduce(rbind)
} 
