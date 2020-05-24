library(directlabels)
library(gganimate)

df <- scrape_lastfm(username)

# save you data to csv if you want
#write.csv(df, "lastfmdata.csv")

# If an artist has changed their name over their career, you can adjust it here 
# (e.g., I merge Marina & the Diamonds and Marina into just Marina)
df <- df %>%
  mutate(Artist = ifelse(Artist == "Marina & the Diamonds", "Marina", Artist))

# You can choose to have static or animated graph or both
top_10_graph <- function(df, static = TRUE, animated = TRUE) {
  # Get Top 10 artists
  top_10_artists <- df %>%
    group_by(Artist) %>%
    summarise(no_of_plays = n()) %>%
    arrange(-no_of_plays) %>%
    slice(1:10) %>%
    pull(Artist)
  
  # Calculate cumulative sums
  cumul_df <- df %>%
    filter(Artist %in% top_10_artists) %>%
    mutate(Month_year = Date %>% format("%Y-%m") %>% str_c("-01") %>% ymd()) %>%
    group_by(Month_year, Artist) %>%
    summarise(no_of_plays = n()) %>%
    ddply(.(Artist), function(x) {
      x %>%
        arrange(Month_year) %>%
        mutate(cumul_plays = cumsum(no_of_plays))
    })
  
  # Generate graph
  plt <- cumul_df %>%
    ggplot(aes(x = Month_year, y = cumul_plays, color = Artist)) +
    geom_line() +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      legend.position = "None"
    ) +
    ylab("Total number of plays") +
    geom_dl(aes(label = Artist), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))  +
    expand_limits(x = max(pull(cumul_df, Month_year)) + 700)
  
  # Make it animated
  anim <- plt + transition_reveal(cumul_df$Month_year)
  
  if (static == TRUE) {
    ggsave(filename = "top_10_graph.jpeg", 
           plot = plt,
           width = 20,
           height = 15,
           units = "cm")
  }
  
  if (animated == TRUE) {
    animate(anim,
            fps = 15,
            width = 20,
            height = 15,
            units = "cm",
            res = 300,
            duration = 30, end_pause = 5
    )
    
    anim_save("top_10_graph.gif")
  }
}