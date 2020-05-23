library(directlabels)
library(gganimate)

df <- scrape_lastfm(username)

# Get Top 10 artists
top_10_artists <- df %>%
  group_by(Artist) %>%
  summarise(no_of_plays = n()) %>%
  arrange(-no_of_plays) %>%
  slice(1:10) %>%
  pull(Artist)


cumul_df <- df %>%
  filter(Artist %in% top_10_artists) %>%
  mutate(Month_year = Date %>% format("%Y-%m") %>% str_c("-01") %>% ymd()) %>%
  group_by(Month_year, Artist) %>%
  summarise(no_of_plays = n()) %>%
  ddply(.(Artist), function(x) {
    x %>%
      arrange(Month_year) %>%
      mutate(cumul_plays = cumsum(no_of_plays))
  }) %>%
  filter(year(Month_year) > 2018)

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
  expand_limits(x = max(pull(cumul_df, Month_year)) + 5)

anim <- plt + transition_reveal(cumul_df$Month_year)

animate(anim,
  fps = 15,
  width = 20,
  height = 15,
  units = "cm",
  res = 300,
  duration = 15, end_pause = 40
)

anim_save("animation.gif")
