library(tidyverse)
library(rtweet)


date <- Sys.Date()
date <- paste0(wday(date, label = T, abbr = F), " ", month(date, label = T, abbr = F), " ", day(date), " ",year(date))
search_term <- "#snow"

# downloads tweets
token <- get_token()
greens <- search_tweets(q= search_term, n=10000,
                        geocode = "53.33,-6.25,1500mi",
                        token = token)

# sees which lones have emojis
green_emojis <- greens %>%
  mutate(emoji = emo::ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE)

# function to download emoji image
emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

# makes html/image label
link_to_img <- function(x, size = 20) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

# makes df of top ten, with url's and labels
top_green <- green_emojis %>%
  slice(1:10) %>%
  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
         label = link_to_img(url))


offset <- max(top_green$n) / 20


top_green %>%
  ggplot(aes(fct_reorder(emoji, n, .desc = F), n, label = label)) +
  geom_col(fill = "darkolivegreen") +
  geom_richtext(aes(y = n + offset), fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = "Number of Tweets") +
  ggtitle(paste0("Emojis in Tweets Featuring\n\"", search_term, "\" from\n", date)) +
  theme_minimal() +
  theme(title = element_text(family = "Ink Free", face = "bold", size = 28),
        plot.title = element_text(hjust = 0.8, vjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 36), # size of emoji icons
        axis.text.x = element_text(size = 24), # numbers on bottom axis
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_flip()
