library(tidyverse)
library(showtext)
library(tidytext)
library(geniusr)

### Scales
scale_color_cerati <- function() {
  ggplot2::scale_colour_manual(
    name = "Álbumes de Cerati",
    values = c("Fuerza Natural" = "#1F6E43",
               "Bocanada" = "#104E8B",
               "Amor Amarillo" = "#EEC900",
               "Ahí Vamos" = "black",
               "Siempre Es Hoy" = "#E63946")
  )
}




scale_fill_cerati <- function() {
  ggplot2::scale_fill_manual(
    name = "Álbumes de Cerati",
    values = c("Fuerza Natural" = "#1F6E43",
               "Bocanada" = "#104E8B",
               "Amor Amarillo" = "#EEC900",
               "Ahí Vamos" = "black",
               "Siempre Es Hoy" = "#E63946")
  )
}


## Token GENIUS

genius_token()

font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Permanent Marker", family = "Permanent Marker")


font_t <- "Permanent Marker"
font <- "Roboto"

showtext_auto()
showtext_opts(dpi = 320)

search_song(search_term = "Amor Amarillo")
get_artist_df(artist_id = 263298)

songs <- get_artist_songs_df(artist_id = 263298)

ids <- c(as.character(songs$song_id))

allLyrics <- data.frame()

while (length(ids) > 0) {
  for (id in ids) {
    tryCatch({
      allLyrics <- rbind(get_lyrics_id(id), allLyrics)
      successful <- unique(allLyrics$song_id)
      ids <- ids[!ids %in% successful]
      print(paste0("done - ", id))
      print(paste0("New length is ", length(ids)))
    }, error = function(e){})
  }
}

allIds <- data.frame(song_id = unique(allLyrics$song_id))
allIds$album <- ""

# add album to each song
for (song in allIds$song_id) {
  allIds[match(song,allIds$song_id),2] <- get_song_df(song)[12]
  print(allIds[match(song,allIds$song_id),])
}

allLyrics <- full_join(allIds, allLyrics) %>% 
  filter(album %in% c("Fuerza Natural", "Bocanada", 
                      "Amor Amarillo", "Ahí Vamos",
                      "Siempre Es Hoy"))

# text analysis -----------------------------------------------------------
# tokenize ----------------------------------------------------------------
allLyricsTokenised <- allLyrics %>%
  unnest_tokens(word, line)

# remove stop words -------------------------------------------------------
tidyLyrics <- allLyricsTokenised %>%
  anti_join(tidytext::get_stopwords("es"))

# get sentiment -----------------------------------------------------------
posit <- read_table("positive_words_es.txt", col_names = FALSE) %>% 
  mutate(sentiment = "positive") %>% select("word" = 1, 2)
neg <- read_table("negative_words_es.txt", col_names = FALSE) %>% 
  mutate(sentiment = "negative") %>% select("word" = 1, 2)
espan <- bind_rows(posit, neg)

sentiments_df <- tidyLyrics %>%
  inner_join(espan)%>% 
  count(album, song_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# create data frame for plot ----------------------------------------------
df <- sentiments_df %>% 
  mutate(test = ifelse(sentiment < 0, "negative", "positive")) %>% 
  arrange(test, sentiment) %>% 
  group_by(test) %>%
  mutate(row_num = row_number()) %>% 
  mutate(row_num = ifelse(test == "negative", rev(row_num), row_num))

# update order ------------------------------------------------------------
df$test <- factor(df$test, levels=c('positive', 'negative'))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_text(aes(x = test, y = ifelse(test == "positive", row_num, (-1 * row_num)), label = song_name, color = album), size = 3.5, family = font_t) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 1, y = -15, label = "GUSTAVO CERATI", family = font_t, fontface = "bold", size = 8.7, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -20, label = "Análisis de sentimiento para las canciones solistas,\ndesde el más positivo (arriba a la izquierda)\nal más negativo (abajo a la derecha)", family = font, size = 3, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -1.25, label = "Positivo", family = font, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 2, y = 1.25, label = "Negativo", family = font, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  coord_cartesian(clip = "off") +
  scale_color_cerati() +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.25, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "\nJuan Ignacio Fulponi | Data: genius.com | Basado en: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave("plot1.png", dpi = 320, width = 6, height = 9)


albumes <- df %>% group_by(album) %>% summarise(sentiment = mean(sentiment),
                                     test = if_else(sentiment < 0, "negative", "positive")) %>% 
  arrange(test, sentiment) %>% 
  group_by(test) %>%
  mutate(row_num = row_number())  %>% 
  mutate(row_num = ifelse(test == "negative", rev(row_num), row_num),
         date = case_when(
           album == "Bocanada" ~ 1998,
           album == "Amor Amarillo" ~ 1994,
           album == "Siempre Es Hoy" ~ 2003,
           album == "Ahí Vamos" ~ 2006,
           album == "Fuerza Natural" ~ 2009
         ),
         label = gsub(" ", "\n\n", album),
         num = if_else(sentiment > 1 , sentiment*1.2, sentiment),
         num = if_else(sentiment < -1, num*1.1, num),
         num2 = if_else(sentiment < 0, num*1.3, num*1.2),
         num2 = if_else(abs(num2) > 1.2, num2*1, num2*1.3)) %>% 
  ggplot() +
  geom_col(aes(date, sentiment, fill = album)) +
  geom_text(aes(date, num*.77, label = label), family = font_t, size = 2.2, color = "white") +
  geom_text(aes(date, if_else(num > 0,.16,-.16), label = date), family = font_t, color = "white") +
  geom_text(aes(date,num2*.77, label = round(sentiment, 2), color = album), family = font_t) +
  coord_cartesian(clip = "off") +
  theme_void()  +
  theme(plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  scale_color_cerati() +
  scale_fill_cerati()

albumes

ggsave(paste0("albumes.png"))

## GRID PLOT


 