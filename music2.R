library(lubridate)
library(jsonlite)
library(tidyr)
library(purrr)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggbump)
library(RColorBrewer)
library(httr)
library(jsonlite)

# set the Last.fm API endpoint and user parameters
endpoint <- "http://ws.audioscrobbler.com/2.0/"
user <- "hammett80"
api_key <- "496e264e9263df224f4936c0151ba6c9"

# Load music file saved in disk from previous execution
music <- read.csv("music2.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
music$uts <- ymd_hms(music$uts)
from <- as.integer(max(music$uts))

# Load genres file saved in disk from previous execution
genres <- read.csv("genres.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
genres <- as.data.frame(sapply(genres, gsub, pattern = " {2, }", replacement = ""))

## Only run below to update tracks and genres

# set the method and parameters for the API call
method <- "user.getrecenttracks"
params <- list(user = user, api_key = api_key, method = method, format = "json", from = from, limit = 200)
response <- GET(url = endpoint, query = params)
data <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(data, flatten = TRUE)
totalPages <- data$recenttracks$'@attr'$totalPages

# Progress bar eye candy for querying last.fm
map_df_progress <- function(.x, .f, ..., .id = NULL) {
    .f <- purrr::as_mapper(.f, ...)
    pb <- progress::progress_bar$new(total = length(.x), force = TRUE, format = "[:bar] :current/:total (:percent)")
    f <- function(...) {
     pb$tick()
     .f(...)
    }
    purrr::map_df(.x, f, ..., .id = .id)
}

# Query for last.fm to retrieve scrobbles. Only retrieves new scrobbles from last date loadad in music file
lastfm <- function(x) {
    Sys.sleep(0.25)
    params <- list(user = user, api_key = api_key, method = method, format = "json", from = from, limit = 200, page = x)

    # make the API call and parse the JSON response
    response <- GET(url = endpoint, query = params)
    data <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(data, flatten = TRUE)

    # extract the listening information from the response
    artist <- data$recenttracks$track$'artist.#text'
    album <- data$recenttracks$track$'album.#text'
    track <- data$recenttracks$track$name
    date <- data$recenttracks$track$'date.#text'

    # create a dataframe from the listening information
    df <- data.frame(artist = artist, album = album, track = track, uts = date)
}

# Get genres from artists from last.fm
artists <- unique(stringi::stri_replace_all_fixed(music$artist,
    pattern = c(" ", "&"),
    replacement = c("%20", "%26"),
    vectorize_all = FALSE)
    )

query <- function(artists) {
    raw <- fromJSON(paste0("https://ws.audioscrobbler.com/2.0/?method=artist.getTopTags",
                           "&api_key=", api_key,
                           "&artist=", artists,
                           "&autocorrect=0",
                           "&format=json")
                )
    artist <- if (!is.null(raw$error)) {
      c("Not found")
    } else {
      raw$toptags$`@attr`$artist
    }
    genre <- if (!is.null(raw$error) || is.null(dim(raw$toptags$tag))) {
      c("Not found")
    } else if (raw$toptags$tag$name[1] == "seen live") {
      raw$toptags$tag$name[2]
    } else {
        raw$toptags$tag$name[1]
    }
    df <- data.frame("artist" = artist,
                     "genre" =  genre,
                     "original_artist" = stringi::stri_replace_all_fixed(artists,
                                                                       pattern = c("%20", "%26"),
                                                                       replacement = c(" ", "&"),
                                                                       vectorize_all = FALSE)
        )
}

genres <- map_df_progress(artists,  ~ query(.x))
genres$genre <- stringr::str_to_title(genres$genre)

# Merge current music df with new scrobbles
df <- map_df_progress(1:totalPages, ~ lastfm(.x))
df$uts <- dmy_hm(df$uts)

music <- bind_rows(music, df)
music <- music %>% left_join(genres %>% select(artist, genre), by = "artist")

### End of update tracks and genres
music <- music %>% left_join(genres %>% select(artist, genre), by = "artist")
music <- music %>% filter(!duplicated(.))

# Save music and genre files to disk
#write.csv2(music, "music2.csv", row.names = FALSE)
#write.csv2(genres, "genres.csv", row.names = FALSE)

# Plot Top 10 genres by year
num_genres <- 10
num_years <- 10
ranking <- music %>%
    filter(year(uts) >= max(year(uts)) - num_years) %>%
    group_by(Year = year(uts), genre) %>%
    summarise(N = n()) %>%
    mutate(rank = row_number(-N))

label <- ranking %>%
    top_n(-num_genres) %>%
    group_by(genre)

getPalette <- colorRampPalette(brewer.pal(12, "Set3"))
fons <- "#203c4e"

ggplot(top_n(ranking, -num_genres), aes(Year, rank)) +
    #geom_line(aes(color = genre, group = factor(genre)), size = 2) +
    geom_bump(aes(group = factor(genre), color = genre), linewidth = 2) +
    geom_point(aes(fill = genre, colour = genre), shape = 21, size = 7) +
    geom_text(aes(label = N), size = 3) +
    geom_text(data = filter(label, Year == min(Year)),
              aes(label = genre, x = Year, y = rank, color = genre),
              check_overlap = TRUE,
              vjust = -1.6) +
    geom_text(data = filter(label, Year == max(Year), min(Year) != max(Year)),
              aes(label = genre, x = Year + 0.11, y = rank, color = genre),
              hjust = 0) +
    scale_x_continuous(expand = c(0, 0.4, 0, 1), breaks = seq(max(year(music$uts)) - num_years, max(year(music$uts)), 1)) +
    scale_fill_manual(values = getPalette(27)) +
    scale_color_manual(values = getPalette(27)) +
    scale_y_reverse(breaks = seq(1, 10, 1)) +
    labs(title = "Top 10 generes musicals per any i el seu ranking",
         subtitle = "Número representa quantitat de cançons escoltades") +
    theme(legend.position = "none",
          axis.text.y = element_text(colour = "grey90"),
          axis.text.x = element_text(colour = "grey90"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = fons),
          plot.background = element_rect(fill = fons),
          plot.title = element_text(colour = "white"),
          plot.subtitle = element_text(colour = "white", face = "italic")
    )

# Plot of habits of listening music by hour
df_hours <- music %>%
    group_by(Year = year(uts), Hour = hour(uts)) %>%
    summarise(N = n(),  .groups = "drop") %>%
    complete(Year, Hour, fill = list(N = 1))

raster <- ggplot(music, aes(year(uts), y = hour(uts))) +
 stat_density_2d_filled(contour_var = "count") +
 labs(y = "Hora del dia",
        x = "Any",
        fill = "Quantitat de cançons") +
 theme(axis.title = element_text(size = 12),
         legend.position = "right",
         panel.grid = element_blank(),
         plot.margin = unit(c(0,  0,  0.1,  0),  "cm")
         )

bar <- ggplot(music,  aes(year(uts))) +
    geom_histogram(stat = "count",  binwidth = 1) +
    stat_bin(binwidth = 1,  geom = "text",  aes(label = format(..count..,  big.mark = ".")),  colour = "black",  drop = TRUE,  angle = 90,  hjust = -0.1)+
    labs(y = "# cançons") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(1.2,  0.2,  0,  0.2),  "cm"),
          panel.border = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_blank()
    ) +
    coord_cartesian(clip = "off")

legend <- get_legend(raster)

plot_grid(
      bar,
      raster + theme(legend.position = "none"),
      align = "v",
      hjust = -1,
      nrow = 2,
      rel_heights = c(0.3, 1),
      labels = "Hàbits de consum de música",
      label_fontface = "plain"
)
