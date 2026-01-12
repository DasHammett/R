#library(lubridate)
library(jsonlite)
#library(tidyr)
#library(purrr)
library(cowplot)
#library(dplyr)
library(ggplot2)
library(ggbump)
library(RColorBrewer)
library(httr)
library(data.table)
setwd("C:/Users/JordiVidalCases/Desktop/R")
Sys.setlocale("LC_TIME", "C")

# set the Last.fm API endpoint and user parameters
endpoint <- "http://ws.audioscrobbler.com/2.0/"
user <- ""
api_key <- ""

# music <- read.csv("music2.csv", sep = ";", header = TRUE, encoding = "UTF-8")
music <- fread("music2.csv", sep = ";", header = TRUE, encoding = "UTF-8")
#music$date <- ymd_hms(music$date)
musicDT <- data.table(music)
from <- as.integer(max(music$date))
musicDT[, date := as.Date(date)]

# Load genres file saved in disk from previous execution
# genres <- read.csv("genres.csv", sep = ";", header = TRUE, encoding = "UTF-8")
genres <- fread("genres.csv", sep = ";", header = TRUE, encoding = "UTF-8")
genres <- as.data.frame(sapply(genres, gsub, pattern = " {2, }", replacement = ""))
genresDT <- data.table(genres)

## Only run below to update tracks and genres

# set the method and parameters for the API call
method <- "user.getrecenttracks"
params <- list(user = user, api_key = api_key, method = method, format = "json", from = from, limit = 200)
params <- list(user = user, api_key = api_key, method = method, format = "json", limit = 200)
response <- GET(url = endpoint, query = params)
data <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(data, flatten = TRUE)
totalPages <- data$recenttracks$'@attr'$totalPages

# Progress bar eye candy for querying last.fm
map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE, format = "[:bar] :current/:total (:percent)", width = 100)
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_df(.x, f, ..., .id = .id)
}

# Query for last.fm to retrieve scrobbles. Only retrieves new scrobbles from last date loadad in music file
lastfm <- function(x) {
  success <- FALSE
  tries <- 0
  while (!success && tries < 3) {
    tries <- tries + 1
    tryCatch({
      params <- list(user = user, api_key = api_key, method = method, format = "json", from = from, limit = 200, page = x)
      response <- GET(url = endpoint, query = params)
      data <- content(response, as = "text", encoding = "UTF-8")
      data <- fromJSON(data, flatten = TRUE)
      df <- data.table(data$recenttracks$track)[, c(4, 7, 9, 11)][, setnames(.SD, gsub(".#text", "", names(.SD)))]
      success <- TRUE
      return(df)
    }, error = function(e) {
      message("Page ", x, " failed retrying...(try: ", tries, ")")
      Sys.sleep(1)  # wait a bit before retrying
    })
  }
  if (!success) {
	  stop("Page ", x, " failed after 3 attempts")
  }
}

# Get genres from artists from last.fm
artists <- setdiff(result$artist, genresDT$artist)
artists <- stringi::stri_trans_general(artists, "Latin-ASCII")

query <- function(artists) {
 # raw <- fromJSON(paste0("https://ws.audioscrobbler.com/2.0/?method=artist.getTopTags",
 #                        "&api_key=", api_key,
 #                        "&artist=", artists,
 #                        "&autocorrect=0",
 #                        "&format=json")
 # )
	raw <- GET("https://ws.audioscrobbler.com/2.0/",
			   query = list(method = "artist.getTopTags",
							api_key = api_key,
							artist = artists,
							autocorrect = 1,
							format = "json"
			   )
	)
  raw <- content(raw, as = "parsed", encoding = "UTF-8")
  artist <- if (!is.null(raw$error)) {
    c("Not found")
  } else {
    raw$toptags$`@attr`$artist
  }
  genre <- if (!is.null(raw$error) || length(raw$toptags$tag) == 0) {
    c("Not found")
  } else if (raw$toptags$tag[[1]]$name == "seen live") {
    raw$toptags$tag[[2]]$name
  } else {
    raw$toptags$tag[[1]]$name
  }
  df <- data.frame("artist" = artist,
                   "genre" =  genre
  )
}

genresdf <- map_df_progress(artists,  ~ query(.x))
# genres <- bind_rows(genres, genresdf)
genres <- rbind(genres, genresdf)
genres <- unique(genres)
genres$genre <- stringr::str_to_title(genres$genre)
genresDT <- data.table(genres)


# Merge current music df with new scrobbles
df <- map_df_progress(1:totalPages, ~ lastfm(.x))
df$date <- as.Date(df$date, format = "%d %b %Y, %H:%M")
# df$date <- dmy_hm(df$date)

dfDT <- data.table(df)
dfDT <- merge(dfDT, genresDT[,.(genre, original_artist)], by.x = "artist", by.y = "original_artist", all.x = TRUE)

music <- bind_rows(music, df)
music$artist <- stringr::str_to_title(music$artist)
music <- music %>% left_join(genres %>% select(artist, genre), by = "artist")

# musicDT <- bind_rows(musicDT, dfDT)
musicDT <- rbindlist(list(musicDT, dfDT), use.names = TRUE)
### End of update tracks and genres

music$artist <- stringr::str_to_title(music$artist)
music <- music %>% left_join(genres %>% select(artist, genre), by = "artist")
music <- music %>% filter(!duplicated(.))

## Data.table version
musicDT <- merge(musicDT, genresDT[,.(original_artist, genre)], by.x = "artist", by.y = "original_artist", all.x = TRUE)

# This is confusing as it looks like a right join and the column name needs to be renamed in order to use the musicDT column name. Columns of the genresDT come first
musicDT <- genresDT[, .(artist = artist, genre)][musicDT, on = .(artist)]

# Alternative join syntax. We add explicitly the column we want as a new column. Still the left join is reversed in the syntax, but no need to rename column and the joined column(s) go at the end
musicDT[, genre := genresDT[musicDT, on = .(original_artist = artist), genre]]

# Save music and genre files to disk
#write.csv2(music, "music2.csv", row.names = FALSE)
fwrite(music, "music2.csv", sep = ";")
#write.csv2(genres, "genres.csv", row.names = FALSE)

# Plot Top 10 genres by year
num_genres <- 10
num_years <- 10
ranking <- music %>%
  filter(year(date) >= max(year(date)) - num_years) %>%
  group_by(Year = year(date), genre) %>%
  summarise(N = n()) %>%
  mutate(rank = row_number(-N))

# Data table equivalent
rankingDT <- musicDT[year(date) >= max(year(date)) - num_years & year(date) != 2026,
                    .(N = .N),
                    by = .(genre, Year = year(date))][, rank := frank(-N, ties.method = "first"), by = Year]

label <- rankingDT %>%
  top_n(-num_genres) %>%
  group_by(genre)

# labelDT <- rankingDT[order(-N), .SD[1:10], by = genre][!is.na(rank)]
labelDT <- rankingDT[order(-N) & rank <= num_genres]


getPalette <- colorRampPalette(brewer.pal(12, "Set3"))
fons <- "#203c4e"

colors <- c(
  "#FF0000", "#00FF00", "#0000FF", "#FFA500", "#800080",
  "#008080", "#FF6347", "#9932CC", "#228B22", "#FF4500",
  "#800000", "#3CB371", "#00FFFF", "#800080", "#FFFF00",
  "#FF69B4", "#1E90FF", "#FF1493", "#FF8C00", "#DC143C",
  "#00FF7F", "#9400D3", "#B22222", "#ADFF2F", "#FF00FF",
  "#FFFFE0", "#7CFC00", "#BA55D3", "#00FA9A", "#FFD700"
)

Cairo::CairoWin()
ggplot(rankingDT[order(rank), .SD[1:num_genres], by = Year], aes(Year, rank)) +
  #geom_line(aes(color = genre, group = factor(genre)), linewidth = 2) +
  geom_bump(aes(group = factor(genre), color = genre), linewidth = 2.3, smooth = 12) +
  geom_point(aes(fill = genre, colour = genre), shape = 21, size = 9) +
  geom_text(aes(label = N), size = 3) +
  geom_label(data = labelDT[, .SD[which.min(Year)], by = genre], #filter(labelDT, Year == min(Year))
            aes(label = genre, x = Year, y = rank, fill = genre),
            check_overlap = TRUE,
            colour = "black",
            nudge_y = 0.33,
            label.size = 0.45) +
  geom_label(data = labelDT[, .SD[.N > 1 & Year == max(Year)], by = genre],
    # data = labelDT[labelDT[, .(maxYear = max(Year), minYear = min(Year)), by = genre][maxYear != minYear], on = .(genre, Year = maxYear)], #filter(labelDT, Year == max(Year), min(Year) != max(Year)),
            aes(label = genre, x = Year + 0.11, y = rank, fill = genre),
            color = "black",
            label.size = 0.45,
            hjust = 0) +
  scale_x_continuous(expand = c(0, 0.4, 0, 1), breaks = seq(max(year(musicDT$date)) - num_years, max(year(musicDT$date))-1, 1)) +
  scale_fill_manual(values = colors) + #getPalette(27)) +
  scale_color_manual(values = colors) + #getPalette(27)) +
  scale_y_reverse(breaks = seq(1, 10, 1)) +
  labs(title = "Top 10 generes musicals per any i el seu ranking",
       subtitle = "Número representa quantitat de cançons escoltades") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(colour = "grey90"),
        axis.text.x = element_text(colour = "grey90", size = 12),
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
  group_by(Year = year(date), Hour = hour(date)) %>%
  summarise(N = n(),  .groups = "drop") %>%
  complete(Year, Hour, fill = list(N = 1))

raster <- ggplot(music, aes(year(date), y = hour(date))) +
  stat_density_2d_filled(contour_var = "count") +
  labs(y = "Hora del dia",
       x = "Any",
       fill = "Quantitat de cançons") +
  theme(axis.title = element_text(size = 12),
        legend.position = "right",
        panel.grid = element_blank(),
        plot.margin = unit(c(0,  0,  0.1,  0),  "cm")
  )

bar <- ggplot(music,  aes(year(date))) +
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

bench <- function(x) {
  start <- Sys.time()
  x
  end <- Sys.time()
  duration <- end - start
  invisible(x)
  return(duration)
}

### Random stuff
rankingDT <- musicDT[,
                     .(N = .N),
                     by = .(genre, year(date))][, rank := frank(-N), by = year]

setkey(rankingDT, genre, year)


rankingDT[,
          c(as.list(round(summary(N),0)),
            Years = .N,
            Total = sum(N),
            Min_year = min(year)
            ),
          by = genre][,"% Total" := scales::percent(Total/sum(Total), accuracy = 0.01)][order(-Total) ,.SD[1:50]] #[,.(genre,Min.,Mean,Max.,Total, Years, Min_year)]


rankingDT %>%
  group_by(genre) %>%
  summarise(Min = min(N),
            '1st' = quantile(N, 0.25),
            Median = median(N),
            Mean = mean(N),
            '3rd' = quantile(N, 0.75),
            Max = max(N),
            Years = n(),
            Total = sum(N),
            Min_year = min(year)
  ) %>%
  mutate("% Total" = percent(Total/sum(Total), 0.01)) %>%
  arrange(-Total) %>%
  print(n = 50)


musicDT[genre %in% musicDT[, .N, by = genre][order(-N)][1:10,genre], .N, by = .(genre, year(date))] %>%
  ggplot(aes(reorder(genre,-N),N)) +
  geom_boxplot(varwidth = FALSE) +
  geom_violin(linewidth = 0.3, colour = "grey70", alpha= 0) +
  stat_summary(
    fun = "mean",
    size = 0.3,
    colour = "darkred") +
  stat_summary(
    geom = "text",
    fun = function(x) quantile(x, c(0.25, 0.5, 0.75)),
    aes(label = round(after_stat(y),2)),
    position = position_nudge(x = 0.4),
	hjust = 0,
    size = 3) +
  labs(x = "Genre") +
  theme(panel.background = element_blank())

musicDT[musicDT[, .I[genre %in% musicDT[, .N, by = genre][order(-N)][1:10,genre]]]]

rankingDT <- musicDT[year(date) >= max(year(date)) - num_years,
                     .(N = .N),
                     by = .(genre, Year = year(date))][, rank := frank(-N), by = Year]

rankingDT <- data.table(rankingDT[order(-N) & Year >= 2013,.SD[1:min(10, .N)], by = genre] %>% complete(genre, Year, fill= list(rank = 11, N = 0)))



labelDT <- rankingDT[order(-N) & rank <= num_genres]

labelDT <- labelDT[CJ(genre, year, unique = TRUE), on = .(genre, year)][,lapply(.SD, function(x) ifelse(is.na(x),11,x))]
labelDT[order(genre, year), new_rank := fcase(year == min(year), 1,
                                              lag(rank) == 11 & rank <= 10, 1,
                                              default = 0), by = genre]
labelDT[order(genre, year), group := cumsum(new_rank)]

ggplot(labelDT, aes(year, rank)) +
  geom_bump(aes(group = genre, colour = genre), linewidth = 1, alpha = 0.2, smooth = 15) +
  geom_bump(data = labelDT[rank <= 10], aes(x = year, y = rank, group = group, colour = genre), linewidth = 1, smooth = 15) +
  geom_point(data = labelDT[rank <= 10], aes(fill = genre, colour = genre), shape = 21, size = 5) +
  geom_segment(data = labelDT[rank <= 10 & year != max(year), by = genre], aes(x = year, xend = year + 0.3, y = rank, yend = rank, colour = genre), linewidth = 1, lineend = "round") +
  scale_y_continuous(limits = c(1,10))







test_fun <- function(x) {
  success <- FALSE
  tries <- 0
  while (!success && tries < 3) {
    tries <- tries + 1
    tryCatch({
      # params <- list(user = user, api_key = api_key, method = method, format = "json", from = from, limit = 200, page = x)
      params <- list(user = user, api_key = api_key, method = method, format = "json", limit = 200, page = x)
      # make the API call and parse the JSON response
      response <- GET(url = endpoint, query = params)
      data <- content(response, as = "text", encoding = "UTF-8")
      data <- fromJSON(data, flatten = TRUE)
      df <- data.table(data$recenttracks$track)[, c(4, 7, 9, 11)][, setnames(.SD, gsub(".#text", "", names(.SD)))]
      success <- TRUE
      return(df)
    }, error = function(e) {
      message("Page ", x, " failed retrying...(try: ", tries, ")")
      Sys.sleep(1)  # wait a bit before retrying
    })
  }
}

test_df <- purrr::map_df(1:totalPages, ~ test_fun(.x), .progress = TRUE, force = TRUE)



result <- purrr::map_df(1:totalPages, ~test_fun(.x),
    .progress = list(
    name = "Processing",
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | ETA: {cli::pb_eta}",
    clear = FALSE  # Keep bar visible after completion
  )
)

