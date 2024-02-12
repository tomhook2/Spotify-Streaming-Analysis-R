# Clear environment
source("R Scripts/Clear Environment.r")

# Get list of years
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

# Select col width
numColWidthShow <- 360
numColWidthHours <- 140
numColWidthMinutes <- 140
numColWidthDistinct <- 120

# Tables for favourite podcasts - All time

gtFavPodcastAllTime <- tblListeningData %>%
  filter(Type == "Podcast",
         ms_played >= 10000) %>%
  group_by(episode_show_name) %>%
  summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
            TotalTimeMinutes = sum(ms_played)/1000/60,
            Count = n_distinct(spotify_episode_uri)) %>%
  ungroup() %>%
  filter(TotalTimeMinutes >= 5) %>%
  arrange(desc(TotalTimeHours)) %>%
  head(10) %>%
  mutate(TotalTimeHours = format(round(TotalTimeHours,0), big.mark = ","),
         TotalTimeMinutes = format(round(TotalTimeMinutes,0), big.mark = ",")) %>%
  rename("Show" = episode_show_name,
         "Hours Played" = TotalTimeHours,
         "Minutes Played" = TotalTimeMinutes,
         "Shows Played" = Count) %>%
  gt() %>%
  cols_width(`Show`~ px(numColWidthShow),
             `Hours Played` ~ px(numColWidthHours),
             `Minutes Played` ~ px(numColWidthMinutes),
             `Shows Played` ~ px(numColWidthDistinct)) %>%
  tab_options(table.font.size = 14,
              column_labels.font.size = 16)

# Tables for favourite podcasts - Each year

# List to store gt tables for each year, and count years
lstFavPodcast <- list()
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

for (i in seq_along(vecYears)) {
  
  tblName <- paste0("gtFavPodcast", vecYears[i])
  
  lstFavPodcast[[tblName]] <- tblListeningData %>%
    filter(substr(Date, 1, 4) == vecYears[i],
           Type == "Podcast",
           ms_played >= 10000) %>%
    group_by(episode_show_name) %>%
    summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
              TotalTimeMinutes = sum(ms_played)/1000/60,
              Count = n_distinct(spotify_episode_uri)) %>%
    ungroup() %>%
    filter(TotalTimeMinutes >= 5) %>%
    arrange(desc(TotalTimeHours)) %>%
    head(10) %>%
    mutate(TotalTimeHours = format(round(TotalTimeHours,0), big.mark = ","),
           TotalTimeMinutes = format(round(TotalTimeMinutes,0), big.mark = ",")) %>%
    rename("Show" = episode_show_name,
           "Hours Played" = TotalTimeHours,
           "Minutes Played" = TotalTimeMinutes,
           "Shows Played" = Count) %>%
    gt() %>%
    cols_width(`Show`~ px(numColWidthShow),
               `Hours Played` ~ px(numColWidthHours),
               `Minutes Played` ~ px(numColWidthMinutes),
               `Shows Played` ~ px(numColWidthDistinct)) %>%
    tab_options(table.font.size = 14,
                column_labels.font.size = 16)
  
}


