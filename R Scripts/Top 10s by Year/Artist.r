# Clear environment
source("R Scripts/Clear Environment.r")

# Get list of years
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

# Select col width
numColWidthArtist <- 215
numColWidthHours <- 140
numColWidthMinutes <- 140
numColWidthDistinct <- 140

# Tables for favourite artist - All time

gtFavArtistAllTime <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
            TotalTimeMinutes = sum(ms_played)/1000/60,
            DistinctSongs = n_distinct(ifelse(ms_played > 10000, spotify_track_uri, NA))) %>%
  ungroup() %>%
  arrange(desc(TotalTimeHours)) %>%
  head(10) %>%
  mutate(TotalTimeHours = format(round(TotalTimeHours, 0), big.mark = ","),
         TotalTimeMinutes = format(round(TotalTimeMinutes, 0), big.mark = ",")) %>%
  rename("Artist" = master_metadata_album_artist_name,
         "Hours Played" = TotalTimeHours,
         "Minutes Played" = TotalTimeMinutes,
         "Distinct Songs" = DistinctSongs) %>%
  gt() %>%
  cols_width(`Artist` ~ px(numColWidthArtist),
             `Hours Played` ~ px(numColWidthHours),
             `Minutes Played` ~ px(numColWidthMinutes),
             `Distinct Songs` ~px(numColWidthDistinct)) %>%
  tab_options(table.font.size = 14,
              column_labels.font.size = 16)

# Tables for favourite artist - Each year

# List to store gt tables for each year, and count years
lstFavArtist <- list()
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

for (i in seq_along(vecYears)) {
  
  tblName <- paste0("gtFavArtist", vecYears[i])
  
  lstFavArtist[[tblName]] <- tblListeningData %>%
    filter(Type == "Song",
           substr(Date, 1, 4) == vecYears[i]) %>%
    group_by(master_metadata_album_artist_name) %>%
    summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
              TotalTimeMinutes = sum(ms_played)/1000/60,
              DistinctSongs = n_distinct(ifelse(ms_played > 10000, spotify_track_uri, NA))) %>%
    ungroup() %>%
    arrange(desc(TotalTimeHours)) %>%
    head(10) %>%
    mutate(TotalTimeHours = format(round(TotalTimeHours, 0), big.mark = ","),
           TotalTimeMinutes = format(round(TotalTimeMinutes, 0), big.mark = ",")) %>%
    rename("Artist" = master_metadata_album_artist_name,
           "Hours Played" = TotalTimeHours,
           "Minutes Played" = TotalTimeMinutes,
           "Distinct Songs" = DistinctSongs) %>%
    gt() %>%
    cols_width(`Artist` ~ px(numColWidthArtist),
               `Hours Played` ~ px(numColWidthHours),
               `Minutes Played` ~ px(numColWidthMinutes),
               `Distinct Songs` ~px(numColWidthDistinct)) %>%
    tab_options(table.font.size = 14,
                column_labels.font.size = 16)
  
}

