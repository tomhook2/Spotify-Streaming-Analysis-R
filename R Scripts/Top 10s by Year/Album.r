# Clear environment
source("R Scripts/Clear Environment.r")

# Get list of years
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

# Select col width
numColWidthAlbum <- 250
numColWidthArtist <- 200
numColWidthHours <- 130
numColWidthMinutes <- 130

# Tables for favourite albums - All time

gtFavAlbumAllTime <- tblListeningData %>%
  filter(Type == "Song",
         !is.na(master_metadata_track_name)) %>%
  group_by(master_metadata_album_album_name, master_metadata_album_artist_name) %>%
  summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
            TotalTimeMinutes = sum(ms_played)/1000/60) %>%
  ungroup() %>%
  arrange(desc(TotalTimeHours)) %>%
  head(10) %>%
  mutate(TotalTimeHours = format(round(TotalTimeHours, 0), big.mark = ","),
         TotalTimeMinutes = format(round(TotalTimeMinutes, 0), big.mark = ",")) %>%
  rename("Album" = master_metadata_album_album_name,
         "Artist" = master_metadata_album_artist_name,
         "Hours Played" = TotalTimeHours,
         "Minutes Played" = TotalTimeMinutes) %>%
  gt() %>%
  cols_width(`Album`~ px(numColWidthAlbum),
             `Artist` ~ px(numColWidthArtist),
             `Hours Played` ~ px(numColWidthHours),
             `Minutes Played` ~ px(numColWidthMinutes)) %>%
  tab_options(table.font.size = 14,
              column_labels.font.size = 16)

# Tables for favourite albums - Each year

# List to store gt tables for each year, and count years
lstFavAlbum <- list()
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

for (i in seq_along(vecYears)) {
  
  tblName <- paste0("gtFavAlbum", vecYears[i])
  
  lstFavAlbum[[tblName]] <- tblListeningData %>%
    filter(Type == "Song",
           !is.na(master_metadata_track_name),
           substr(Date, 1, 4) == vecYears[i]) %>%
    group_by(master_metadata_album_album_name, master_metadata_album_artist_name) %>%
    summarise(TotalTimeHours = sum(ms_played)/1000/60/60,
              TotalTimeMinutes = sum(ms_played)/1000/60) %>%
    ungroup() %>%
    arrange(desc(TotalTimeHours)) %>%
    head(10) %>%
    mutate(TotalTimeHours = format(round(TotalTimeHours, 0), big.mark = ","),
           TotalTimeMinutes = format(round(TotalTimeMinutes, 0), big.mark = ",")) %>%
    rename("Album" = master_metadata_album_album_name,
           "Artist" = master_metadata_album_artist_name,
           "Hours Played" = TotalTimeHours,
           "Minutes Played" = TotalTimeMinutes) %>%
    gt() %>%
    cols_width(`Album`~ px(numColWidthAlbum),
               `Artist` ~ px(numColWidthArtist),
               `Hours Played` ~ px(numColWidthHours),
               `Minutes Played` ~ px(numColWidthMinutes)) %>%
    tab_options(table.font.size = 14,
                column_labels.font.size = 16)
  
}

