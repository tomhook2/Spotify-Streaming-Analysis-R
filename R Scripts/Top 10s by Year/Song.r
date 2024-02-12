# Clear environment
source("R Scripts/Clear Environment.r")

# Get list of years
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

# Select col width
numColWidthSong <- 250
numColWidthAlbum <- 250
numColWidthArtist <- 200
numColWidthCount <- 110

# Tables for favourite songs - All time

gtFavSongAllTime <- tblListeningData %>%
  filter(Type == "Song",
         !is.na(master_metadata_track_name),
         ms_played >= 10000) %>%
  group_by(master_metadata_track_name,
           master_metadata_album_album_name,
           master_metadata_album_artist_name,
           spotify_track_uri) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  mutate(Count = format(Count, big.mark = ",")) %>%
  mutate(
    master_metadata_track_name = map2(
      master_metadata_track_name, 
      spotify_track_uri, 
      ~ htmltools::a(href = .y, .x)
    ),
    master_metadata_track_name = map(
      master_metadata_track_name, 
      ~ gt::html(as.character(.x))
    )
  ) %>%
  select(-spotify_track_uri) %>%
  rename("Song" = master_metadata_track_name,
         "Album" = master_metadata_album_album_name,
         "Artist" = master_metadata_album_artist_name,
         "Times Played" = Count) %>%
  gt() %>%
  cols_width(`Song`~ px(numColWidthSong),
             `Album` ~ px(numColWidthAlbum),
             `Artist` ~ px(numColWidthArtist),
             `Times Played` ~ px(numColWidthCount)) %>%
  cols_align("left", columns = Song) %>%
  tab_options(table.font.size = 14,
              column_labels.font.size = 16)

# Tables for favourite songs - Each year

# List to store gt tables for each year, and count years
lstFavSong <- list()
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

for (i in seq_along(vecYears)) {
  
  tblName <- paste0("gtFavSong", vecYears[i])
  
  lstFavSong[[tblName]] <- tblListeningData %>%
    filter(substr(Date, 1, 4) == vecYears[i],
           Type == "Song",
           !is.na(master_metadata_track_name),
           ms_played >= 10000) %>%
    group_by(master_metadata_track_name,
             master_metadata_album_album_name,
             master_metadata_album_artist_name,
             spotify_track_uri) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    arrange(desc(Count)) %>%
    head(10) %>%
    mutate(Count = format(Count, big.mark = ",")) %>%
    mutate(
      master_metadata_track_name = map2(
        master_metadata_track_name, 
        spotify_track_uri, 
        ~ htmltools::a(href = .y, .x)
      ),
      master_metadata_track_name = map(
        master_metadata_track_name, 
        ~ gt::html(as.character(.x))
      )
    ) %>%
    select(-spotify_track_uri) %>%
    rename("Song" = master_metadata_track_name,
           "Album" = master_metadata_album_album_name,
           "Artist" = master_metadata_album_artist_name,
           "Times Played" = Count) %>%
    gt() %>%
    cols_width(`Song`~ px(numColWidthSong),
               `Album` ~ px(numColWidthAlbum),
               `Artist` ~ px(numColWidthArtist),
               `Times Played` ~ px(numColWidthCount)) %>%
    cols_align("left", columns = Song) %>%
    tab_options(table.font.size = 14,
                column_labels.font.size = 16)
  
}


