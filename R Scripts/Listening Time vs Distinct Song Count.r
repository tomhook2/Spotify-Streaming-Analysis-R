# Clear environment
source("R Scripts/Clear Environment.r")

# Text variables
tblArtistSummary <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(SongCount = n_distinct(spotify_track_uri),
            TotalTimeHours = round(sum(ms_played)/1000/60/60, 0),
            TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeMinutes))

strMaxArtist <- tblArtistSummary$master_metadata_album_artist_name[1]

tblArtistSummary <- tblArtistSummary %>%
  arrange(desc(SongCount),
          desc(TotalTimeMinutes))

strMaxArtistSongCount <- tblArtistSummary$master_metadata_album_artist_name[1]

boolArtistTimeCount <- if (strMaxArtist == strMaxArtistSongCount) {
  TRUE
} else {
  FALSE
}

textMaxArtist <- 
  if (boolArtistTimeCount == TRUE) {
    paste0("We know you've spent the most amount of time listening to ", strMaxArtist, " over the history of the account,
           and it turns out they are also the artist you've played the most distinct songs from.")
  } else {
    paste0("We know you've spent the most amount of time listening to ", strMaxArtist, " over the history of the account,
           but it's actually ", strMaxArtistSongCount, " that have the most distinct songs you've listened to.")
  }

# Charts

tblTimeVsCount <- tblListeningData %>%
  filter(Type == "Song",
         !is.na(master_metadata_track_name),
         ms_played >= 10000) %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(`Distinct Songs` = n_distinct(spotify_track_uri),
            `Time Played (Hours)` = sum(ms_played)/1000/60/60) %>%
  ungroup() %>%
  arrange(desc(`Time Played (Hours)`)) %>%
  head(25) %>%
  rename(Artist = master_metadata_album_artist_name)

chrtTimeVsCount <- ggplot(tblTimeVsCount, aes(x = `Distinct Songs`, y = `Time Played (Hours)`, color = Artist, label = Artist)) +
  geom_point() +
  ggtitle("Artists by Distinct Songs Played, and Time Spent Listening") +
  xlab("Distinct Songs Played") +
  ylab("Total Hours Played") +
  theme_minimal() +
  geom_text_repel() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none")

lstTimeVsCountTables <- list()
lstTimeVsCountCharts <- list()
vecYears <- unique(substr(tblListeningData$Date, 1, 4))

for (i in seq_along(vecYears)) {
  
  tblName <- paste0("tblTimeVsCount", vecYears[i])
  
  lstTimeVsCountTables[[tblName]] <- tblListeningData %>%
    filter(Type == "Song",
           !is.na(master_metadata_track_name),
           ms_played >= 10000,
           substr(Date, 1, 4) == vecYears[i]) %>%
    group_by(master_metadata_album_artist_name) %>%
    summarise(`Distinct Songs` = n_distinct(spotify_track_uri),
              `Time Played (Hours)` = sum(ms_played)/1000/60/60) %>%
    ungroup() %>%
    arrange(desc(`Time Played (Hours)`)) %>%
    head(25) %>%
    rename(Artist = master_metadata_album_artist_name)
  
  chrtName <- paste0("chrtTimeVsCount", vecYears[i])
  
  lstTimeVsCountCharts[[chrtName]] <- ggplot(lstTimeVsCountTables[[i]], aes(x = `Distinct Songs`, y = `Time Played (Hours)`, color = Artist, label = Artist)) +
    geom_point() +
    ggtitle(paste("Artists by Distinct Songs Played, and Time Spent Listening during", vecYears[i])) +
    xlab("Distinct Songs Played") +
    ylab("Total Hours Played") +
    theme_minimal() +
    geom_text_repel() +
    theme(panel.grid = element_blank()) +
    theme(legend.position = "none")
  
}

