# Clear environment
source("R Scripts/Clear Environment.r")

vecYears <- unique(substr(tblListeningData$Date, 1, 4))

tblDataByYear <- data.frame(
  Year = vecYears,
  SongMinutesPlayed = character(length(vecYears)),
  SongHoursPlayed = character(length(vecYears)),
  DistinctDays = character(length(vecYears)),
  SongMostPlayed = character(length(vecYears)),
  SongMostPlayedURI = character(length(vecYears)),
  SongMostPlayedArtist = character(length(vecYears)),
  SongMostPlayedCount = character(length(vecYears)),
  ArtistMostPlayed = character(length(vecYears)),
  ArtistMostPlayedSongCount = character(length(vecYears)),
  ArtistMostPlayedTime = character(length(vecYears)),
  PodcastPlayed = logical(length(vecYears)),
  PodcastMostPlayed = character(length(vecYears)),
  PodcastMostPlayedCount = character(length(vecYears)),
  PodcastMostPlayedTime = character(length(vecYears))
)

for (i in seq_along(vecYears)) {
  tblDataByYear$Year[i] <- as.numeric(vecYears[i])
  
  # Create temp table that only contains data for the year
  tblTemp <- tblListeningData %>%
    filter(substr(Date, 1, 4) == vecYears[i])
  
  # Minutes listened to songs
  tblDataByYear$SongMinutesPlayed[i] <- tblTemp %>%
    filter(Type == "Song") %>%
    summarise(SongMinutesPlayed = format(round(sum(ms_played)/1000/60, 0), big.mark=",")) %>%
    pull(SongMinutesPlayed)
  
  # Hours listened to songs
  tblDataByYear$SongHoursPlayed[i] <- tblTemp %>%
    filter(Type == "Song") %>%
    summarise(SongHoursPlayed = format(round(sum(ms_played)/1000/60/60, 0), big.mark=",")) %>%
    pull(SongHoursPlayed)
  
  # Unique days Spotify is used
  tblDataByYear$DistinctDays[i] <- tblTemp %>%
    summarise(DistinctDays = n_distinct(Date)) %>%
    pull(DistinctDays)
  
  
  # Table for most played song
  tblMostPlayedSongByYear <- tblTemp %>%
    filter(Type == "Song",
           !is.na(master_metadata_track_name),
           ms_played >= 10000) %>%
    group_by(master_metadata_album_artist_name, master_metadata_track_name, spotify_track_uri) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    arrange(desc(Count)) %>%
    head(1)
  
  # Most played song
  tblDataByYear$SongMostPlayed[i] <- tblMostPlayedSongByYear$master_metadata_track_name
  
  # URI of the most played song
  tblDataByYear$SongMostPlayedURI[i] <- tblMostPlayedSongByYear$spotify_track_uri
  
  # Artist of the most played song
  tblDataByYear$SongMostPlayedArtist[i] <- tblMostPlayedSongByYear$master_metadata_album_artist_name
  
  # Count of the most played song
  tblDataByYear$SongMostPlayedCount[i] <- tblMostPlayedSongByYear$Count
  
  
  # Table for most played artists
  tblMostPlayedArtistByYear <- tblTemp %>%
    filter(Type == "Song") %>%
    group_by(master_metadata_album_artist_name) %>%
    summarise(SongCount = n_distinct(spotify_track_uri),
              TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
    ungroup() %>%
    arrange(desc(TotalTimeMinutes)) %>%
    head(1)
  
  # Most played artist
  tblDataByYear$ArtistMostPlayed[i] <- tblMostPlayedArtistByYear$master_metadata_album_artist_name
  
  # Unique songs from artist
  tblDataByYear$ArtistMostPlayedSongCount[i] <- tblMostPlayedArtistByYear$SongCount
  
  # Time listened to most played artist
  tblDataByYear$ArtistMostPlayedTime[i] <- format(tblMostPlayedArtistByYear$TotalTimeMinutes, big.mark=",")
  
  
  # Table for most played podcasts
  tblMostPlayedPodcastByYear <- tblTemp %>%
    filter(Type == "Podcast",
           ms_played >= 10000) %>%
    group_by(episode_show_name) %>%
    summarise(Count = n_distinct(spotify_episode_uri),
              TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
    ungroup() %>%
    arrange(desc(TotalTimeMinutes)) %>%
    head(1)
  
  # Was a podcast played this year?
  tblDataByYear$PodcastPlayed[i] <- if (nrow(tblMostPlayedPodcastByYear) > 0) {
    TRUE
  } else {
    FALSE
  }
  
  # Most played podcast show
  tblDataByYear$PodcastMostPlayed[i] <- if (tblDataByYear$PodcastPlayed[i] == TRUE) {
    tblMostPlayedPodcastByYear$episode_show_name[1]
  } else {
    NA
  }
  
  # Episode count of the most played podcast
  tblDataByYear$PodcastMostPlayedCount[i] <- if (tblDataByYear$PodcastPlayed[i] == TRUE) {
    tblMostPlayedPodcastByYear$Count[1]
  } else {
    NA
  }
  
  # Time spent listening to most played podcast
  tblDataByYear$PodcastMostPlayedTime[i] <- if (tblDataByYear$PodcastPlayed[i] == TRUE) {
    format(tblMostPlayedPodcastByYear$TotalTimeMinutes[1], big.mark=",")
  } else {
    NA
  }
}
