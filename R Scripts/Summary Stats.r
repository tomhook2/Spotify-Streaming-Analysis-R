# Clear environment
source("R Scripts/Clear Environment.r")

# First and most recent listen
datetimeFirstListen <- as.POSIXct(min(tblListeningData$ts), format = "%Y-%m-%dT%H:%M:%SZ")
dateFirstListen <- format(datetimeFirstListen, "%d %B %Y")
timeFirstListen <- format(datetimeFirstListen, "%H:%M")

datetimeLatest <- as.POSIXct(max(tblListeningData$ts), format = "%Y-%m-%dT%H:%M:%SZ")
dateLatest <- format(datetimeLatest, "%d %B %Y")

timeDifference <- difftime(datetimeLatest, datetimeFirstListen)
timeDifferenceYears <- round(timeDifference/365.25, 1) 

# Over the history of the account stats

# 1
tblListenTimeSummary <- tblListeningData %>%
  filter(!is.na(master_metadata_track_name),
         Type == "Song") %>%
  group_by() %>%
  summarise(SongCount = n(),
            TotalTimeHours = round(sum(ms_played)/1000/60/60, 0),
            TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeHours))

numListenTimeHours <- format(tblListenTimeSummary$TotalTimeHours, big.mark=",")
numListenTimeMinutes <- format(tblListenTimeSummary$TotalTimeMinutes, big.mark=",")

numListenPerDayMinutes <- round(tblListenTimeSummary$TotalTimeMinutes/as.numeric(timeDifference), 0)
pctListenPercent <- label_percent(accuracy = 0.1)(tblListenTimeSummary$TotalTimeHours/as.numeric(timeDifference)/24) # timeDifference is in days

# 2
tblSongSummary <- tblListeningData %>%
  filter(Type == "Song",
         !is.na(master_metadata_track_name),
         ms_played >= 10000) %>%
  group_by(master_metadata_album_artist_name, master_metadata_track_name, spotify_track_uri) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  head(1)

strMaxSongName <- tblSongSummary$master_metadata_track_name[1]
strMaxSongArtist <- tblSongSummary$master_metadata_album_artist_name[1]
cntMaxSongCount <- tblSongSummary$Count[1]
strMaxSongLink <- tblSongSummary$spotify_track_uri[1]
strMaxSongLink2 <- paste0("https://open.spotify.com/track/", gsub("spotify:track:", "", tblSongSummary$spotify_track_uri[1]))

numAverageDaysBetweenMaxSong <- round(timeDifference/cntMaxSongCount, 1)

# 4
tblArtistSummary <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(SongCount = n_distinct(spotify_track_uri),
            TotalTimeHours = round(sum(ms_played)/1000/60/60, 0),
            TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeMinutes))

strMaxArtist <- tblArtistSummary$master_metadata_album_artist_name[1]
cntMaxArtistSongs <- tblArtistSummary$SongCount[1]
numMaxArtistTimeHours <- format(tblArtistSummary$TotalTimeHours[1], big.mark=",")
numMaxArtistTimeMinutes <- format(tblArtistSummary$TotalTimeMinutes[1], big.mark=",")

# 5
tblPodcastEpisodeSummary <- tblListeningData %>%
  filter(Type == "Podcast") %>%
  group_by(episode_name, episode_show_name , spotify_episode_uri) %>%
  summarise(Count = n()) %>%
  ungroup()

cntPodcastEpisodeCount <- nrow(tblPodcastEpisodeSummary)

tblPodcastShowSummary <- tblListeningData %>%
  filter(Type == "Podcast",
         ms_played >= 10000) %>%
  group_by(episode_show_name) %>%
  summarise(Count = n_distinct(spotify_episode_uri),
            TotalTimeHours = round(sum(ms_played)/1000/60/60, 0),
            TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeMinutes))

cntPodcastShowCount <- nrow(tblPodcastShowSummary)
strPodcastShowMax <- tblPodcastShowSummary$episode_show_name[1]
numPodcastShowTimeMax <- tblPodcastShowSummary$TotalTimeHours[1]

# 3
cntDistinctArtists <- tblListeningData %>%
  filter(Type == "Song",
         !is.na(master_metadata_track_name),
         ms_played >= 10000) %>%
  distinct(master_metadata_album_artist_name) %>%
  n_distinct() %>%
  format(big.mark=",")
  
# 6 (not currently using)
tblMaxListen <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(Date) %>%
  summarise(TotalTimeHours = round(sum(ms_played)/1000/60/60, 1),
            TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeMinutes)) %>%
  filter(TotalTimeHours <= 24) %>% # data quality issues where some times play time is > 24hrs
  head(1)

dateMaxListen <- tblMaxListen$Date[1]
numMaxListenHours <- tblMaxListen$TotalTimeHours[1]
numMaxListenMinutes <- tblMaxListen$TotalTimeMinutes[1]
