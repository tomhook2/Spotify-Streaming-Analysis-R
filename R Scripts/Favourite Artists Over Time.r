# Clear environment
source("R Scripts/Clear Environment.r")

# TRY TO UNDERSTAND WHY tblArtistPlayTimeChartRollingAvg WORKS (ChatGPT wrote it...)

# Choose number of artists to look at on chart
numArtistsChart <- 8
numArtistsTable <- 25

# CHART

# Create a tbl with days every day (to fill in days where there's no data) for rolling avg
dateMin <- min(tblListeningData$Date)
dateMax <- max(tblListeningData$Date)

tblDates <- data.frame(Date = seq(dateMin, dateMax, by = "1 day"))

# VARIABLES: 
# No. rolling days ~ formula to estimate an appropriate rolling avg based on amount of data
numRollingDays <- ceiling((dateMax - dateMin)/15/30)*30

# Total listening rolling avg

tblPlayTimeRollingAvg <- tblDates %>%
  left_join(tblListeningData %>%
              filter(Type == "Song") %>%
              group_by(Date) %>%
              summarise(TotalTimeMinutes = sum(ms_played)/1000/60) %>%
              ungroup(),
            by = "Date") %>%
  mutate(TotalTimeMinutes = ifelse(is.na(TotalTimeMinutes), 0, TotalTimeMinutes)) %>%
  mutate(TotalRollingAvg = rollapply(TotalTimeMinutes, width = numRollingDays, FUN = mean, align = "right", fill = NA)) %>%
  filter(Date >= dateMin + numRollingDays - 1)

# Get top n artists of all time

tblMostPlayedArtistsChart <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(TotalTime = sum(ms_played)) %>%
  ungroup() %>%
  arrange(desc(TotalTime)) %>%
  select(master_metadata_album_artist_name) %>%
  head(numArtistsChart)

tblArtistPlayTimeChart <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(Date, master_metadata_album_artist_name) %>%
  summarise(TotalTimeMinutes = sum(ms_played)/1000/60) %>%
  ungroup() %>%
  filter(master_metadata_album_artist_name %in% tblMostPlayedArtistsChart$master_metadata_album_artist_name) %>%
  pivot_wider(names_from = master_metadata_album_artist_name, values_from = TotalTimeMinutes)

tblArtistPlayTimeChartRollingAvg <- tblDates %>%
  left_join(tblArtistPlayTimeChart, by = "Date") %>%
  mutate(across(-one_of("Date"), ~ifelse(is.na(.), 0, .))) %>% 
  mutate_at(vars(-one_of("Date")), 
            ~rollapply(.x, width = numRollingDays, FUN = mean, align = "right", fill = NA)) %>%
  filter(Date >= dateMin + numRollingDays - 1) %>%
  pivot_longer(cols = -Date, 
               names_to = "Artist", 
               values_to = "RollingAvg") %>%
  left_join(tblPlayTimeRollingAvg, by = "Date") %>%
  mutate(Percentage = RollingAvg/TotalRollingAvg*100)

# Charts, total and percent

chrtArtistRollingAvg <- ggplot(tblArtistPlayTimeChartRollingAvg, aes(x = Date, y = RollingAvg, color = Artist)) +
  geom_line() +
  labs(title = paste("Listen Time by Artist -", numRollingDays, "Day Rolling Average"),
       x = "Date",
       y = "Minutes per Day",
       color = "Artist") +
  theme_minimal() + 
  guides(color = guide_legend(override.aes = list(linewidth = 1.2)))

chrtArtistRollingAvgPct <- ggplot(tblArtistPlayTimeChartRollingAvg, aes(x = Date, y = Percentage, color = Artist)) +
  geom_line() + 
  labs(title = paste("Listen Time by Artist (Percent) -", numRollingDays, "Day Rolling Average"),
       x = "Date",
       y = "Percentage",
       color = "Artist") +
  theme_minimal() + 
  guides(color = guide_legend(override.aes = list(linewidth = 1.2)))

# TABLE

# Create total listening per year table

tblPlayTimeTotal <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(Year = format(Date, "%Y")) %>%
  summarise(TotalTime = sum(ms_played)) %>%
  ungroup()

# Get top n artists of all time

tblMostPlayedArtistsTable <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(master_metadata_album_artist_name) %>%
  summarise(ArtistTime = sum(ms_played)) %>%
  ungroup() %>%
  arrange(desc(ArtistTime)) %>%
  select(master_metadata_album_artist_name) %>%
  head(numArtistsTable)

# GT Table

tblArtistPlayTimeTableGT <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(Year = format(Date, "%Y"), master_metadata_album_artist_name) %>%
  summarise(ArtistTime = sum(ms_played)) %>%
  ungroup() %>%
  filter(master_metadata_album_artist_name %in% tblMostPlayedArtistsTable$master_metadata_album_artist_name) %>%
  left_join(tblPlayTimeTotal, by = "Year") %>%
  mutate(Percent = percent(ArtistTime / TotalTime, scale = 100, accuracy = 0.1)) %>%
  select(Year, master_metadata_album_artist_name, Percent) %>%
  pivot_wider(names_from = Year, values_from = Percent) 

gtArtistPlayTimeTable <- tblMostPlayedArtistsTable %>%
  left_join(tblArtistPlayTimeTableGT, by = c("master_metadata_album_artist_name")) %>% # Reordering from highest to lowest hours played
  mutate_all(~ifelse(is.na(.), "0.0%", .)) %>%
  rename("Artist" = master_metadata_album_artist_name) %>%
  gt() %>%
  cols_width(`Artist` ~ px(210),
             everything() ~ px(75)) %>%
  tab_options(table.font.size = 14,
              column_labels.font.size = 16)

# Heatmap

vecMostPlayedArtistsTable <- tblMostPlayedArtistsTable$master_metadata_album_artist_name

tblArtistPlayTimeTableHeat <- tblListeningData %>%
  filter(Type == "Song") %>%
  group_by(Year = format(Date, "%Y"), master_metadata_album_artist_name) %>%
  summarise(ArtistTime = sum(ms_played)) %>%
  ungroup() %>%
  filter(master_metadata_album_artist_name %in% tblMostPlayedArtistsTable$master_metadata_album_artist_name) %>%
  left_join(tblPlayTimeTotal, by = "Year") %>%
  mutate(Percent = ArtistTime/TotalTime) %>%
  select(Year, master_metadata_album_artist_name, Percent) 

# Order high to low
tblArtistPlayTimeTableHeat$master_metadata_album_artist_name <- factor(
  tblArtistPlayTimeTableHeat$master_metadata_album_artist_name,
  levels = rev(vecMostPlayedArtistsTable)
)

heatArtistPlayTime <- ggplot(tblArtistPlayTimeTableHeat, aes(x = Year, y = master_metadata_album_artist_name, fill = Percent)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#1db953", labels = percent_format(scale = 100)) +
  labs(title = "Percentage Playtime by Artist for each Year", x = "Year", y = "Artist", fill = "Percent") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_text(aes(label = paste0(round(Percent * 100), "%")), size = 3.5) + 
  theme(legend.position = "none")

# Width for heatmap
numWidthHeatmap <- max(6, length(unique(substr(tblListeningData$Date, 1, 4))) + 1)




