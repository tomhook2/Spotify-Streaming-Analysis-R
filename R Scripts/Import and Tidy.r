# Install/Load the libraries
if(!require("pacman")) install.packages("pacman"); library (pacman)
pacman::p_load(rstudioapi, jsonlite, dplyr, ggplot2, zoo, ggrepel, openxlsx)

# Clear environment
rm(list = ls())

# Write name below
strName <- "Your_Name_Here"

# unzip streaming data
unzip(paste0("Data/", strName, "/extended_streaming.zip"), exdir = paste0("Data/", strName))

# Get all the .json files in the directory and remove any "video" (for now)
listJSONFiles <- list.files(path = paste0("Data/", strName, "/MyData"), pattern = "\\.json$", full.names = TRUE)
listJSONFiles <- listJSONFiles[!grepl("Video", listJSONFiles)]

# Initialize an empty list to store the data frames
tblListeningData <- list()

for (i in seq_along(listJSONFiles)) {
  # Read the JSON file and assign it to a temporary data frame
  tblListeningData[[i]] <- fromJSON(listJSONFiles[i])
}

# Combine all data frames in tblListeningData into a single data frame
tblListeningData <- bind_rows(tblListeningData)

# Tidy data
tblListeningData <- tblListeningData %>%
  mutate(Date = as.Date(substring(ts, 1, 10), format = "%Y-%m-%d"),
         Time = format(strptime(substr(ts, nchar(ts) - 8, nchar(ts) - 1), format = "%H:%M:%S"), format = "%H:%M:%S"),
         Type = ifelse(grepl("episode", spotify_episode_uri), "Podcast", "Song"))

# Remove poor DQ days
tblPoorDQDays <- tblListeningData %>%
  group_by(Date) %>%
  summarise(TotalTimeHours = round(sum(ms_played)/1000/60/60, 1)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeHours)) %>%
  filter(TotalTimeHours > 24) 

tblListeningData <- tblListeningData %>%
  anti_join(tblPoorDQDays, by = "Date")

# Save data
saveRDS(tblListeningData, paste0("Data/", strName, "/Listening Data.rds"))


