# Clear environment
source("R Scripts/Clear Environment.r")

# Text numbers

tblMaxPlayTime <- tblListeningData %>%
  group_by(YearMonth = format(Date, "%B %Y")) %>%
  summarise(TotalTimeMinutes = round(sum(ms_played)/1000/60, 0)) %>%
  ungroup() %>%
  arrange(desc(TotalTimeMinutes)) %>%
  head(1)

mnthMaxPlayTime <- tblMaxPlayTime$YearMonth
numMaxPlayTime <- format(tblMaxPlayTime$TotalTimeMinutes, big.mark=",")

# Create a tbl with days every day (to fill in days where there's no data) for rolling avg
dateMin <- min(tblListeningData$Date)
dateMax <- max(tblListeningData$Date)

tblDates <- data.frame(Date = seq(dateMin, dateMax, by = "1 day"))

# VARIABLES: 
# No. rolling days ~ formula to estimate an appropriate rolling avg based on amount of data
numRollingDays <- ceiling((dateMax - dateMin)/25/30)*30

# X axis breaks, a little higher than the rolling days, in months
numXAxisBreak <- ceiling((dateMax - dateMin)/500/3)*3

# Charts

# 1 - Music and Podcasts Combined

# Table of minutes played per day
tblRollingListeningTime <- tblDates %>%
  left_join(tblListeningData %>%
              group_by(Date) %>%
              summarise(HoursPlayed = sum(ms_played)/1000/60/60) %>%
              ungroup(),
            by = "Date") %>%
  mutate(HoursPlayed = ifelse(is.na(HoursPlayed), 0, HoursPlayed)) %>%
  mutate(RollingAvgHoursPlayedPerDay = rollapply(HoursPlayed, width = numRollingDays, FUN = mean, align = "right", fill = NA)) %>%
  filter(Date >= dateMin + numRollingDays - 1)

chrtRollingListeningTime <- ggplot(tblRollingListeningTime, aes(x = Date, y = RollingAvgHoursPlayedPerDay)) +
  geom_line() +
  labs(title = paste("Music & Podcasts Combined Time Played -", numRollingDays, "Day Rolling Average"), x = "Date", y = "Hours per Day") +
  scale_x_date(date_breaks = paste(numXAxisBreak, "months"), date_labels = "%b-%y") +
  expand_limits(y = 0) +
  theme_minimal()

#plotlyRollingListeningTime <- ggplotly(chrtRollingListeningTime) %>%
#  layout(title = paste("Music & Podcasts Combined Time Played -", numRollingDays, "Day Rolling Average")) %>%
#  config(scrollZoom = FALSE, displayModeBar = FALSE, modeBarButtonsToRemove = list('zoomIn2d', 'zoomOut2d'))

# 2 - Music Only

# Table of minutes played per day
tblRollingListeningTimeMusic <- tblDates %>%
  left_join(tblListeningData %>%
              filter(Type == "Song") %>%
              group_by(Date) %>%
              summarise(HoursPlayed = sum(ms_played)/1000/60/60) %>%
              ungroup(),
            by = "Date") %>%
  mutate(HoursPlayed = ifelse(is.na(HoursPlayed), 0, HoursPlayed)) %>%
  mutate(RollingAvgHoursPlayedPerDay = rollapply(HoursPlayed, width = numRollingDays, FUN = mean, align = "right", fill = NA)) %>%
  filter(Date >= dateMin + numRollingDays - 1)

chrtRollingListeningTimeMusic <- ggplot(tblRollingListeningTimeMusic, aes(x = Date, y = RollingAvgHoursPlayedPerDay)) +
  geom_line() +
  labs(title = paste("Music Time Played -", numRollingDays, "Day Rolling Average"), x = "Date", y = "Hours per Day") +
  scale_x_date(date_breaks = paste(numXAxisBreak, "months"), date_labels = "%b-%y") +
  expand_limits(y = 0) +
  theme_minimal()

# 3 - Podcasts Only

# Table of minutes played per day
tblRollingListeningTimePodcast <- tblDates %>%
  left_join(tblListeningData %>%
              filter(Type == "Podcast") %>%
              group_by(Date) %>%
              summarise(HoursPlayed = sum(ms_played)/1000/60/60) %>%
              ungroup(),
            by = "Date") %>%
  mutate(HoursPlayed = ifelse(is.na(HoursPlayed), 0, HoursPlayed)) %>%
  mutate(RollingAvgHoursPlayedPerDay = rollapply(HoursPlayed, width = numRollingDays, FUN = mean, align = "right", fill = NA)) %>%
  filter(Date >= dateMin + numRollingDays - 1)

chrtRollingListeningTimePodcast <- ggplot(tblRollingListeningTimePodcast, aes(x = Date, y = RollingAvgHoursPlayedPerDay)) +
  geom_line() +
  labs(title = paste("Podcasts Time Played -", numRollingDays, "Day Rolling Average"), x = "Date", y = "Hours per Day") +
  scale_x_date(date_breaks = paste(numXAxisBreak, "months"), date_labels = "%b-%y") +
  expand_limits(y = 0) +
  theme_minimal()
