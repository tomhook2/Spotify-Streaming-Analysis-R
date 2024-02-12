# Clear environment
source("R Scripts/Clear Environment.r")

vecDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Overall - GT tables

tblDayOfWeekSummary <- tblListeningData %>%
  group_by(`Day of Week` = format(Date, "%A")) %>%
  summarise(`Hours Played` = sum(ms_played)/1000/60/60) %>%
  ungroup() %>%
  arrange(desc(`Hours Played`)) %>%
  mutate(Percent = percent(`Hours Played` / sum(`Hours Played`), scale = 100, accuracy = 0.1),
         `Hours Played` = round(`Hours Played`, 0)) %>%
  select(`Day of Week`, Percent, `Hours Played`)

gtDayOfWeekSummary <- tblDayOfWeekSummary%>%
  gt() %>%
  cols_width(`Day of Week` ~ px(120),
             `Percent` ~ px(100),
             `Hours Played` ~ px(120))

textDayOfWeekSummary <- 
  if (tblDayOfWeekSummary$`Day of Week`[1] %in% c("Saturday", "Sunday") &&
      tblDayOfWeekSummary$`Day of Week`[2] %in% c("Saturday", "Sunday")) {
  "Looking at the table, it looks like you enjoy listening mostly on weekends!"
} else if (tblDayOfWeekSummary$`Day of Week`[6] %in% c("Saturday", "Sunday") &&
           tblDayOfWeekSummary$`Day of Week`[7] %in% c("Saturday", "Sunday")) {
  "Looking at the table, it looks like you enjoy listening mostly on weekdays!"
} else {
  ""
}

# Heatmaps (not in use)

tblDayOfWeekByYear <- tblListeningData %>%
  group_by(`Day of Week` = format(Date, "%A"),
           Year = year(Date)) %>%
  summarise(TimePlayed = sum(ms_played)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(`Percent` = (TimePlayed/sum(TimePlayed))) %>%
  select(-TimePlayed) %>%
  arrange(Year, match(`Day of Week`, vecDays)) %>%
  mutate(`Day of Week` = factor(`Day of Week`, levels = rev(vecDays)))

heatDayOfWeekByYear <- ggplot(tblDayOfWeekByYear, aes(x = Year, y = `Day of Week`, fill = Percent)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#1db953", labels = percent_format(scale = 100)) +
  labs(title = "Percentage Playtime by Day of Week for each Year",
       x = "Year",
       y = "Day of Week") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_text(aes(label = paste0(round(Percent * 100), "%")), size = 3.5) + 
  theme(legend.position = "none")

  
