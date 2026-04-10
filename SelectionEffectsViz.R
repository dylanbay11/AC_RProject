# Examining possible selection effects (hardcore vs casual fans) by survey day
source(here("Cleaning.R"))
library(ggplot2)
library(ggridges)

# the weekenders were possibly more hardcore fans?
ggplot(df, aes(x = total_played, y = reorder(paste(RecordedDay, "-", RecordedWeekday), RecordedDay), 
      fill = as.factor(RecordedDay))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_d(name = "Total Played", option = "C", alpha = .7) +
  labs(title = "Shift in total_played Distribution by Response Day",
       y = "Survey Day", x = "Total Played Value") +
  theme_ridges()

# alternate visualization type without the end-smoothing (but still proportional within-day)
ggplot(df, aes(x = total_played, y = reorder(paste(RecordedDay, "-", RecordedWeekday), RecordedDay), fill = as.factor(RecordedDay))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, stat = "binline", bins = 12, draw_baseline = FALSE) +
  scale_fill_viridis_d(option = "C", guide = "none") + 
  labs(title = "Shift in total_played Distribution by Month",
       y = "Survey Day", x = "Total Played Value") +
  theme_ridges()

# look at just the counts (needs more shifting to avoid overlap), to avoid false impressions
ggplot(df, aes(
  x = total_played, 
  y = reorder(paste(RecordedDay, "-", RecordedWeekday), RecordedDay), 
  fill = as.factor(RecordedDay),
  height = after_stat(count) # This forces the ridges to scale by response volume
)) +
  geom_density_ridges_gradient(
    scale = 2, 
    rel_min_height = 0.01, 
    stat = "binline", 
    bins = 12, 
    draw_baseline = FALSE
  ) +
  scale_fill_viridis_d(option = "C", guide = "none", alpha = .7) + 
  labs(title = "Shift in total_played Distribution by Month",
       y = "Survey Day", x = "Total Played Value") +
  theme_ridges()

# A ridgeplot of the same - though not very exciting
ggplot(df, aes(
  x = total_played, 
  y = reorder(paste(RecordedDay, "-", RecordedWeekday), RecordedDay), 
  fill = as.factor(RecordedDay),
  height = after_stat(count) # Now it has a count to grab
)) +
  geom_density_ridges(
    stat = "density",          # CRITICAL: Use this stat to enable count mapping
    scale = 1.2, 
    rel_min_height = 0.01,
    alpha = 0.7,
    trim = TRUE                # Keeps the spikes at 1 and 12 from bleeding out
  ) +
  scale_fill_viridis_d(option = "C", guide = "none") + 
  labs(
    title = "Shift in total_played Distribution by Response Day",
    subtitle = "Height reflects raw number of responses per day",
    y = "Survey Day", 
    x = "Total Played Value"
  ) +
  theme_ridges()