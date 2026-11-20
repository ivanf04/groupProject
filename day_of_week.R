# Plot burned area by day of the week. Run from the project directory.
forest_fires <- read.csv("forest+fires/forestfires.csv")

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_positions <- match(forest_fires$day, tolower(substr(weekdays, 1, 3)))

# Spread points horizontally to reveal overlapping observations within each day.
plot(
  jitter(day_positions, amount = 0.19),
  forest_fires$area,
  xlim = c(0.5, 7.5),
  xaxt = "n",
  xlab = "Day of week",
  ylab = "Burned area (ha)",
  main = "Burned area by day of week",
  pch = 16,
  col = adjustcolor("#087F8C", alpha.f = 0.40)
)
axis(1, at = seq_along(weekdays), labels = weekdays, cex.axis = 0.85)
