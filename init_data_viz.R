# 11/11/2026 | Math 167R Group Project: Initial Data Visualization
# Goal: familiarize ourselves with the dataset and determine which
# advanced statistical techniques are appropriate.

# Compare burned area with spatial, fire-weather, and temperature variables.
# Use the same y-axis limits in every panel so comparisons stay meaningful.
plot_fire_relationships <- function(data) {
  previous_par <- par(no.readonly = TRUE)
  on.exit(par(previous_par))

  par(
    mfrow = c(2, 3),
    mar = c(4.8, 4.8, 3.2, 1.2),
    oma = c(3, 0, 4, 0),
    family = "sans",
    bg = "white",
    fg = "#334155",
    col.axis = "#475569",
    col.lab = "#334155",
    col.main = "#0F172A",
    cex.axis = 0.9,
    cex.lab = 1,
    cex.main = 1.1,
    las = 1,
    bty = "n",
    tcl = -0.25
  )

  panels <- list(
    list("X", "Location in the park", "Park grid coordinate (X)"),
    list("FFMC", "Fine fuel moisture", "Fine Fuel Moisture Code (FFMC)"),
    list("DMC", "Duff moisture", "Duff Moisture Code (DMC)"),
    list("DC", "Drought conditions", "Drought Code (DC)"),
    list("ISI", "Potential fire spread", "Initial Spread Index (ISI)"),
    list("temp", "Air temperature", expression("Temperature (" * degree * "C)"))
  )

  for (panel in panels) {
    plot(
      data[[panel[[1]]]], data$area,
      type = "n",
      main = panel[[2]],
      xlab = panel[[3]],
      ylab = "Burned area (ha)",
      ylim = c(0, max(data$area, na.rm = TRUE))
    )
    # Draw the grid first so it stays behind the observations.
    abline(h = axTicks(2), v = axTicks(1), col = "#E2E8F0", lwd = 0.8)
    points(
      data[[panel[[1]]]], data$area,
      pch = 16, cex = 0.85,
      col = adjustcolor("#087F8C", alpha.f = 0.45)
    )
  }

  mtext(
    "What relates to forest fire size?", side = 3, outer = TRUE,
    line = 1.8, cex = 1.5, font = 2, col = "#0F172A"
  )
  mtext(
    "Montesinho Natural Park, Portugal", side = 3, outer = TRUE,
    line = 0.3, cex = 1, col = "#475569"
  )
  mtext(
    sprintf(
      "%s observations | Burned area shown on its original scale | Source: Cortez & Morais (2007)",
      format(nrow(data), big.mark = ",")
    ),
    side = 1, outer = TRUE, line = 1, cex = 0.85, col = "#64748B"
  )
}

plot_fire_relationships(forest_fires)

# For a presentation-sized export, run:
# png("forest_fire_relationships.png", width = 1800, height = 1200, res = 150)
# plot_fire_relationships(forest_fires)
# dev.off()
