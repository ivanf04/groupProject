# Run 00_setup.R first before running this file 

#compute descriptive stats and a 95% confidence interval for the mean
summarize_area <- function(area) {
  
  area = area[!is.na(area)]  #remove NA values 
  n = length(area)
  mean_area = mean(area)
  quartiles = quantile(area,c(0.25, 0.75), names=FALSE)
  margin <- qt(0.975, df = n-1) * sd(area) / sqrt(n)
  
  c(
    records = n,
    mean = mean_area,
    median = median(area),
    q1 = quartiles[1],
    q3 = quartiles[2],
    min = min(area),
    max = max(area),
    ci_lower = mean_area - margin,
    ci_upper = mean_area + margin
  )
}

area_summary = cbind(
  "All records" = summarize_area(forest_fires$area),
  "Positive-area records" = summarize_area(forest_fires_01$area)
)

#format the table to look more pretty in the report 
format_area_summary <- function(summary) {
  format_area <- function(value) {
    formatC(value, format = "f",digits = 2, big.mark = ",")
  }
  c(
    format(summary["records"], big.mark = ",", trim = TRUE),
    format_area(summary[c("mean", "median", "q1", "q3", "min", "max")]),
    paste(format_area(summary[c("ci_lower","ci_upper")]), collapse = " to ")
  )
}

comparison_table <- as.data.frame(apply(area_summary, 2, format_area_summary))
rownames(comparison_table) <- c(
  "Records", "Mean (ha)","Median (ha)", "First quartile (ha)", "Third quartile (ha)",
  "Minimum (ha)", "Maximum (ha)", "95% CI for mean (ha)"
)

cat("\nBurned Area Summary\n\n")
print(comparison_table, right = TRUE)

# Draw the same table in the Plots pane or on a file graphics device.
plot_area_comparison <- function(table = comparison_table) {
  left <- 0.06
  right <- 0.94
  column_x <- c(0.59, right)
  row_y <- seq(0.71, 0.12, length.out = nrow(table))
  text_style <- grid::gpar(fontfamily = "sans", fontsize = 11, col = "#202020")
  header_style <- grid::gpar(fontfamily = "sans", fontsize = 11, fontface = "bold")
  rule_style <- grid::gpar(col = "#B8B8B8")

  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grid::grid.text("Burned Area Summary", x = left, y = 0.93, just = "left",
                  gp = grid::gpar(fontfamily = "sans", fontsize = 16, fontface = "bold"))
  grid::grid.text(colnames(table), x = column_x, y = 0.81, just = "right",
                  gp = header_style)
  grid::grid.segments(left, c(0.76, 0.07), right, c(0.76, 0.07), gp = rule_style)
  grid::grid.text(rownames(table), x = left, y = row_y, just = "left", gp = text_style)

  for (column in seq_along(table)) {
    grid::grid.text(table[[column]], x = column_x[column], y = row_y,
                    just = "right", gp = text_style)
  }
}

png("plots/burned_area_summary.png", width = 6.5, height = 3.8, units = "in", res = 300)
plot_area_comparison()
invisible(dev.off())

if (interactive()) plot_area_comparison()
