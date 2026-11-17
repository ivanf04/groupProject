#inital descriptive statistics 

mean_area = mean(forest_fires$area)
median_area = median(forest_fires$area)

no_burn = sum(forest_fires$area==0, na.rm = TRUE)

Q1 = quantile(forest_fires$area, 0.25, na.rm=TRUE)
Q3 = quantile(forest_fires$area, 0.75, na.rm=TRUE)
min = min(forest_fires$area, na.rm = TRUE)
max = max(forest_fires$area, na.rm = TRUE)

library(ggplot2)

# Keep the full range on a linear scale to show the long right tail.
area_histogram = ggplot(forest_fires, aes(x = area)) +
  geom_histogram(
    binwidth = 25, boundary = 0, closed = "left",
    fill = "#287D8E", color = "white", linewidth = 0.3,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(0, 1200, by = 200),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Distribution of Burned Area",
    subtitle = sprintf(
      "%s observations  |  Median: %.2f ha  |  Mean: %.2f ha",
      sum(!is.na(forest_fires$area)),
      median(forest_fires$area, na.rm = TRUE),
      mean(forest_fires$area, na.rm = TRUE)
    ),
    x = "Burned area (hectares)",
    y = "Number of observations",
    caption = "Bin width: 25 hectares."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#183544"),
    plot.subtitle = element_text(color = "#52616B", margin = margin(b = 16)),
    plot.caption = element_text(color = "#52616B", hjust = 0, margin = margin(t = 14)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5EAED"),
    axis.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 24, 18, 20)
  )

print(area_histogram)



ggplot(forest_fires, aes(x = temp, y = area)) + 
         geom_point() +
        geom_smooth(method= "lm", se = FALSE) +
  scale_y_continuous(transform = "log")

lm_fit = lm(log1p(area) ~ temp + wind + rain,forest_fires)

summary(lm_fit)
ggplot(forest_fires, aes(x = rain, y = area)) + 
  geom_point()


ggplot(forest_fires, aes(x = ISI, y = area)) + e
  geom_point() +
  geom_smooth(method= "lm", se = FALSE)

lm_fit = lm(area ~ ISI,forest_fires)

ggplot(forest_fires, aes(x = , y = area)) + 
  geom_point() +
  geom_smooth(method= "lm", se = FALSE)

lm_fit = lm(area~ISI,forest_fires)c

lm_fit2 = lm(log1p(area) ~ RH,forest_fires)
summary(lm_fit2)
ggplot(forest_fires, aes(x = day, y = area)) +
  geom_boxplot(fill = "#87CEEB", color = "#334155") +
  scale_x_discrete(
    limits = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"),
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ) +
  labs(
    title = "Burned Area by Day of the Week",
    x = "Day of the Week",
    y = "Burned Area (ha)"
  ) +
  theme_minimal(base_size = 13)

lm_fit3 = lm(log1p(area) ~ FFMC,forest_fires)
summary(lm_fit3)


ggplot(forest_fires, aes(x = FFMC, y = area) +
  geom_point() +
  geom_smooth(method ="lm")
