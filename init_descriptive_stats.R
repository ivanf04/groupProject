#inital descriptive statistics 

mean_area = mean(forest_fires$area)
median_area = median(forest_fires$area)

no_burn = sum(forest_fires$area==0, na.rm = TRUE)

Q1 = quantile(forest_fires$area, 0.25, na.rm=TRUE)
Q3 = quantile(forest_fires$area, 0.75, na.rm=TRUE)
min = min(forest_fires$area, na.rm = TRUE)
max = max(forest_fires$area, na.rm = TRUE)


library(ggplot2)
ggplot(forest_fires, aes(x = temp, y = area)) + 
         geom_point() +
        geom_smooth(method= "lm", se = FALSE) +
  scale_y_continuous(transform = "log")

lm_fit = lm(log1p(area) ~ temp + wind + rain,forest_fires)

summary(lm_fit)
ggplot(forest_fires, aes(x = rain, y = area)) + 
  geom_point()


ggplot(forest_fires, aes(x = ISI, y = area)) + 
  geom_point() +
  geom_smooth(method= "lm", se = FALSE)

lm_fit = lm(area ~ ISI,forest_fires)

ggplot(forest_fires, aes(x = , y = area)) + 
  geom_point() +
  geom_smooth(method= "lm", se = FALSE)

lm_fit = lm(area~ISI,forest_fires)c

lm_fit2 = lm(log1p(area) ~ RH,forest_fires)
summary(lm_fit2)