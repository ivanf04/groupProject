#inital descriptive statistics 

mean_area = mean(forest_fires$area)
median_area = median(forest_fires$area)

no_burn = sum(forest_fires$area==0, na.rm = TRUE)

Q1 = quantile(forest_fires$area, 0.25, na.rm=TRUE)
Q3 = quantile(forest_fires$area, 0.75, na.rm=TRUE)
min = min(forest_fires$area, na.rm = TRUE)
max = max(forest_fires$area, na.rm = TRUE)

