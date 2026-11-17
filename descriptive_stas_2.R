#regression with data without zeros 

lm_fit_01 = lm(log1p(area) ~ temp,forest_fires_01)
summary(lm_fit_01)

lm_fit_02 = lm(area ~ temp,forest_fires_01)
summary(lm_fit_02)