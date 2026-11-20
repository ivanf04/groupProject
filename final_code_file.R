#Final Code for 167R Group Project - Group 2
  #Kristopher Asuncion, Ivan Faranda, Ryan Wong 

#Section 0: Setup
  #import data set & clean zero values 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
forest_fires <- read.csv(url, stringsAsFactors = TRUE)

forest_fires_01 <- forest_fires[forest_fires$area>0,]   #dataset with area > 0

#load ggplot2
library(ggplot2)


#Section 1: Initial descriptive statistics of our data set

#Histogram of Burned Area 

area_histogram = ggplot(forest_fires, aes(x = area)) +
  geom_histogram(fill= "steelblue",na.rm = TRUE) + #blue color, remove NA vals
  labs( #add labels 
    title = "Distribution of Burned Area",
    x = "Burned area (hectares)",
    y = "Number of observations",
  )
print(area_histogram) #display histogram in Rstudio

#compute descriptive stats and a 95% confidence interval for the mean
  #written as a function to be called twice; once for each dataset
summarize_area <- function(area) {
  
  #calculate basic stats
  area = area[!is.na(area)]  #remove NA values 
  n = length(area)
  mean_area = mean(area)
  quartiles = quantile(area,c(0.25, 0.75), names=FALSE)
  margin <- qt(0.975, df = n-1) * sd(area) / sqrt(n) #used to find CI 
  
  #R functions return last expression by default; function will return this vector
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

#Call summary function on both data sets, use cbind to combine the results 
area_summary = cbind(
  "All records" = summarize_area(forest_fires$area),
  "Positive-area records" = summarize_area(forest_fires_01$area)
)

#format the table to look pretty in the report 
format_area_summary <- function(summary) {
  format_area <- function(value) {
    #2 digits after the decimal, add "," for large values 
    formatC(value, format = "f",digits = 2, big.mark = ",")
  }
  c(
    format(summary["records"], big.mark = ",", trim = TRUE), #add "," remove blanks
    format_area(summary[c("mean", "median", "q1", "q3", "min", "max")]),#call formatting function on stats
    paste(format_area(summary[c("ci_lower","ci_upper")]), collapse = " to ") #add "to" between upper and lower intervals
  )
}

#apply formatting function above to the area summary vector 
  #and add descriptive row names 
comparison_table <- as.data.frame(apply(area_summary, 2, format_area_summary))
rownames(comparison_table) <- c(
  "Records", "Mean (ha)","Median (ha)", "First quartile (ha)", "Third quartile (ha)",
  "Minimum (ha)", "Maximum (ha)", "95% CI for mean (ha)"
)

#display table in the console
comparison_table

#Section 2: Data visualization 

#plotting layout
par(mfrow = c(1,1))

#boxplot of temperature
boxplot(
  forest_fires_01$temp,
  main = "Boxplot of Temperature",
  ylab = "Temperature (°C)",
  col = "gray"
)

#find index of outliers 
which(forest_fires_01$temp < 5)
#index one outlier
forest_fires_01[130,]

#histogram of temperature
par(bg = "gray90")

hist(
  forest_fires_01$temp,
  main = "Distribution of Temperature",
  xlab = "Temperature (°C)",
  ylab = "Frequency",
  breaks = 20,
  col = "steelblue",
  border = "white"
)

#histogram of log area
hist(
  log(forest_fires_01$area),
  main = "Distribution of Area Burned",
  xlab = "Natural Log of Area Burned",
  ylab = "Frequency",
  breaks = 20,
  col = "steelblue",
  border = "white"
)
#find outlier index
which.max(forest_fires_01$area)
#index outlier
forest_fires_01[101,]

#scatter plot temperature v. log area
plot(log(forest_fires_01$area) ~  forest_fires_01$temp,
     xlab = "Temperature (°C)",
     ylab = "Log(Area Burned)",
     main = "Temperature vs. Log Area Burned",
     pch = 19,
     col = "steelblue")

#proposed linear regression model (temp w/ log area) 
lm_fit <- lm(log(area) ~ temp, data = forest_fires_01)

abline(lm_fit, lwd = 2)

#Q-Q residual plot
qqnorm(
  residuals(lm_fit),
  main = "Normal Q-Q Plot of Regression Residuals",
  xlab = "Theoretical Normal Quantiles",
  ylab = "Observed Residuals",
  pch = 21,
  bg = "steelblue",
  col = "white",
  cex = 1.2,
  cex.main = 1.3,
  cex.lab = 1.1,
  panel.first = grid(col = "gray85", lty = 1)
)

qqline(
  residuals(lm_fit),
  col = "black",
  lwd = 2
)


#Section 3: Statistical analysis

#H_0: B_1 = 0, H_a: B_1!=0
lm_fit <- lm(log(area)~temp,forest_fires_01)
coefficients(lm_fit) #provides the coefficients
summary(lm_fit) #produced estimates, std. error, t-value, and p-value
confint(lm_fit) #produced confidence interval
qt(0.975, df = 268) #produces critical value

#H_0: 
anova(lm_fit) #produced f-value
qf(0.95,1,268) #produced f-critical value

#This plot was not used in our report
ggplot(forest_fires_01, aes(x = temp, y = log(area + 1)))+ # (check for linearity)
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
plot(lm_fit, which = 1)  #Residual vs. Fitted  (check for constant variance, independence) 
plot(lm_fit, which = 2)  #Q-Q Plot (check for normality of errors)
plot(lm_fit, which = 3)  #Scale-Location
plot(lm_fit, which = 4)  #Cook's distance


days_ordered <- factor( 
  forest_fires_01$day, 
  levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
)

barplot(  #bar plot
  table(days_ordered), #table
  main = "Number of Forest Fires by Day of the Week",
  xlab = "Day of the Week",
  ylab = "Number of Fires",
  col = "steelblue"
)


