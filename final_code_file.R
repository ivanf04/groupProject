#Final Submission 

#Section 0, Setup: import dataset & clean zero values 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
forest_fires <- read.csv(url, stringsAsFactors = TRUE)

forest_fires_01 <- forest_fires[forest_fires$area>0,]


