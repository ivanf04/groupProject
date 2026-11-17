#File to read csv and load dataset into the R environment 

forest_fires = read.csv("forest+fires/forestfires.csv")

#preview dataset 
head(forest_fires)

if (interactive()) View(forest_fires)

#data with area == 0 parsed out 
forest_fires_01 = forest_fires[forest_fires$area != 0,]
