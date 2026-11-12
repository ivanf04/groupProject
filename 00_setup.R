#File to read csv and load dataset into the R environment 

forest_fires = read.csv("forest+fires/forestfires.csv")

#preview dataset 
head(forest_fires)

if (interactive()) View(forest_fires)