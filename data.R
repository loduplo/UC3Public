## data.R ##
library("dplyr")
library("lubridate")

# DONNEES de consommation et production de Provence Alpes Agglomeration
territoire <- "Provence Alpes Agglomeration"
territoireCourt <- "PAA"
mesData <- read.csv2("PAAdata.csv",encoding = "UTF-8")