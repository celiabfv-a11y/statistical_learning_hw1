library(tidyverse)
library(tidyr)
library(dplyr)
library(leaflet)
library(mapboxapi)
library(sf)
library(readxl)
#library(rgdal)
library(stringr)
library(ggplot2)
library(reshape2)
library(igraph)
library(caret)
library(countrycode)
library(rworldmap)
library(GGally)
library(factoextra)
library(cluster)
library(mclust)
library(kernlab)

rm(list=ls()) # for cleaning the working space
set.seed(123)
getwd() #to get your working directory

setwd("C:/Users/celia/Downloads/Ciencia e Ingeniería de datos/Aprendizaje estadístico/Homework1")

data = read.csv("winequality-red.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

any(is.na(data))

df2 = data

skewness_Var1 = skewness(df2$Variable1, na.rm = TRUE)
print(skewness_Var1)
