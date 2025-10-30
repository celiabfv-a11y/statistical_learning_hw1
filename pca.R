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
# library(caret)
library(countrycode)
library(rworldmap)
library(GGally)
library(factoextra)
library(cluster)
library(mclust)
library(kernlab)

rm(list=ls())
set.seed(123)
getwd()

setwd("C:/Users/celia/Downloads/Ciencia e Ingeniería de datos/Aprendizaje estadístico/Homework1")

data = read.csv("dirty_v3_path.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

any(is.na(data))
is.na(data)

for(i in 1:ncol(data)){
  if(any(is.na(data[[i]]))){
    cat("Variable", names(data)[i], "has NA values\n")
  } else {
    cat("Variable", names(data)[i], "does not have NA values\n")
  }
}

###missing_summary = data %%
#  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %%
#  pivot_longer(cols = everything(), names_to = "Variable", values_to = "PercentageNA")
#print(missing_summary)
#ggplot(missing_summary, aes(x = reorder(Variable, -PercentageNA), y = PercentageNA)) +
#  geom_bar(stat = "identity", fill = "skyblue") +
#  coord_flip() +
#  labs(title = "Percentage of Missing values",
#       x = "Variable",
#       y = "% NA") +
#  theme_minimal()

data$random_notes = NULL
data$noise_col = NULL

#Create a data frame
df2 = data
# Compute the skewness of Variable1 (repeat this process with all numeric variables with NA values)
skewness_Var1 = skewness(df2$Variable1, na.rm = TRUE)
print(skewness_Var1) #over 1 is very skewed to the right, negative means skewed to left.
# Close to 0 is not skewed (this is the best case, we do not want our data to be skewed)
hist(df2$Variable1, main = "Histogram of Var1", xlab = "Valor", col = "skyblue", breaks = 30)
plot(density(df2$Variable1, na.rm = TRUE), main = "Var1 Density", col = "blue")

# Select only numeric variables with missing values
df_skewed = df2[, c("Var1”, “Var2”)]
# Impute missing values using knn on skewed variables
df_skewed_imputed = kNN(df_skewed, k = 5)
summary(df_skewed_imputed)
#Check if there are any NA
missing_values_imputed = sapply(df_skewed_imputed, function(x) sum(is.na(x)))
print(missing_values_imputed)
#Let's change the original variables for the ones without NA that we had computed using knn
data$Var1 = df_skewed_imputed$Var1
data$Var2 = df_skewed_imputed$Var2

