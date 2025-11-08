---
title: "Homework 1"
autor: "Malen Abarrategui Meire and Celia Benavente Fernández de Velasco"
date: "November 2025"
output:
  html_document: 
    css: my-theme.css
    theme: cerulean
    highlight: tango
    number_sections: no
    toc: no
    toc_depth: 1
  pdf_document:
    css: my-theme.css
    theme: cerulean
    highlight: tango
    number_sections: yes
    toc: yes
    toc_depth: 1
editor_options:
  chunk_output_type: console
---

```{r global_options, include=T, echo = F}
knitr::opts_chunk$set(echo = T, warning=FALSE, message=FALSE)
```

# Introduction

This proyect focuses an analyzing populations healthcare risks factors using various data analysis techniques. Our data is called "dirty_v3_path.csv". It contains 20 variables and 30000 observations.

Among the 20 variables we can find:
Age: Patient’s age (in years).
Gender: Male / Female.
Medical Condition: Reported health condition (e.g., Diabetes, Hypertension, Asthma, Obesity, Healthy).
Glucose: Blood glucose level.
Blood Pressure: Blood pressure measurement.
BMI: Body Mass Index.
Oxygen Saturation: Blood oxygen saturation level.
LengthOfStay: Hospital length of stay (days).
Cholesterol: Cholesterol level.
Triglycerides: Triglyceride level.
HbA1c: Hemoglobin A1c (glycated hemoglobin).
Smoking: Smoking status (0 = Non-smoker, 1 = Smoker).
Alcohol: Alcohol consumption (0 = No, 1 = Yes).
Physical Activity: Physical activity (approx. hours/week).
Diet Score: Diet quality score (numeric).
Family History: Family medical history (0 = No, 1 = Yes).
Stress Level: Stress level (numeric scale).
Sleep Hours: Average sleep hours per day.
random_notes: Random notes (lorem / ipsum / ###).
noise_col: Noise column (unrelated random values).

# Preprocessing

Preprocessing ensures that the data is clean, complete, and ready for analysis. It allows detecting errors, NAs, and outliers that can distort results. Feature engineering steps are also included (for example, converting gender into a numerical variable) so that models can use this information correctly.

```{r}
# Insert your working directory
# setwd()

# Used libraries
library(tidyverse)
library(stringr)
library(mice)
library(outliers)
library(GGally)
library(factoextra)
library(cluster)
library(mclust)
library(boot)
library(bootstrap)
library(tidyr)
# Implement the database
data = read.csv("dirty_v3_path.csv", stringsAsFactors = FALSE)

str(data)
summary(data)
colSums(is.na(data))

# Fixed combination of data to get always the same result
set.seed(123)

# Remove NA values
data = na.omit(data)

# Remove unnecessary variable
data$noise_col = NULL
data$random_notes = NULL

# Deal with empty values for given variables
aux = which(data$Gender == "")
length(aux)
data = data[-aux,]

aux = which(data$Medical.Condition == "")
length(aux)
data = data[-aux,]

# Males as 0 and Females as 1
data$Gender = factor(data$Gender, levels = c('Male', 'Female'), labels = c(0, 1))
```

# Visualization tools

Exploratory visualization allows understanding patterns, distributions, and relationships between variables before applying models. For example, the boxplot shows extreme values (patients with long hospital stays), while the correlation matrix shows which variables are associated (e.g., glucose and triglycerides).

```{r}
## Getting the outliers for the LengthOfStay variable as it is a good indicator 
# of health. Less time in a hospital -> better health. The outliers of this 
# variable are the people whose health is not the best
# Therefore, we conclude it is enough to check the LengthofStay to know if a
# person is healthy
ggplot(data, aes(x = "", y = LengthOfStay)) +
geom_boxplot(fill = "lightblue", color = "blue", outlier.color = "red", 
           outlier.shape = 16) + labs(title = "Outliers en LengthOfStay")

## Index of the outliers in LengthOfStay
idx_out = outlier(data$LengthOfStay, logical = TRUE)

## Comparative between the Age, Medical.Condition and LenghtOfStay 
### People who spent 19 days in the Hospital are very likely to have Cancer
data[idx_out, c("Age","Medical.Condition","LengthOfStay")]

ggplot(data, aes(x = LengthOfStay)) + geom_histogram(bins = 30, color = "white", 
                                                 fill = "lightblue") +
labs(title = "Stay distribution", x = "Days")

ggplot(data, aes(x = Age)) + geom_histogram(bins = 30, color = "white", 
                                        fill = "lightblue") +
labs(title = "Age distribution")

#Bivariate plot:Comparative between stress level and cholesterol:people having hypertension and cancer
#are likely to suffer from this,which is interesting to our target variable.
ggplot(data=data,aes(x=Stress.Level,y=Cholesterol,color=Medical.Condition))+geom_point(alpha = 0.7, size = 2.0)+
  theme_minimal(base_size = 10)+
  labs(
    title = "Relationship between Stress Level and Cholesterol",
    subtitle = "Colored by Medical Condition",
    x = "Stress Level",
    y = "Cholesterol (mg/dL)")+
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 12))

# Tema de estilo limpio

# We choose some numeric variables which we consider are related with the 
# Medical.Condition
Xnum = data %>%
select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
     Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
     Sleep.Hours, LengthOfStay)

# Correlation between numerical variables previously selected
GGally::ggcorr(Xnum, label = TRUE, hjust = 0.75)
```

# Regression models and resampling

Regression models are used to predict a numerical variable (in this case, LengthOfStay) based on others.
Resampling (bootstrap) evaluates the robustness and variability of the models, helping to identify which one generalizes better. 
In this case, model 2 has the lowest MSE → best fit without overfitting.


```{r}
# Get a training set from all the data (70%)
n = nrow(data)
id_train = sample(1:n, size = round(0.7*n))
train = data[id_train, ]
# Get the test set (data which is not in the training set)
test  = data[-id_train, ]

# To train the models we used the train set 
## Model 1: the simplest 
mod1 = lm(LengthOfStay ~ Age + Gender, data = train)

## Model 2: takes into account the most important numeric variables
mod2 = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
        Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
        Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
        Gender, data = train)

## Model 3: polynomial model
mod3 = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
        Glucose + Blood.Pressure +
        Gender, data = train)

summary(mod1)
summary(mod2)
summary(mod3)

# Predictions for each of the models using the test set
pred1 = predict(mod1, newdata = test)
pred2 = predict(mod2, newdata = test)
pred3 = predict(mod3, newdata = test)

mse1 = mean( (test$LengthOfStay - pred1)^2 )
mse2 = mean( (test$LengthOfStay - pred2)^2 )
mse3 = mean( (test$LengthOfStay - pred3)^2 )

mse1; mse2; mse3

# k means
B = 200
err1 = rep(0, B)
err2 = rep(0, B)
err3 = rep(0, B)

for (b in 1:B) {

# Train the model with a different train set each iteration
idx_b = sample(1:nrow(train), replace = TRUE)
train_b = train[idx_b, ]

# We use the same models as before
m1_b = lm(LengthOfStay ~ Age + Gender, data = train_b)
m2_b = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
          Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
          Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
          Gender, data = train_b)
m3_b = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
          Glucose + Blood.Pressure +
          Gender, data = train_b)

# Prediction on the test set, which is also different each time
p1_b = predict(m1_b, newdata = test)
p2_b = predict(m2_b, newdata = test)
p3_b = predict(m3_b, newdata = test)

err1[b] = mean( (test$LengthOfStay - p1_b)^2 )
err2[b] = mean( (test$LengthOfStay - p2_b)^2 )
err3[b] = mean( (test$LengthOfStay - p3_b)^2 )
}

mean(err1); sd(err1)
mean(err2); sd(err2)
mean(err3); sd(err3)

err_df = data.frame(modelo = factor(rep(c("Model 1","Model 2","Model3"), 
                                    each = B)),
                mse = c(err1, err2, err3))

```

# Principal Component Analysis

PCA reduces the number of variables by creating linear combinations (components) 
that capture most of the variance. Here we use mostly metabolic and lifestyle variables.

```{r}
# We choose the almost the same variables as before, this time we are not using 
# the LengthOfStay variable
X = data %>%
select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
     Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
     Sleep.Hours)

# We scale the data so that all variables contribute equally to PCA.
X_scaled = scale(X)

pca_res = prcomp(X_scaled, scale = FALSE)
summary(pca_res)
loadings_res = pca_res$rotation[, 1]
contributions_res = loadings_res^2

loadings_res = pca_res$rotation[, 2]
contributions_res = loadings_res^2
fviz_contrib(pca_res, choice = "var", axes = 1)

fviz_eig(pca_res, addlabels = TRUE)
fviz_pca_var(pca_res, col.var = "contrib", 
         gradient.cols = c("grey80","steelblue","darkblue"), repel = TRUE)

# Color based on Medical.Condition
fviz_pca_ind(pca_res, geom = "point", habillage = data$Medical.Condition, 
         addEllipses = TRUE)
```

# Factor Analysis

Factor Analysis tries to find latent variables that cause the observed correlations.
It is similar to PCA but more model-based.
We try both 3 and 2 factors and use varimax rotation to make loadings easier to interpret.

```{r}
# We try two different number of factors
## 3 factors
fa3 = factanal(X, factors = 3, scores = "Bartlett", rotation = "varimax")
fa3
loadings(fa3)

# 2 factors
fa2 = factanal(X, factors = 2, scores = "Bartlett", rotation = "varimax")
fa2
loadings(fa2)
```

# Clustering tools

Clustering groups individuals without using prior labels, based on similarities between variables. 
It allows the discovery of patient profiles (for example, groups with higher metabolic risk or shorter hospital stays).
Each method (k-means, PAM, Mclust) offers a different perspective on the structure of the data.


```{r}

# We take the same numeric variables as before
#Choose number of clusters by WSS and silhouette
fviz_nbclust(X_scaled, kmeans, method = "wss", k.max = 10)
fviz_nbclust(X_scaled, kmeans, method = "silhouette", k.max = 10)
fit.kmeans = kmeans(X_scaled, centers = 4, nstart = 25)

fit.kmeans$size
fit.kmeans$centers[1:4,]
fviz_cluster(fit.kmeans, data = X_scaled, geom = "point", ellipse.type = "norm",
         main = "K-means for k = 4")
centers=fit.kmeans$centers
tidy = cbind(
gather(as.data.frame(t(centers)), "cluster", "coor"),
var=rep(colnames(centers, k)),
size=rep(table(fit.kmeans$cluster), each=ncol(centers))
)
tidy %>%
ggplot(aes(x=cluster, y=coor, fill=cluster)) +
geom_col() +
facet_wrap(~var) +
geom_text(aes(label=size),
        position=position_stack(1.2))

fit.pam = pam(X_scaled, k = 4)
fit.pam$clustering[1:20]
fviz_cluster(fit.pam, geom = "point", data = X_scaled, main = "PAM for k = 4")
#Model-based clustering: Mclust picks the optimal number of clusters
res.Mclust = Mclust(X_scaled)
summary(res.Mclust)
## Suggested number of groups
res.Mclust$G

fviz_mclust(res.Mclust, "classification")
# Choose one of the 3 tried methods
group = fit.kmeans$cluster
#We attach chosen cluster labels (here: k-means) back to original data
data$cluster = group

# Now we use the mean values of each variable(summarize clinical profiles by cluster)
data %>%
group_by(cluster) %>%
summarise(
n = n(),
Age = mean(Age),
BMI = mean(BMI),
Glucose = mean(Glucose),
Blood.Pressure = mean(Blood.Pressure),
Cholesterol = mean(Cholesterol),
Triglycerides = mean(Triglycerides),
LOS = mean(LengthOfStay)
)

```

# Conclusion

Preprocesado: el dataset tenía NA en variables clínicas; se imputó con mice(method="rf") como en clase → datos completos.

Visualización: LengthOfStay es asimétrica y hay outliers; hay bastantes correlaciones entre las variables metabólicas.

Regresión + remuestreo: con 200 bootstraps el modelo grande (M2) suele dar menor MSE que el modelo sencillo; el modelo polinómico (M3) no siempre mejora → podemos hablar de sobreajuste como en la sesión.

PCA: 2–3 componentes explican buena parte de la variabilidad y separan el eje metabólico del de estilo de vida.

FA: con 3 factores rotados varimax se obtiene una estructura interpretable muy parecida a la del PCA.

Clustering: con k=4 aparecen grupos con distinto riesgo metabólico y distinta estancia media → son segmentos útiles.
