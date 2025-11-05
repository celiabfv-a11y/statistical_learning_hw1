setwd("C:/Users/celia/Downloads/Ciencia e Ingeniería de datos/Aprendizaje estadístico/Homework1")

# Used libraries
library(outliers)
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

# Implement the database
data = read.csv("dirty_v3_path.csv", stringsAsFactors = FALSE)

# pasar nombres con _ como hacemos muchas veces
# names(data) <- gsub(" ", "_", names(data))

# Preprocessing
## See how our data is distributed
str(data)
summary(data)
colSums(is.na(data))

## Fixed combination of data to get always the same result
#set.seed(123)

# elegimos SOLO columnas numéricas para imputar con mice
#vars_imputar <- c("Age","Glucose","Blood_Pressure","BMI", "Oxygen_Saturation",
#                  "Cholesterol", "Triglycerides","HbA1c",
#                  "Physical_Activity","Diet_Score",
#                  "Stress_Level","Sleep_Hours")
#datos1 = datos[, c(1, 2, 4, 5, 6, 7, 9, 10, 11, 14, 15, 17, 18)]

## Cleaning the data
data = na.omit(data)
### Get rid of unnecessary variables 
data$noise_col = NULL
data$random_notes = NULL

### Get rid of empty inputs 
aux = which(data$Gender == "")
length(aux)
data = data[-aux,]

aux = which(data$Medical.Condition == "")
length(aux)
data = data[-aux,]

### Refactor: convert string to factors
#### Males as 0 and Females as 1
data$Gender = factor(data$Gender, levels = c('Male', 'Female'), labels = c(0, 1))

#imp_obj1 = mice(datos1, method = "rf", m=3)
##imp_obj <- mice(datos[, vars_imputar], method = "rf", m = 3)
##imp <- complete(imp_obj)
# sustituimos en el dataset original
# datos[, vars_imputar] <- imp
# para las categóricas con NA, les ponemos una categoría
#datos$Gender[is.na(datos$Gender)] <- "Unknown"
#datos$Medical_Condition[is.na(datos$Medical_Condition)] <- "Unknown"
#colSums(is.na(datos))

## Getting the outliers for the LengthOfStay variable as it is a good indicator 
# of health. Less time in a hospital -> better health. The outliers of this 
# variable are the people whose health is not the best
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


# We choose some numeric variables which we consider are related with the 
# Medical.Condition
Xnum = data %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
         Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
         Sleep.Hours, LengthOfStay)

GGally::ggcorr(Xnum, label = TRUE, hjust = 0.75)

# Get a training set from all the data (70%)
n = nrow(data)
id_train = sample(1:n, size = round(0.7*n))
train = data[id_train, ]
# Get the test set (data which is not in the training set)
test  = data[-id_train, ]

# To train the models we used the train set 
## Model 1: the simplest 
mod1 = lm(LengthOfStay ~ Age + Gender + Medical.Condition, data = train)

## Model 2: takes into account the most important numeric variables
mod2 = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender + Medical.Condition, data = train)

## Model 3: polynomial model
mod3 = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender + Medical.Condition, data = train)

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
  m1_b = lm(LengthOfStay ~ Age + Gender + Medical.Condition, data = train_b)
  m2_b = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender + Medical.Condition, data = train_b)
  m3_b = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender + Medical.Condition, data = train_b)

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

# Representation of all MSE as a boxplot
ggplot(err_df, aes(x = modelo, y = mse, fill = modelo)) + geom_boxplot() +
  labs(title = "MSE distribution by bootstrap")

# We choose the almost the same variables as before, this time we are not using 
# the LengthOfStay variable
X = data %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
         Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
         Sleep.Hours)

# We scale the data
X_scaled = scale(X)

pca_res = prcomp(X_scaled, scale = FALSE)
summary(pca_res)

fviz_eig(pca_res)
fviz_pca_var(pca_res, col.var = "contrib", 
             gradient.cols = c("grey80","steelblue","darkblue"), repel = TRUE)

# Color based on Medical.Condition
fviz_pca_ind(pca_res, geom = "point", habillage = data$Medical.Condition, 
             addEllipses = TRUE)

# We try two different number of factors
## 3 factors
fa3 = factanal(X, factors = 3, scores = "Bartlett", rotation = "varimax")
fa3
loadings(fa3)

# 2 factors
fa2 = factanal(X, factors = 2, scores = "Bartlett", rotation = "varimax")
fa2
loadings(fa2)

# We take the same numeric variables as before
fviz_nbclust(X_scaled, kmeans, method = "wss", k.max = 10)
fviz_nbclust(X_scaled, kmeans, method = "silhouette", k.max = 10)
fit.kmeans = kmeans(X_scaled, centers = 4, nstart = 25)

fit.kmeans$size
fit.kmeans$centers[1:4,]
fviz_cluster(fit.kmeans, data = X_scaled, geom = "point", ellipse.type = "norm",
             main = "K-means for k = 4")

fit.pam = pam(X_scaled, k = 4)
fit.pam$clustering[1:20]
fviz_cluster(fit.pam, geom = "point", data = X_scaled, main = "PAM for k = 4")
res.Mclust = Mclust(X_scaled)
summary(res.Mclust)
# Suggested number of groups
res.Mclust$G

fviz_mclust(res.Mclust, "classification")
# Choose one of the 3 tried methods
group = fit.kmeans$cluster
data$cluster = group

# Now we use the mean values of each variable 
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

# Conclusions
# Preprocesado: el dataset tenía NA en variables clínicas; se imputó con mice(method="rf") como en clase → datos completos.
#Visualización: LengthOfStay es asimétrica y hay outliers; hay bastantes correlaciones entre las variables metabólicas.
#Regresión + remuestreo: con 200 bootstraps el modelo grande (M2) suele dar menor MSE que el modelo sencillo; el modelo polinómico (M3) no siempre mejora → podemos hablar de sobreajuste como en la sesión.
#PCA: 2–3 componentes explican buena parte de la variabilidad y separan el eje metabólico del de estilo de vida.
#FA: con 3 factores rotados varimax se obtiene una estructura interpretable muy parecida a la del PCA.
#Clustering: con k=4 aparecen grupos con distinto riesgo metabólico y distinta estancia media → son segmentos útiles.
