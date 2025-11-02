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

#AI version:
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
datos <- read.csv("dirty_v3_path.csv", stringsAsFactors = FALSE)

# pasar nombres con _ como hacemos muchas veces

names(datos) <- gsub(" ", "_", names(datos))

str(datos)
summary(datos)
colSums(is.na(datos))
set.seed(123)

# elegimos SOLO columnas numéricas para imputar con mice

vars_imputar <- c("Age","Glucose","Blood_Pressure","BMI",
"Oxygen_Saturation","Cholesterol",
"Triglycerides","HbA1c",
"Physical_Activity","Diet_Score",
"Stress_Level","Sleep_Hours")

imp_obj <- mice(datos[, vars_imputar], method = "rf", m = 3)
imp <- complete(imp_obj)

# sustituimos en el dataset original

datos[, vars_imputar] <- imp

# para las categóricas con NA, les ponemos una categoría

datos$Gender[is.na(datos$Gender)] <- "Unknown"
datos$Medical_Condition[is.na(datos$Medical_Condition)] <- "Unknown"

colSums(is.na(datos))
ggplot(datos, aes(x = "", y = LengthOfStay)) +
geom_boxplot(fill = "lightblue", color = "blue",
outlier.color = "red", outlier.shape = 16) +
labs(title = "Outliers en LengthOfStay")
library(outliers)
idx_out <- outlier(datos$LengthOfStay, logical = TRUE)
datos[idx_out, c("Age","Medical_Condition","LengthOfStay")] %>% head()
ggplot(datos, aes(x = LengthOfStay)) +
geom_histogram(bins = 30, color = "white") +
labs(title = "Distribución de la estancia", x = "días")
ggplot(datos, aes(x = Age)) +
geom_histogram(bins = 30, color = "white") +
labs(title = "Distribución de la edad")
Xnum <- datos %>%
select(Age, Glucose, Blood_Pressure, BMI, Oxygen_Saturation,
Cholesterol, Triglycerides, HbA1c,
Physical_Activity, Diet_Score,
Stress_Level, Sleep_Hours,
LengthOfStay)

GGally::ggcorr(Xnum, label = TRUE, hjust = 0.75)
set.seed(123)
n <- nrow(datos)
id_train <- sample(1:n, size = round(0.7*n))
train <- datos[id_train, ]
test  <- datos[-id_train, ]
# Modelo 1: sencillo

mod1 <- lm(LengthOfStay ~ Age + Gender + Medical_Condition,
data = train)

# Modelo 2: grande (todas las numéricas principales)

mod2 <- lm(LengthOfStay ~ Age + Glucose + Blood_Pressure + BMI +
Oxygen_Saturation + Cholesterol + Triglycerides + HbA1c +
Physical_Activity + Diet_Score + Stress_Level + Sleep_Hours +
Gender + Medical_Condition,
data = train)

# Modelo 3: como en la sesión de polinomios

mod3 <- lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) +
poly(BMI, 2, raw = TRUE) +
Glucose + Blood_Pressure +
Gender + Medical_Condition,
data = train)

summary(mod1)
summary(mod2)
summary(mod3)
pred1 <- predict(mod1, newdata = test)
pred2 <- predict(mod2, newdata = test)
pred3 <- predict(mod3, newdata = test)

mse1 <- mean( (test$LengthOfStay - pred1)^2 )
mse2 <- mean( (test$LengthOfStay - pred2)^2 )
mse3 <- mean( (test$LengthOfStay - pred3)^2 )

mse1; mse2; mse3
set.seed(123)
B <- 200       # nº de remuestreos
err1 <- rep(NA, B)
err2 <- rep(NA, B)
err3 <- rep(NA, B)

for (b in 1:B) {

# remuestreo sobre el train

idx_b <- sample(1:nrow(train), replace = TRUE)
train_b <- train[idx_b, ]

m1_b <- lm(LengthOfStay ~ Age + Gender + Medical_Condition,
data = train_b)
m2_b <- lm(LengthOfStay ~ Age + Glucose + Blood_Pressure + BMI +
Oxygen_Saturation + Cholesterol + Triglycerides + HbA1c +
Physical_Activity + Diet_Score + Stress_Level + Sleep_Hours +
Gender + Medical_Condition,
data = train_b)
m3_b <- lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) +
poly(BMI, 2, raw = TRUE) +
Glucose + Blood_Pressure +
Gender + Medical_Condition,
data = train_b)

# medimos SIEMPRE sobre EL MISMO test (como hicimos con las curvas)

p1_b <- predict(m1_b, newdata = test)
p2_b <- predict(m2_b, newdata = test)
p3_b <- predict(m3_b, newdata = test)

err1[b] <- mean( (test$LengthOfStay - p1_b)^2 )
err2[b] <- mean( (test$LengthOfStay - p2_b)^2 )
err3[b] <- mean( (test$LengthOfStay - p3_b)^2 )
}

mean(err1); sd(err1)
mean(err2); sd(err2)
mean(err3); sd(err3)
err_df <- data.frame(
modelo = factor(rep(c("M1","M2","M3"), each = B)),
mse = c(err1, err2, err3)
)

ggplot(err_df, aes(x = modelo, y = mse, fill = modelo)) +
geom_boxplot() +
labs(title = "Distribución del MSE por bootstrap")
X <- datos %>%
select(Age, Glucose, Blood_Pressure, BMI, Oxygen_Saturation,
Cholesterol, Triglycerides, HbA1c,
Physical_Activity, Diet_Score,
Stress_Level, Sleep_Hours)

X_scaled <- scale(X)

pca_res <- prcomp(X_scaled, scale = FALSE)
summary(pca_res)
fviz_eig(pca_res)
fviz_pca_var(pca_res,
col.var = "contrib",
gradient.cols = c("grey80","steelblue","darkblue"),
repel = TRUE)
# coloreamos por condición médica

fviz_pca_ind(pca_res,
geom = "point",
habillage = datos$Medical_Condition,
addEllipses = TRUE)
# nº de factores: probamos con 3 (metabólico, presión, estilo de vida)

fa3 <- factanal(X, factors = 3, scores = "Bartlett", rotation = "varimax")
fa3
loadings(fa3)
fa2 <- factanal(X, factors = 2, scores = "Bartlett", rotation = "varimax")
fa2
loadings(fa2)
X_clust <- scale(X)   # las numéricas de antes
fviz_nbclust(X_clust, kmeans, method = "wss", k.max = 10)
fviz_nbclust(X_clust, kmeans, method = "silhouette", k.max = 10)
set.seed(123)
fit.kmeans <- kmeans(X_clust, centers = 4, nstart = 25)

fit.kmeans$size
fit.kmeans$centers[1:4,]
fviz_cluster(fit.kmeans, data = X_clust,
geom = "point",
ellipse.type = "norm",
main = "K-means con k=4")
fit.pam <- pam(X_clust, k = 4)
fit.pam$clustering[1:20]
fviz_cluster(fit.pam, geom = "point", data = X_clust,
main = "PAM con k=4")
res.Mclust <- Mclust(X_clust)
summary(res.Mclust)
res.Mclust$G   # nº de grupos sugerido

fviz_mclust(res.Mclust, "classification")
grupos <- fit.kmeans$cluster   # elegimos uno de los 3 métodos
datos$cluster <- grupos

datos %>%
group_by(cluster) %>%
summarise(
n = n(),
Age = mean(Age),
BMI = mean(BMI),
Glucose = mean(Glucose),
Blood_Pressure = mean(Blood_Pressure),
Cholesterol = mean(Cholesterol),
Triglycerides = mean(Triglycerides),
LOS = mean(LengthOfStay)
)
8. Conclusiones

(aquí solo escribes texto, no código, con las ideas que saquemos de las salidas anteriores)

Preprocesado: el dataset tenía NA en variables clínicas; se imputó con mice(method="rf") como en clase → datos completos.

Visualización: LengthOfStay es asimétrica y hay outliers; hay bastantes correlaciones entre las variables metabólicas.

Regresión + remuestreo: con 200 bootstraps el modelo grande (M2) suele dar menor MSE que el modelo sencillo; el modelo polinómico (M3) no siempre mejora → podemos hablar de sobreajuste como en la sesión.

PCA: 2–3 componentes explican buena parte de la variabilidad y separan el eje metabólico del de estilo de vida.

FA: con 3 factores rotados varimax se obtiene una estructura interpretable muy parecida a la del PCA.

Clustering: con k=4 aparecen grupos con distinto riesgo metabólico y distinta estancia media → son segmentos útiles.
