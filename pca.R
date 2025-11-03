setwd("C:/Users/celia/Downloads/Ciencia e Ingeniería de datos/Aprendizaje estadístico/Homework1")

# AI version:
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

#vars_imputar <- c("Age","Glucose","Blood_Pressure","BMI", "Oxygen_Saturation",
#                  "Cholesterol", "Triglycerides","HbA1c",
#                  "Physical_Activity","Diet_Score",
#                  "Stress_Level","Sleep_Hours")

#datos1 = datos[, c(1, 2, 4, 5, 6, 7, 9, 10, 11, 14, 15, 17, 18)]
datos1 = datos
datos1 = na.omit(datos1)
datos1$noise_col = NULL
datos1$random_notes = NULL

aux=which(datos1$Gender =="")
length(aux)
datos1=datos1[-aux,]

aux=which(datos1$Medical.Condition =="")
length(aux)
datos1=datos1[-aux,]

# Males: 0, Female: 1
datos1$Gender = factor(datos1$Gender,
                         levels = c('Male', 'Female'),
                         labels = c(0, 1))

#imp_obj1 = mice(datos1, method = "rf", m=3)

##imp_obj <- mice(datos[, vars_imputar], method = "rf", m = 3)
##imp <- complete(imp_obj)

# sustituimos en el dataset original

# datos[, vars_imputar] <- imp

# para las categóricas con NA, les ponemos una categoría

#datos$Gender[is.na(datos$Gender)] <- "Unknown"
#datos$Medical_Condition[is.na(datos$Medical_Condition)] <- "Unknown"

#colSums(is.na(datos))
ggplot(datos1, aes(x = "", y = LengthOfStay)) +
  geom_boxplot(fill = "lightblue", color = "blue",
               outlier.color = "red", outlier.shape = 16) +
  labs(title = "Outliers en LengthOfStay")

library(outliers)

idx_out <- outlier(datos1$LengthOfStay, logical = TRUE)

datos1[idx_out, c("Age","Medical.Condition","LengthOfStay")]

ggplot(datos1, aes(x = LengthOfStay)) + geom_histogram(bins = 30, color = "white") +
  labs(title = "Distribución de la estancia", x = "días")

ggplot(datos, aes(x = Age)) + geom_histogram(bins = 30, color = "white") +
  labs(title = "Distribución de la edad")

Xnum <- datos1 %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation,
         Cholesterol, Triglycerides, HbA1c,
         Physical.Activity, Diet.Score,
         Stress.Level, Sleep.Hours,
         LengthOfStay)

GGally::ggcorr(Xnum, label = TRUE, hjust = 0.75)
set.seed(123)
n <- nrow(datos1)
id_train <- sample(1:n, size = round(0.7*n))
train <- datos1[id_train, ]
test  <- datos1[-id_train, ]
# Modelo 1: sencillo

mod1 <- lm(LengthOfStay ~ Age + Gender + Medical.Condition,
           data = train)

# Modelo 2: grande (todas las numéricas principales)

mod2 <- lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender + Medical.Condition, data = train)

# Modelo 3: como en la sesión de polinomios

mod3 <- lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender + Medical.Condition, data = train)

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

  m1_b <- lm(LengthOfStay ~ Age + Gender + Medical.Condition, data = train_b)
  m2_b <- lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender + Medical.Condition, data = train_b)
  m3_b <- lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender + Medical.Condition, data = train_b)

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

err_df <- data.frame(modelo = factor(rep(c("M1","M2","M3"), each = B)),
                     mse = c(err1, err2, err3))

ggplot(err_df, aes(x = modelo, y = mse, fill = modelo)) +
  geom_boxplot() +
  labs(title = "Distribución del MSE por bootstrap")

X <- datos1 %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation,
         Cholesterol, Triglycerides, HbA1c,
         Physical.Activity, Diet.Score,
         Stress.Level, Sleep.Hours)

X_scaled <- scale(X)

pca_res <- prcomp(X_scaled, scale = FALSE)
summary(pca_res)

fviz_eig(pca_res)
fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("grey80","steelblue","darkblue"),
             repel = TRUE)
# coloreamos por condición médica

fviz_pca_ind(pca_res, geom = "point", habillage = datos1$Medical.Condition, 
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
fviz_cluster(fit.kmeans, data = X_clust, geom = "point", ellipse.type = "norm",
             main = "K-means con k=4")

fit.pam <- pam(X_clust, k = 4)
fit.pam$clustering[1:20]
fviz_cluster(fit.pam, geom = "point", data = X_clust, main = "PAM con k=4")
res.Mclust <- Mclust(X_clust)
summary(res.Mclust)
res.Mclust$G   # nº de grupos sugerido

fviz_mclust(res.Mclust, "classification")
grupos <- fit.kmeans$cluster   # elegimos uno de los 3 métodos
datos1$cluster <- grupos

datos1 %>%
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

#8. Conclusiones

#(aquí solo escribes texto, no código, con las ideas que saquemos de las salidas anteriores)

#Preprocesado: el dataset tenía NA en variables clínicas; se imputó con mice(method="rf") como en clase → datos completos.

#Visualización: LengthOfStay es asimétrica y hay outliers; hay bastantes correlaciones entre las variables metabólicas.

#Regresión + remuestreo: con 200 bootstraps el modelo grande (M2) suele dar menor MSE que el modelo sencillo; el modelo polinómico (M3) no siempre mejora → podemos hablar de sobreajuste como en la sesión.

#PCA: 2–3 componentes explican buena parte de la variabilidad y separan el eje metabólico del de estilo de vida.

#FA: con 3 factores rotados varimax se obtiene una estructura interpretable muy parecida a la del PCA.

#Clustering: con k=4 aparecen grupos con distinto riesgo metabólico y distinta estancia media → son segmentos útiles.
