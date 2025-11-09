setwd("C:/Users/celia/Downloads/Ciencia e Ingeniería de datos/Aprendizaje estadístico/Homework1")

# Load required libraries
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

# Load the dataset
data = read.csv("dirty_v3_path.csv", stringsAsFactors = FALSE)


# Preprocessing ----------------------------------------------------------------
## Inspect the structure and summarize the data
str(data)
summary(data)
colSums(is.na(data)) ## Number of NAs per column

## Fix the random seed to ensure reproducibility
set.seed(123)

## Remove rows with NA values
data = na.omit(data)

## Remove unnecessary variables 
data$noise_col = NULL
data$random_notes = NULL

## Remove empty rows in categorical variables
aux = which(data$Gender == "")
data = data[-aux,]
aux = which(data$Medical.Condition == "")
data = data[-aux,]

## Convert categorical variables to factors (Male = 0, Female = 1)
data$Gender = factor(data$Gender, levels = c('Male', 'Female'), labels = c(0, 1))


# Outlier analysis -------------------------------------------------------------
## Identify outliers for the LengthOfStay variable, as it is a good health 
## indicator.
## Less time in the hospital → better health. Outliers represent patients with 
## worse health.
ggplot(data, aes(x = "", y = LengthOfStay)) +
  geom_boxplot(fill = "lightblue", color = "blue", outlier.color = "red", 
               outlier.shape = 16) + labs(title = "Outliers en LengthOfStay")

## Based on these results, we decide to use the LengthOfStay variable as the target.

## Index of the outliers
idx_out = outlier(data$LengthOfStay, logical = TRUE)

## Check atypical cases
data[idx_out, c("Age","Medical.Condition","LengthOfStay")]

## Descriptive histograms
ggplot(data, aes(x = LengthOfStay)) + 
  geom_histogram(bins = 30, color = "white", fill = "lightblue") +
  labs(title = "Stay distribution", x = "Days")

ggplot(data, aes(x = Age)) + 
  geom_histogram(bins = 30, color = "white", fill = "lightblue") +
  labs(title = "Age distribution")

# Bivariate plot: Comparative between stress level and cholesterol:people having 
# hypertension and cancer are likely to suffer from this, which is interesting 
# to our target variable.
ggplot(data = data, aes(x = Stress.Level, y = Cholesterol, color = Medical.Condition)) + 
  geom_point(alpha = 0.7, size = 2.0) +
  theme_minimal(base_size = 10) +
  labs(title = "Relationship between Stress Level and Cholesterol",
       subtitle = "Colored by Medical Condition",
       x = "Stress Level",
       y = "Cholesterol (mg/dL)")+
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30", size = 12))


# Correlation matrix -----------------------------------------------------------
## Select numeric variables that may be related to LengthOfStay
Xnum = data %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
         Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
         Sleep.Hours, LengthOfStay)

GGally::ggcorr(Xnum, label = TRUE, hjust = 0.75)


# Train/test split -------------------------------------------------------------
n = nrow(data)
id_train = sample(1:n, size = round(0.7*n))
train = data[id_train, ]
test  = data[-id_train, ]


# Regression models ------------------------------------------------------------
## Model 1: the simplest model 
mod1 = lm(LengthOfStay ~ Age + Gender, data = train)

## Model 2: includes the most relevant numeric variables
mod2 = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender, data = train)

## Model 3: polynomial model
mod3 = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender, data = train)

## Compare model results
summary(mod1)
summary(mod2)
summary(mod3)

## Predictions and mean squared error (MSE)
pred1 = predict(mod1, newdata = test)
pred2 = predict(mod2, newdata = test)
pred3 = predict(mod3, newdata = test)

mse1 = mean( (test$LengthOfStay - pred1)^2 )
mse2 = mean( (test$LengthOfStay - pred2)^2 )
mse3 = mean( (test$LengthOfStay - pred3)^2 )

mse1; mse2; mse3 ## Comparison of mean squared errors


# Bootstrap to estimate MSE variability across samples -------------------------
B = 200
err1 = rep(0, B)
err2 = rep(0, B)
err3 = rep(0, B)

for (b in 1:B) {
  idx_b = sample(1:nrow(train), replace = TRUE)
  train_b = train[idx_b, ]

  m1_b = lm(LengthOfStay ~ Age + Gender, data = train_b)
  m2_b = lm(LengthOfStay ~ Age + Glucose + Blood.Pressure + BMI +
             Oxygen.Saturation + Cholesterol + Triglycerides + HbA1c +
             Physical.Activity + Diet.Score + Stress.Level + Sleep.Hours +
             Gender, data = train_b)
  m3_b = lm(LengthOfStay ~ poly(Age, 2, raw = TRUE) + poly(BMI, 2, raw = TRUE) +
             Glucose + Blood.Pressure +
             Gender, data = train_b)

  p1_b = predict(m1_b, newdata = test)
  p2_b = predict(m2_b, newdata = test)
  p3_b = predict(m3_b, newdata = test)

  err1[b] = mean( (test$LengthOfStay - p1_b)^2 )
  err2[b] = mean( (test$LengthOfStay - p2_b)^2 )
  err3[b] = mean( (test$LengthOfStay - p3_b)^2 )
}

## Mean and standard deviation of the errors
mean(err1); sd(err1)
mean(err2); sd(err2)
mean(err3); sd(err3)

## Boxplot of the errors
err_df = data.frame(modelo = factor(rep(c("Model 1","Model 2","Model3"), 
                                        each = B)),
                     mse = c(err1, err2, err3))


# PCA analysis -----------------------------------------------------------------
X = data %>%
  select(Age, Glucose, Blood.Pressure, BMI, Oxygen.Saturation, Cholesterol, 
         Triglycerides, HbA1c, Physical.Activity, Diet.Score, Stress.Level, 
         Sleep.Hours)

## We scale the data so that all variables contribute equally to PCA.
X_scaled = scale(X)

## Principal component analysis
pca_res = prcomp(X_scaled, scale = FALSE)
summary(pca_res)

loadings_res = pca_res$rotation[, 1]
contributions_res = loadings_res^2
loadings_res = pca_res$rotation[, 2]
contributions_res = loadings_res^2
fviz_contrib(pca_res, choice = "var", axes = 1)

fviz_eig(pca_res, addlabels = TRUE) ## Variance explained by component
fviz_pca_var(pca_res, col.var = "contrib", 
             gradient.cols = c("grey80","steelblue","darkblue"), repel = TRUE)
fviz_pca_ind(pca_res, geom = "point", habillage = data$Medical.Condition, 
             addEllipses = TRUE)


# Factor analysis --------------------------------------------------------------
fa3 = factanal(X, factors = 3, scores = "Bartlett", rotation = "varimax")
fa3
loadings(fa3)
fa2 = factanal(X, factors = 2, scores = "Bartlett", rotation = "varimax")
fa2
loadings(fa2)
results = cbind(fa3$loadings, fa3$uniquenesses)
colnames(results) = c("factor1", "factor2", "factor3", "uniquenesses")
print(results)


# Clustering -------------------------------------------------------------------
## Evaluate the optimal number of clusters
fviz_nbclust(X_scaled, kmeans, method = "wss", k.max = 10)
fviz_nbclust(X_scaled, kmeans, method = "silhouette", k.max = 10)

## K-means for k = 4
fit.kmeans = kmeans(X_scaled, centers = 4, nstart = 25)
fit.kmeans$size
fit.kmeans$centers[1:4,]
fviz_cluster(fit.kmeans, data = X_scaled, geom = "point", ellipse.type = "norm",
             main = "K-means for k = 4")

centers = fit.kmeans$centers
tidy = cbind(gather(as.data.frame(t(centers)), "cluster", "coor"), 
             var = rep(colnames(centers, k)), 
             size = rep(table(fit.kmeans$cluster), each = ncol(centers))
)
tidy %>%
  ggplot(aes(x = cluster, y = coor, fill = cluster)) +
  geom_col() +
  facet_wrap(~ var) +
  geom_text(aes(label = size),
            position=position_stack(1.2))

## PAM clustering (Partitioning Around Medoids)
fit.pam = pam(X_scaled, k = 4)
fviz_cluster(fit.pam, geom = "point", data = X_scaled, main = "PAM for k = 4")

## Clustering based on Gaussian mixtures
res.Mclust = Mclust(X_scaled)
summary(res.Mclust)
res.Mclust$G ## Suggested number of clusters
fviz_mclust(res.Mclust, "classification")

## Choose one of the 3 tried methods
group = fit.kmeans$cluster

## Assign the final cluster to the dataset
data$cluster = group

## Mean values of the variables per cluster
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
