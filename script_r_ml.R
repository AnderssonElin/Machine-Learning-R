# -------------------------------------------------------------------------------------------------
# Data Preparation and Libraries
# -------------------------------------------------------------------------------------------------

# Clear the workspace
rm(list = ls())

# Read in data
# Read the CSV file with semicolon as separator
dataset <- read.csv2("Kollektivavtal_Raw.csv", header = TRUE)

# Install and load necessary libraries
if (!require(caTools)) install.packages("caTools")
if (!require(randomForest)) install.packages("randomForest")
if (!require(cluster)) install.packages("cluster")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(factoextra)) install.packages("factoextra")
if (!require(knitr)) install.packages("knitr")
if (!require(scales)) install.packages("scales")

library(caTools)
library(randomForest)
library(cluster)
library(ggplot2)
library(factoextra)
library(knitr)
library(scales)

# Create directory for graphs if it doesn't exist
if (!dir.exists("data/graphs")) {
  dir.create("data/graphs", recursive = TRUE)
}

# -------------------------------------------------------------------------------------------------
# Load and Prepare Data
# -------------------------------------------------------------------------------------------------
# Read the dataset again and omit NA values
dataset <- read.csv2("Kollektivavtal_Raw.csv", header = TRUE, sep = ";")
dataset <- na.omit(dataset)

# Select relevant variables
# Selecting columns for analysis
dataset <- dataset[, c("antalanstallda_UC", "Omsättning_SEK", "Total_lön", "Genomsnittlig_lön_Inkl..Pension_TSEK", "Resultat")]

# Handle outliers using the IQR method
remove_outliers <- function(x) {
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.05, na.rm = TRUE)
    Q3 <- quantile(x, 0.95, na.rm = TRUE)
    IQR <- Q3 - Q1
    x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- NA
  }
  return(x)
}
dataset[] <- lapply(dataset, remove_outliers)

# Handle missing values
# Remove rows with NA values
dataset <- na.omit(dataset)

# -------------------------------------------------------------------------------------------------
# Clustering
# -------------------------------------------------------------------------------------------------

# Prepare data for K-means clustering
kmeans_data <- dataset[, c("antalanstallda_UC", "Total_lön")]

# Elbow method to determine optimal number of clusters
wcss <- sapply(2:10, function(k) sum(kmeans(kmeans_data, centers = k, nstart = 25)$tot.withinss))
plot1 <- ggplot(data.frame(K = 2:10, WCSS = wcss), aes(x = K, y = WCSS)) +
  geom_line(color = "#8e7ce0") +
  geom_point(color = "#8e7ce0") +
  labs(title = "Elbow Method", x = "Number of Clusters", y = "WCSS") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank())

ggsave("data/graphs/elbow_method.png", plot = plot1)  # Save the Elbow method plot as PNG

# Silhouette Score to evaluate clustering
silhouette_scores <- sapply(2:10, function(k) {
  kmeans_model <- kmeans(kmeans_data, centers = k, nstart = 25)
  mean(silhouette(kmeans_model$cluster, dist(kmeans_data))[, 3])
})
plot2 <- ggplot(data.frame(K = 2:10, Silhouette = silhouette_scores), aes(x = K, y = Silhouette)) +
  geom_line(color = "#8e7ce0") +
  geom_point(color = "#8e7ce0") +
  geom_text(aes(label = round(Silhouette, 2)), vjust = -0.5, size = 3, color = "white") +
  labs(title = "Silhouette Score for Clustering", x = "Number of Clusters", y = "Silhouette Score") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank())

ggsave("data/graphs/silhouette_score.png", plot = plot2)  # Save the Silhouette Score plot as PNG

# Perform K-means clustering with 3 clusters
set.seed(123)
kmeans_result <- kmeans(kmeans_data, centers = 3, nstart = 25)
dataset$Cluster <- factor(kmeans_result$cluster)

# Visualize clustering results
plot3 <- fviz_cluster(kmeans_result, data = kmeans_data, geom = "point", ellipse.type = "convex",
             palette = "Set1", ggtheme = theme_minimal(),
             main = "Clustering with 3 Clusters", stand = FALSE) +
  labs(x = "Number of Employees", y = "Total Salary") +
  theme(plot.background = element_rect(fill = "black"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank())

ggsave("data/graphs/clustering_results.png", plot = plot3)  # Save clustering results plot as PNG

# -------------------------------------------------------------------------------------------------
# Data Splitting for Training and Testing
# -------------------------------------------------------------------------------------------------
set.seed(123)
split <- sample.split(dataset$Resultat, SplitRatio = 0.7)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# -------------------------------------------------------------------------------------------------
# Models
# -------------------------------------------------------------------------------------------------

# Linear regression model
model_regression <- lm(Resultat ~ antalanstallda_UC + Omsättning_SEK + Total_lön + Genomsnittlig_lön_Inkl..Pension_TSEK, data = training_set)
y_pred_regression <- predict(model_regression, newdata = test_set)

# Random Forest model
model_rf <- randomForest(Resultat ~ antalanstallda_UC + Omsättning_SEK + Total_lön + Genomsnittlig_lön_Inkl..Pension_TSEK, data = training_set, ntree = 55)
y_pred_rf <- predict(model_rf, newdata = test_set)

# Linear regression model with clustering
model_regression_cluster <- lm(Resultat ~ Cluster + Omsättning_SEK + Genomsnittlig_lön_Inkl..Pension_TSEK, data = training_set)
y_pred_regression_cluster <- predict(model_regression_cluster, newdata = test_set)

# Random Forest model with clustering
model_rf_cluster <- randomForest(Resultat ~ Cluster + Omsättning_SEK + Genomsnittlig_lön_Inkl..Pension_TSEK, data = training_set, ntree = 55)
y_pred_rf_cluster <- predict(model_rf_cluster, newdata = test_set)

# -------------------------------------------------------------------------------------------------
# Results Table
# -------------------------------------------------------------------------------------------------

# Create a comparison table for model performance
comparison <- data.frame(
  Modell = c("Regression", "Random Forest", "Regression with Clustering", "Random Forest with Clustering"),
  MAE = c(
    mean(abs(test_set$Resultat - y_pred_regression)),
    mean(abs(test_set$Resultat - y_pred_rf)),
    mean(abs(test_set$Resultat - y_pred_regression_cluster)),
    mean(abs(test_set$Resultat - y_pred_rf_cluster))
  ),
  MSE = c(
    mean((test_set$Resultat - y_pred_regression)^2),
    mean((test_set$Resultat - y_pred_rf)^2),
    mean((test_set$Resultat - y_pred_regression_cluster)^2),
    mean((test_set$Resultat - y_pred_rf_cluster)^2)
  ),
  RMSE = c(
    sqrt(mean((test_set$Resultat - y_pred_regression)^2)),
    sqrt(mean((test_set$Resultat - y_pred_rf)^2)),
    sqrt(mean((test_set$Resultat - y_pred_regression_cluster)^2)),
    sqrt(mean((test_set$Resultat - y_pred_rf_cluster)^2))
  )
)

# Print the comparison table
print(comparison)

##############################################################################################
