install.packages("factoextra")
library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(stats)

data <- iris
str(data)


width_length <-ggplot(data) +
  geom_point(mapping=aes(x = Sepal.Length, y= Sepal.Width, colour = Species))+
  labs(
    x = "Sepal Length",
    y = "Sepal Width",
    title = "Sepal Width vs Sepal Length colored by Species")

width_length

pca <- prcomp(data[1:4] ,center = TRUE, scale = TRUE)
summary(pca)


fviz_pca_ind(pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data$Species, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Species") +
  ggtitle("2D PCA-plot for Iris Data") +
  theme(plot.title = element_text(hjust = 0.5))


set.seed (102)

clusters <- kmeans(data[1:4], 3)

str(clusters)

kmeans_prediction_of_species <- clusters$cluster

kmeans_prediction_of_species <- replace(kmeans_prediction_of_species, kmeans_prediction_of_species==1,"virginica")
kmeans_prediction_of_species <- replace(kmeans_prediction_of_species, kmeans_prediction_of_species==2,"setosa")
kmeans_prediction_of_species <- replace(kmeans_prediction_of_species, kmeans_prediction_of_species==3,"versicolor")

true_species <- data$Species

comparison1 <- data.frame(as.factor(kmeans_prediction_of_species),true_species)
str(comparison)

distance <- dist(data, method = "euclidean")

hierarchical_clustering <- hclust(distance, method = "average" )

hierarchical_clustering
plot(hierarchical_clustering, cex = 0.6, hang = -1)

hierarchically_clustered_data_predictions <- cutree(hierarchical_clustering, k = 3)
hierarchically_clustered_data_predictions
hierarchically_clustered_data_predictions <- replace(hierarchically_clustered_data_predictions, hierarchically_clustered_data_predictions==1,"setosa")
hierarchically_clustered_data_predictions <- replace(hierarchically_clustered_data_predictions, hierarchically_clustered_data_predictions==2,"versicolor")
hierarchically_clustered_data_predictions <- replace(hierarchically_clustered_data_predictions, hierarchically_clustered_data_predictions==3,"virginica")

comparison2 <- data.frame(as.factor(hierarchically_clustered_data_predictions),true_species)

comparison2
