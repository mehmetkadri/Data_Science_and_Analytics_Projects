library(ggplot2)
library(cowplot)
library(reshape2)
library(caret)
library(Amelia)
library(e1071)
library(tidyverse)

data <- read.csv("data/data.csv")
label <- select(data,diagnosis)

cormat <- cor(subset(subset( data, select = - X ),select = - diagnosis))

melted_cormat <- melt(cormat)

heatmap_Plot <- ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value))+
geom_tile()+
scale_fill_gradientn(colours = c("black", "#5c0042", "#990159", "red", "#fee1b3", "#fef0be"), values = c(0,0.1,0.2,0.4,0.8,1), name = "")+
theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))+
labs(x = "", y = "", title = "Heatmap")

ggsave("heatmap.png",  width = 4000, height = 2500, units = "px")

############

data <- subset( data, select = - id )
data <- subset( data, select = - X )

data$diagnosis <- as.factor(data$diagnosis)

############

set.seed(1)

partial_data <- createDataPartition(y=data$diagnosis,p=0.8,list = FALSE)

train_data <- data[partial_data,]
test_data <- data[-partial_data,]

model <- glm(diagnosis~.,data = train_data,family = "binomial")

summary(model)

############

prob <- predict(model , newdata = test_data)
pred <- ifelse(prob>0 , "M", "B")
mean(test_data$diagnosis == pred)
confusionMatrix(test_data$diagnosis,as.factor(pred))

############

pca <- prcomp(data[3:31],center = TRUE, scale = TRUE)

summary(pca)

pca_for_heatmap <- as.data.frame(pca$x[,1:6])

pca_cormat <- cor(pca_for_heatmap)
meltec_pca_cormat <- melt(pca_cormat)

pca_heatmap_Plot <- ggplot(meltec_pca_cormat, aes(x=Var1, y=Var2, fill=value))+
geom_tile()+
scale_fill_gradientn(colours = c("black", "#5c0042", "#990159", "red", "#fee1b3", "#fef0be"),values = c(0,0.1,0.2,0.4,0.8,1), name = "")+
theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))+
labs(x = "", y = "", title = "PCAHeatmap")

ggsave("PCAHeatmap.png",  width = 4000, height = 2500, units = "px")

############

set.seed(2)

pca_for_heatmap$diagnosis <- label$diagnosis

partial_pca_data <- createDataPartition(y=pca_for_heatmap$diagnosis,p=0.8,list = FALSE)

pca_train_data <- pca_for_heatmap[partial_pca_data,]
pca_test_data <- pca_for_heatmap[-partial_pca_data,]

pca_model <- glm(as.factor(diagnosis)~.,data = pca_train_data,family = "binomial")

summary(pca_model)


############

pca_prob <- predict(pca_model , newdata = pca_test_data)
pca_pred <- ifelse(pca_prob>0 , "M", "B")
mean(pca_test_data$diagnosis == pca_pred)
confusionMatrix(pca_test_data$diagnosis,as.factor(pca_pred))

############
