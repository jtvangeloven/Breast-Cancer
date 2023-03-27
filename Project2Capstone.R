#Install the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")


#Load the required packages
library(tidyverse)
library(caret)
library(readr)
library(ggpubr)

#Import the data set
url <- "https://raw.githubusercontent.com/jtvangeloven/Breast-Cancer/main/breast%20cancer.csv"
dat <- read_csv(url)
download.file(url, "breast%20cancer.csv")
breast_cancer <- dat

#View breast cancer data set
head(breast_cancer)

#Convert breast cancer tibble into a data frame
breast_cancer <- as.data.frame(breast_cancer)

#Inspect the breast cancer data set
head(breast_cancer)

#Remove the last column, as it contains only NAs
breast_cancer$...33 <- NULL

#Create and assign functional colnames
colnames(breast_cancer) <- c("id", "diagnosis", "radius_mean", "texture_mean",
                             "perimeter_mean", "area_mean", "smoothness_mean", 
                             "compactness_mean", "concavity_mean", 
                             "concave_points_mean", "symmetry_mean", 
                             "fractal_dimension_mean", "radius_se", "texture_se", 
                             "perimeter_se", "area_se", "smoothness_se",
                             "compactness_se", "concavity_se", "concave_points_se",
                             "symmetry_se", "fractal_dimension_se", "radius_worst",
                             "texture_worst", "perimeter_worst", "area_worst", 
                             "smoothness_worst", "compactness_worst", 
                             "concavity_worst", "concave_points_worst", 
                             "symmetry_worst", "fractal_dimension_worst")

#View the final data set
head(breast_cancer)

#Data visualization
#Effectiveness of radius_mean in determining if the tumor is malignant
breast_cancer %>% 
    ggplot(aes(id, radius_mean, color = diagnosis)) +
    geom_point() +
    scale_x_log10()

#Check the id's 
breast_cancer %>%
    arrange(desc(id)) %>%
    select(id) %>%
    top_n(10)

#Boxplots of all of the variables to check how good they are at predicting M or B

#Boxplot of radius means (good) and radius se (good) and radius worst (good)
box_radius_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, radius_mean, color = diagnosis)) +
    geom_boxplot()

box_radius_se <- breast_cancer %>%
    ggplot(aes(diagnosis, radius_se, color = diagnosis)) +
    geom_boxplot()

box_radius_worst <- breast_cancer %>%
    ggplot(aes(diagnosis, radius_worst, color = diagnosis)) +
    geom_boxplot()

ggarrange(box_radius_mean, box_radius_se, box_radius_worst,
          ncol = 2, nrow = 2)

#Boxplot of texture means (not good)
box_texture_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, texture_mean, color = diagnosis)) +
    geom_boxplot()
box_texture_mean

#Boxplot of perimeter mean (good) perimeter se (could be good) worst (good)
box_perimeter_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, perimeter_mean, color = diagnosis)) +
    geom_boxplot()

box_perimeter_se <- breast_cancer %>%
    ggplot(aes(diagnosis, perimeter_se, color = diagnosis)) +
    geom_boxplot()

box_perimeter_worst <- breast_cancer %>%
    ggplot(aes(diagnosis, perimeter_worst, color = diagnosis)) +
    geom_boxplot()

ggarrange(box_perimeter_mean, box_perimeter_se, box_perimeter_worst,
          ncol = 2, nrow = 2)

#Boxplot of area mean (good) and area se (could be good) area worst (good)
box_area_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, area_mean, color = diagnosis)) +
    geom_boxplot()

box_area_se <- breast_cancer %>%
    ggplot(aes(diagnosis, area_se, color = diagnosis)) +
    geom_boxplot()

box_area_worst <- breast_cancer %>%
    ggplot(aes(diagnosis, area_worst, color = diagnosis)) +
    geom_boxplot()

ggarrange(box_area_mean, box_area_se, box_area_worst,
          ncol = 2, nrow = 2)

#Boxplot of Smoothness mean (not good)
box_smooth_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, smoothness_mean, color = diagnosis)) +
    geom_boxplot()
box_smooth_mean

#Boxplot of compactness mean (not good)
box_comp_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, compactness_mean, color = diagnosis)) +
    geom_boxplot()
box_comp_mean

#Boxplot of concavity mean (could be good) and se (not good) and worst (not good) 
box_concavity_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, concavity_mean, color = diagnosis)) +
    geom_boxplot()

box_concavity_se <- breast_cancer %>%
    ggplot(aes(diagnosis, concavity_se, color = diagnosis)) +
    geom_boxplot()

box_concavity_worst <- breast_cancer %>%
    ggplot(aes(diagnosis, concavity_worst, color = diagnosis)) +
    geom_boxplot()

ggarrange(box_concavity_mean, box_concavity_se, box_concavity_worst,
          ncol = 2, nrow = 2)

#Boxplot of concave points mean (good), se (not so good), worst (good)
box_concave_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, concave_points_mean, color = diagnosis)) +
    geom_boxplot()

box_concave_se <- breast_cancer %>%
    ggplot(aes(diagnosis, concave_points_se, color = diagnosis)) +
    geom_boxplot()

box_concave_worst <- breast_cancer %>%
    ggplot(aes(diagnosis, concave_points_worst, color = diagnosis)) +
    geom_boxplot()

ggarrange(box_concave_mean, box_concave_se, box_concave_worst,
          ncol = 2, nrow = 2)

#Boxplot of symmetry mean
box_symmetry_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, symmetry_mean, color = diagnosis)) +
    geom_boxplot()
box_symmetry_mean

#Boxplot of fractal dimension mean
box_fractal_mean <- breast_cancer %>%
    ggplot(aes(diagnosis, fractal_dimension_mean, color = diagnosis)) +
    geom_boxplot()
box_fractal_mean

#Page of less useful parameters
ggarrange(box_texture_mean, box_smooth_mean, box_comp_mean,
          box_symmetry_mean, box_fractal_mean,
          ncol = 2, nrow = 3)

#Good measures of diagnosis are: radius mean, radius se, radius worst,
#perimeter mean, perimeter se, perimeter worst, area mean, area se, area worst

#radius means mean by diagnosis
breast_cancer %>% group_by(diagnosis) %>%
    summarize(mean = mean(radius_mean), se = sd(radius_mean))

#Set seed
set.seed(1, sample.kind("Rounding"))

#Separate into train and test set
y <- breast_cancer$diagnosis
x <- breast_cancer$radius_mean

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- breast_cancer[test_index, ]
train_set <- breast_cancer[-test_index, ]

#Try predicting using the radius mean and choosing a cutoff
cutoff <- seq(12, 18, 0.25)
accuracy_radius_mean <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$radius_mean > x, "M", "B")
    mean(y_hat == test_set$diagnosis)
})

#Plot the cutoffs against their accuracy at predicting B v M
plot(cutoff, accuracy_radius_mean)
accuracy_mean <- max(accuracy_radius_mean)
accuracy_mean

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


#Try perimeter mean
breast_cancer %>% group_by(diagnosis) %>%
    summarize(mean = mean(perimeter_mean), se = sd(perimeter_mean))

#Try cutoffs for perimeter mean
cutoff <- seq(78, 115, 0.25)

accuracy_perimeter_mean <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$perimeter_mean > x, "M", "B")
    mean(y_hat == test_set$diagnosis)
})

plot(cutoff, accuracy_perimeter_mean)

accuracy_perimeter <- max(accuracy_perimeter_mean)
accuracy_perimeter

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Check the prevalence of M and B
prev <- mean(y == "M")
prev

mean(y == "B")
mean(train_set$diagnosis == "M")
mean(test_set$diagnosis == "M")

#Try an glm model for radius mean
train_glm_radius_mean <- train(diagnosis ~ radius_mean, method = "glm", data = train_set)

y_hat_glm_radius_mean <- predict(train_glm_radius_mean, test_set)

confusionMatrix(y_hat_glm_radius_mean, as.factor(test_set$diagnosis))$overall[["Accuracy"]]

#Try a glm model for radius mean, se, and worst
train_glm_radius <- train(diagnosis ~ radius_mean + radius_se + radius_worst, method = "glm", data = train_set)

y_hat_glm_radius <- predict(train_glm_radius, test_set)

accuracy_glm_radius <- confusionMatrix(y_hat_glm_radius, as.factor(test_set$diagnosis))$overall[["Accuracy"]]
accuracy_glm_radius

#Try a glm model for perimeter mean, se, and worst
train_glm_perimeter <- train(diagnosis ~ perimeter_mean + perimeter_se + perimeter_worst, method = "glm", data = train_set)

y_hat_glm_perimeter <- predict(train_glm_perimeter, test_set)

accuracy_perimeter_mean <- confusionMatrix(y_hat_glm_perimeter, as.factor(test_set$diagnosis))$overall[["Accuracy"]]
accuracy_perimeter_mean

#Try glm model for area mean, se, worst
train_glm_area <- train(diagnosis ~ area_mean + area_se + area_worst, method = "glm", data = train_set)

y_hat_glm_area <- predict(train_glm_area, test_set)

accuracy_glm_area <- confusionMatrix(y_hat_glm_area, as.factor(test_set$diagnosis))$overall[["Accuracy"]]
accuracy_glm_area

#Try glm model for concave points mean, se, and worst
train_glm_concave <- train(diagnosis ~ concave_points_mean + concave_points_se + concave_points_worst, method = "glm", data = train_set)

y_hat_glm_concave <- predict(train_glm_concave, test_set)

accuracy_glm_concave <- confusionMatrix(y_hat_glm_concave, as.factor(test_set$diagnosis))$overall[["Accuracy"]]
accuracy_glm_concave

#Try glm model for all predictors
train_glm_all <- train(diagnosis ~ .,
                   method = "glm", data = train_set)

y_hat_glm_all <- predict(train_glm_all, test_set)

accuracy_glm <- confusionMatrix(y_hat_glm_all, as.factor(test_set$diagnosis))$overall[["Accuracy"]]
accuracy_glm

#Try knn model for radius mean (worse than glm)
train_knn_radius_mean <- train(diagnosis ~ radius_mean, method = "knn", data = train_set)

y_hat_knn_radius_mean <- predict(train_knn_radius_mean, test_set)

confusionMatrix(y_hat_knn_radius_mean, as.factor(test_set$diagnosis))$overall[["Accuracy"]]

ggplot(train_knn_radius_mean, highlight = TRUE)

#Try tuning knn for just radius mean
train_knn_radius_tune <- train(diagnosis ~ radius_mean, method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(5, 20)))

ggplot(train_knn_radius_tune, highlight = TRUE)

y_hat_knn_radius_tune <- predict(train_knn_radius_tune, test_set)

confusionMatrix(y_hat_knn_radius_tune, as.factor(test_set$diagnosis))$overall[["Accuracy"]]

#Try tuning knn for all variables
train_knn_all <- train(diagnosis ~ ., 
                   method = "knn",
                   data = train_set[,-1],
                   tuneGrid = data.frame(k = seq(5, 20)))

ggplot(train_knn, highlight = TRUE)

accuracy_knn_all <- confusionMatrix(predict(train_knn_all, test_set),
                as.factor(test_set$diagnosis))$overall["Accuracy"]
accuracy_knn_all

#Try an rpart model
train_rpart_all <- train(diagnosis ~ .,
                         method = "rpart",
                         tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                         data = train_set)
plot(train_rpart_all)

y_hat_rpart <- predict(train_rpart_all, test_set)

accuracy_rpart_all <- confusionMatrix(data = y_hat_rpart, reference = as.factor(test_set$diagnosis))$overall["Accuracy"]
accuracy_rpart_all

#The highest accuracy was the result of the glm on all predictors
#Table of accuracies

table_accuracies <- data.frame(accuracy_mean = mean(accuracy_mean),
                               accuracy_perimeter_mean = mean(accuracy_perimeter_mean),
                               accuracy_glm = mean(accuracy_glm),
                               accuracy_knn_all = mean(accuracy_knn_all),
                               accuracy_rpart_all = mean(accuracy_rpart_all))

t(table_accuracies)

#Confusion matrix for the rpart model
confusionMatrix(data = predict(train_rpart_all, test_set),
                reference = as.factor(test_set$diagnosis))

