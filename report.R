require(readr)
require(ggplot2)
require(caret)
require(dplyr)
require(randomForest)
require(gbm)
require(pROC)

data <- read.csv("D:/Jenny/Durham/scda/Classification/heart_failure.csv")

# Problem Description

# Data summary
summary(data)

# Visualisations of the data
# Plot 1: Boxplot of ejection_fraction by fatal_mi status

ggplot(data, aes(x=factor(fatal_mi), y=ejection_fraction, fill=factor(fatal_mi))) + geom_boxplot() + 
  labs(title="Ejection Fraction by Fatal MI Status",
       x="Fatal MI Status",
       y="Ejection Fraction",
       fill="Fatal MI Status") +
  theme_minimal()

# Plot 2: Histogram of age with fatal_mi overlay
ggplot(data, aes(x=age, fill=factor(fatal_mi))) +
  geom_histogram(position="identity", alpha=0.5, bins=30) +
  labs(title="Age Distribution with Fatal MI Overlay",
       x="Age",
       y="Count",
       fill="Fatal MI Status") +
  theme_minimal()

# Plot 3: Density plot of serum_creatinine colored by fatal_mi
ggplot(data, aes(x=serum_creatinine, color=factor(fatal_mi))) +
  geom_density() +
  labs(title="Density of Serum Creatinine by Fatal MI Status",
       x="Serum Creatinine",
       y="Density",
       color="Fatal MI Status") +
  theme_minimal()



# Model Fitting
data$fatal_mi <- factor(data$fatal_mi, levels = c(0, 1))
index <- createDataPartition(data$fatal_mi, p=0.8, list=FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

train_control <- trainControl(method="cv", number=10)

models <- c("glm", "rf", "gbm")

fit_models <- function(models, train_data, train_control) {
  results <- list()
  for(model in models) {
    set.seed(100)
    fit <- train(fatal_mi~., data=train_data, method=model, trControl=train_control)
    results[[model]] <- fit
  }
  return(results)
}

model_fitting <- fit_models(models, train_data, train_control)

results <- resamples(model_fitting)

# Model Improvements
summary(results)

bwplot(results, metric = "Accuracy")

tuneGrid <- expand.grid(
  interaction.depth = c(1, 3, 5), 
  n.trees = seq(50, 150, 50), 
  shrinkage = c(0.01, 0.1), 
  n.minobsinnode = c(10, 20)
)

set.seed(100)
gbm <- train(
  fatal_mi ~ ., 
  data = train_data, 
  method = "gbm", 
  trControl = train_control, 
  verbose = FALSE, 
  tuneGrid = tuneGrid
)

print(gbm)

tuning_results <- gbm$results


# Performance Evaluation

confMatrix_df <- as.data.frame(confMatrix$table)

ggplot(data=confMatrix_df, aes(x=Reference, y=Prediction, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  geom_text(aes(label=sprintf("%0.0f", Freq)), vjust=1) +
  labs(title="Confusion Matrix Heatmap", x="Actual Label", y="Predicted Label") +
  theme_minimal()

prob_predictions <- predict(gbm, newdata=test_data, type="prob")

roc_obj <- roc(test_data$fatal_mi, prob_predictions[,2], levels=rev(levels(test_data$fatal_mi)))

plot(roc_obj, main="ROC Curve")
abline(a=0, b=1, lty=2, col="red")

