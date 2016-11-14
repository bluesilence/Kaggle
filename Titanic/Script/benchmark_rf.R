setwd('C:/Users/bluesilence/Kaggle/Titanic/')

# requires following packages
require(data.table)
require(ggplot2)
require(randomForest)

set.seed(1)
train <- read.csv("Data/Raw/train.csv", sep = ',', stringsAsFactors = T)
test <- read.csv("Data/Raw/test.csv", sep = ',', stringsAsFactors = T)

head(train)
summary(train)
table(train$Age)
table(train$Fare)
table(train$Pclass)
table(test$Pclass)
train_hasAge <- train[is.na(train$Age) == FALSE, ]
nrow(train_hasAge)
ggplot(data = train_hasAge, aes(x = Fare, y = Age)) + geom_point() + geom_density2d()

# Benchmark random forest
extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[, features]
  median_age <- median(fea$Age, na.rm = TRUE)
  fea$Age[is.na(fea$Age)] <- median_age
  #fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
  fea$Embarked[fea$Embarked == ""] = "S"
  fea$Sex <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  
  return(fea)
}

train_features <- extractFeatures(train)
sapply(train_features, class)
rf <- randomForest(train_features, as.factor(train$Survived), ntree = 100, importance = TRUE)

test_features <- extractFeatures(test)
# This is to Solve error below:
# Error in predict.randomForest(rf, test_features) : 
# Type of predictors in new data do not match that of the training data.
levels(test_features$Sex) <- levels(train$Sex)
sapply(test_features, class)
levels(test_features$Embarked) <- levels(train$Embarked)
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, test_features)
write.csv(submission, file = "Data/Submission/1_random_forest_r_submission_medianAge.csv", row.names = FALSE)

imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[, 1])
featureImportance

p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("Plot/2_feature_importance_medianAge.png", p)
