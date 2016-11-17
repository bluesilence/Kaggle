setwd('C:/Users/bluesilence/Kaggle/Titanic/')

# requires following packages
require(data.table)
require(ggplot2)

train <- read.csv("Data/Raw/train.csv", sep = ',', stringsAsFactors = F)
test <- read.csv("Data/Raw/test.csv", sep = ',', stringsAsFactors = F)

head(train)

summary(train)
# train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
train$Survived <- as.factor(train$Survived)
train$Pclass<- as.factor(train$Pclass)

summary(test)
# test$Age[is.na(test$Age)] <- median(test$Age, na.rm = TRUE)
test$Pclass<- as.factor(test$Pclass)

print_status <- function(feature) {
  cat("Processing", feature, ": OK")
}

targets <- train$Survived
train <- subset(train, select = -Survived)

combined <- rbind(train, test)
summary(combined)

head(combined$Name)
tail(combined$Name)

##### Extract Titles from Name
extract_title <- function(name) {
  str1 <- strsplit(name, ',')[[1]][2]
  str2 <- strsplit(str1, '\\.')[[1]][1]
  
  return(trimws(str2))
}

# a map of more aggregated titles
original_titles <- c(
  'Capt',
  'Col',
  'Major',
  'Jonkheer',
  'Don',
  'Sir',
  'Dr',
  'Rev',
  'the Countess',
  'Dona',
  'Mme',
  'Mlle',
  'Ms',
  'Mr',
  'Mrs',
  'Miss',
  'Master',
  'Lady'
)

aggr_titles <- c(
  'Officer',
  'Officer',
  'Officer',
  'Royalty',
  'Royalty',
  'Royalty',
  'Officer',
  'Officer',
  'Royalty',
  'Royalty',
  'Mrs',
  'Miss',
  'Mrs',
  'Mr',
  'Mrs',
  'Miss',
  'Master',
  'Royalty'
)

title_dictionary = data.frame(OrigTitle=original_titles,
                              AggrTitle=aggr_titles,
                              stringsAsFactors = FALSE)


names <- unlist(lapply(combined$Name, extract_title))
names_table <- data.frame(OrigTitle = names, stringsAsFactors = F)
combined$Title <- (merge(names_table, title_dictionary))$AggrTitle

# Process Age
combined.hasAge <- combined[is.na(combined$Age) == F,]
groups <- list(combined.hasAge$Sex, combined.hasAge$Pclass, combined.hasAge$Title)

grouped <- tapply(combined.hasAge$Age, groups, median, na.rm = T)
grouped.df <- as.data.frame.table(grouped)
colnames(grouped.df) <- c('Sex', 'Pclass', 'Title', 'MedianAge')
combined$MedianAge <- (merge(combined, grouped.df))$MedianAge
# Fill NA ages with median age for each group
combined[is.na(combined$Age), ]$Age <- combined[is.na(combined$Age), ]$MedianAge
summary(combined$Age)

# One-hot encoding for Title
library(dummies)
d <- dummy.data.frame(combined)
title_onehot = get.dummy(d, 'Title')
combined <- cbind(combined, title_onehot)
head(combined)
combined <- combined[, !(colnames(combined) %in% c("Title"))]

# Filling NA fare with mean
combined$Fare[is.na(combined$Fare)] <- mean(combined$Fare, na.rm = T)

# Filling NA embarked with the most frequent one (S)
combined$Embarked[is.na(combined$Embarked)] <- 'S'
embarked_onehot = get.dummy(d, 'Embarked')
combined <- cbind(combined, embarked_onehot)
head(combined)
combined <- combined[, !(colnames(combined) %in% c("Embarked"))]

# Filling NA cabin with U (Unknown)
combined$Cabin["" == combined$Cabin] <- 'U'
combined$Cabin <- sapply(combined$Cabin, substr, 0, 1)
cabin_onehot = get.dummy(d, 'Cabin')
combined <- cbind(combined, cabin_onehot)
head(combined)
combined <- combined[, !(colnames(combined) %in% c("Cabin"))]
summary(combined)

# Convert Sex to numerical value
convert_sex <- function(sex) {
  if(sex == 'male') {
    return(1)
  }
  else {
    return(0)
  }
}

Sex_converted <- sapply(combined$Sex, convert_sex)
combined$Sex <- Sex_converted

# One-hot encoding for Pclass
pclass_onehot = get.dummy(d, 'Pclass')
combined <- cbind(combined, pclass_onehot)
head(combined)
combined <- combined[, !(colnames(combined) %in% c("Pclass"))]
summary(combined)

is.not.numeric <- function (text) {
  return(!is.na(text) && is.numeric(text))
}

# Process Ticket
is.not.numeric <- function (text) {
   return(is.na(text) || is.na(as.numeric(text)))
}

clean_ticket <- function (ticket) {
   ticket = gsub('\\.', '', ticket)
   ticket = gsub('/', '', ticket)
   ticket_parts = strsplit(ticket, ' ')[[1]]
   ticket_parts_not_numeric = sapply(ticket_parts, is.not.numeric)
   ticket_parts = ticket_parts[ticket_parts_not_numeric]
   #cat(ticket_parts_not_numeric)
   if(length(ticket_parts) > 0) {
      return(ticket_parts[1])
   } else {
      return('XXX')
   }
}

ticket_cleaned <- sapply(combined$Ticket, clean_ticket)
combined$Ticket <- ticket_cleaned
d <- dummy.data.frame(combined)
ticket_onehot = get.dummy(d, 'Ticket')
combined <- cbind(combined, ticket_onehot)
head(combined)
combined <- combined[, !(colnames(combined) %in% c("Ticket"))]
summary(combined)

# Process Family
combined$FamilySize <- combined$Parch + combined$SibSp + 1
combined$Singleton <- sapply(combined$FamilySize, (function (x) if(x == 1) return(1) else return(0)))
combined$SmallFamily <- sapply(combined$FamilySize, (function (x) if(x >= 2 && x <= 4) return(1) else return(0)))
combined$LargeFamily <- sapply(combined$FamilySize, (function (x) if(x >= 5) return(1) else return(0)))

# Normalize numeric features
features <- combined[, !(colnames(combined) %in% c("PassengerId", "Name"))]
features.normalized = lapply(features, (function(col) return(col / max(col))))
table(features$Age)
table(features.normalized$Age)

features.normalized.df <- data.frame(features.normalized)
combined.normalized <- data.frame(PassengerId = combined$PassengerId, features.normalized.df)
combined.normalized <- combined.normalized[, !(colnames(combined.normalized) %in% c("MedianAge"))]

train.normalized <- combined.normalized[combined.normalized$PassengerId %in% train$PassengerId, ]
head(train.normalized[, 1:10])
train.normalized.withLabel <- data.frame(train.normalized, targets)

test.normalized <- combined.normalized[combined.normalized$PassengerId %in% test$PassengerId, ]
head(test.normalized[, 1:10])

# Benchmark model
library(randomForest)
rf <- randomForest(train.normalized, targets, ntree = 1100, importance = TRUE)

# This is to Solve error below:
# Error in predict.randomForest(rf, test_features) : 
# Type of predictors in new data do not match that of the training data.
#levels(test_features$Sex) <- levels(train$Sex)
#levels(test_features$Embarked) <- levels(train$Embarked)
submission <- data.frame(PassengerId = test.normalized$PassengerId)
submission$Survived <- predict(rf, test.normalized)
write.csv(submission, file = "Data/Submission/featureEngineering_rf_1100.csv", row.names = FALSE)

imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[, 1])
featureImportance[order(featureImportance$Importance, decreasing = T), ]
top.features.n <- 15
top.features <- head(featureImportance[order(featureImportance$Importance, decreasing = T), ], top.features.n)

# ExtraTrees
library(extraTrees)
et <- extraTrees(train.normalized[, (colnames(train.normalized) %in% top.features$Feature)], targets, ntree = 150)
submission.et <- data.frame(PassengerId = test.normalized$PassengerId)
submission.et$Survived <- predict(et, test.normalized)
write.csv(submission.et, file = "Data/Submission/featureEngineering_extraTree_150.csv", row.names = FALSE)
