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

combined.hasAge <- combined[is.na(combined$Age) == F,]
groups <- list(combined.hasAge$Sex, combined.hasAge$Pclass, combined.hasAge$Title)
grouped <- tapply(combined.hasAge$Age, groups, median, na.)
grouped.df <- as.data.frame.table(grouped)
colnames(grouped.df) <- c('Sex', 'Pclass', 'Title', 'MedianAge')
combined$MedianAge <- (merge(combined, grouped.df))$MedianAge
# Fill NA ages with median age for each group
combined$Age[is.na(combined$Age)] <- combined[is.na(combined$Age),]$MedianAge
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
