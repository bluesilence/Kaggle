### Part 1: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/
## Initialization
setwd("D:/kaggle/Titanic/Data/Raw")

# Import libraries
require(ggplot2)

# Import Dataset
library(readr)
train <- read_csv("train.csv")
test <- read_csv("test.csv")

summary(train)
summary(test)
str(train)
table(train$Survived)
# Sex should be imported as factor
train$Sex <- as.factor(train$Sex)
str(train)

prop.table(table(train$Survived))

## 1st prediction: everyone dies in the test set
# The public leader board shows accuracy of 0.62679
# This is close to the prior distribution of Survived in the training set: p ~= 0.616
nrow(test)
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "../Prediction/predict_theyallperish.csv", row.names = FALSE)


### Part 2: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-2-the-gender-class-model/
summary(train$Sex)
## 2-way comparison on the # of males/females that survived
prop.table(table(train$Sex, train$Survived))
# Proportions in the 1st dimension which stands for the rows (using “2” instead would give you column proportions)
prop.table(table(train$Sex, train$Survived), 1)

# 2nd prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
prop.table(table(test$Survived))
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "../Prediction/predict_maleallperish.csv", row.names = FALSE)

## Digging into Age
summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1
# Create a table with both gender and age to see the survival proportions
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
# Find out the total # of ppl in each subset
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
# Calculate the proportion for each subset
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) { sum(x) / length(x) })
# It still appears that if a passenger is female most survive,
# regardless of whether they were a child or not.
# So we haven’t got anything to change our predictions on here

## Dig into Class
# Bin the fares into several groups
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) { sum(x) / length(x) })
# 8  20-30      3 female 0.3333333
# 9    30+      3 female 0.1250000
# It’s a little hard to imagine why someone in third class with an expensive ticket would be worse off in the accident,
# but perhaps those more expensive cabins were located close to the iceberg impact site,
# or further from exit stairs

# 3rd prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "../Prediction/predict_3.csv", row.names = FALSE)


## Start using decision trees
library(rpart)

fit <- rpart(
              Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = train,
              method = "class"
            )
plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

# 4th prediction
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_firstdtree.csv", row.names = FALSE)

# Customize control of rpart
fit <- rpart(
              Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = train,
              method = "class",
              control = rpart.control(minsplit = 2, cp = 0)
            )
fancyRpartPlot(fit)
# It's overfitting
# Interactively trim the tree
new.fit <- prp(fit, snip = TRUE)$obj
fancyRpartPlot(new.fit)
