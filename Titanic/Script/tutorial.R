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
test$Sex <- as.factor(test$Sex)
str(train)
str(test)

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


### Part 3: Start using decision trees
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

### Part 4: Feature Engineering
## Take a glance at Name
train$Name[1]
head(train$Name)
tail(train$Name)
# Notice the titles indicate the social status of passengers, which may be an important feature
test$Survived <- NA

## Add intermediate columns in test so that train and test have the same columns
test$Child <- 0
test$Child[test$Age < 18] <- 1

test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'

# Combine train and test sets
combi <- rbind(train, test)
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
# Apply a function to get the title for each name
combi$Title <- sapply(combi$Name, FUN = function(x) { strsplit(x, split = '[,.]')[[1]][2] })
# Trim the 1st space
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

## Combine a few of the most unusual titles
# Mme: Madame
# Mlle: Mademoiselle
# They are pretty similar
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
# Military titles or rich fellas: Captain, Don, Major and Sir
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
# For ladies of noble birth: Dona, Lady, Jonkheer, Countess
combi$Title[combi$Title %in% c('Dona', 'Lady', 'Jonkheer', 'Countess')] <- 'Lady'
# Change the Title to factor type
combi$Title <- factor(combi$Title)

## Create a new feature FamilySize
combi$FamilySize <- combi$SibSp + combi$Parch + 1
# Combining the Surname with FamilySize could find ppl in the same family
combi$Surname <- sapply(combi$Name, FUN = function(x) { strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
# Still, the single ppl with the same Surname would have the same FamilyID
# Our hypothesis is that large families might have trouble sticking together in the panic
# By knocking out any small families would fix this problem
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
# A lot of the # of ppl with the same FamilyID != their FamilySize
# So let's subset this dataframe to show only those unexpectedly small FamilyID groups
famIDs <- famIDs[famIDs$Freq <= 2, ]
# Overwrite these FamilyIDs to be small families
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
head(combi$FamilyID)

nrow(train)
train <- combi[1:nrow(train), ]
test <- combi[(nrow(train)+1):nrow(combi), ]

# 5th prediction
fit <- rpart(
              Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
              data = train,
              method = "class"
            )

fancyRpartPlot(fit)
# It shows another drawback with decision trees: they are biased to favour factors with many levels

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_seconddtree.csv", row.names = FALSE)
