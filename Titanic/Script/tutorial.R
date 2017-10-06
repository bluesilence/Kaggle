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


### Part 5: Random Forests
## R's Random Forest cannot deal with NA's like the rpart does, so we need to fix the NA's first
summary(combi$Age)
nrow(combi)
# 263 out of 1309 rows were missing Age
# Grow a tree on the subset of the data with Age available
# To-Do: Leave off FamilyID here as they don't seem to be having much impact on predicting Age
fitAge <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age), ],
                method = "anova")
combi$Age[is.na(combi$Age)] <- predict(fitAge, combi[is.na(combi$Age), ])
# Now all the NAs in Age are gone
summary(combi$Age)

## Fix missing values for other features
summary(combi)

## Embark has 2 NA's
combi$Embarked <- factor(combi$Embarked)
table(combi$Embarked)
which(is.na(combi$Embarked))
# Replace those 2 with 'S' since it's the majority
combi$Embarked[which(is.na(combi$Embarked))] <- 'S'
table(combi$Embarked)

# Fare is lacking 1 value
summary(combi$Fare)
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm = TRUE)

# R's Random Forest can only digest factors with up to 32 levels
unique(combi$FamilyID)
# So we need to manually reduce the # of levels of FamilyID
# Increase the cut-off of Small family from 2 to 3 people
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
unique(combi$FamilyID2)
# FamilyID2 is down to 22 levels

train <- combi[1:nrow(train), ]
test <- combi[(nrow(train)+1):nrow(combi), ]

library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                          Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE,
                    ntree = 2000)
                    
varImpPlot(fit)

# 6th prediction
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_firstforest.csv", row.names = FALSE)
# The result is not very good.
# On smaller datasets, sometimes a fancier model won't beat a simple one
# Let's try a forest of conditional inference trees
library(party)
set.seed(415)
# mtry: the # of variables to sample at each node
# Since cforest is able to handle factors with more levels than Random Forests can, let's go back to FamilyID
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 7th prediction
# The prediction function requires a few extra nudgfes for cforest
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_cforest.csv", row.names = FALSE)


### Further exploration
## Ticket
table(combi$Ticket)
unique(combi$Ticket)
get_index_of_last_occurrence_of_ch <- function(str, reg_ch)
{
  reg <- paste(reg_ch, "[^", reg_ch, "]*$", sep = "")
  #print(reg)
  regexpr(reg, str)[1]
}

get_substr_before_last_occurrence_of_ch <- function(str, reg_ch)
{
  last_occurrence_index <- get_index_of_last_occurrence_of_ch(str, reg_ch)
  #print(last_occurrence_index)
  
  if (last_occurrence_index < 0) # No reg_ch found
  {
    str
  }
  else
  {
    result <- substring(str, 0, last_occurrence_index-1)
    # Trim leading/trailing whitespace
    trimws(result)
    
    print(result)
  }
}

get_ticket_title <- function(ticket)
{
  if (grepl(' ', ticket, fixed = TRUE))
  {
    raw_title <- get_substr_before_last_occurrence_of_ch(ticket, ' ')
    gsub('\\.', '', raw_title)
  }
  else
  {
    ''
  }
}

combi$TicketTitle <- sapply(combi$Ticket, FUN = get_ticket_title)
unique(combi$TicketTitle)
train <- combi[1:nrow(train), ]
table(train$TicketTitle, train$Survived)
prop.table(table(train$TicketTitle, train$Survived))
# There seems to be some dups at the title that can be manually corrected
combi$TicketTitle[combi$TicketTitle == 'A4'] <- 'A/4'
combi$TicketTitle[combi$TicketTitle == 'A5'] <- 'A/5'
combi$TicketTitle[combi$TicketTitle == 'STON/O 2'] <- 'STON/O2'
combi$TicketTitle[combi$TicketTitle == 'WEP'] <- 'WE/P'
combi$TicketTitle[combi$TicketTitle == 'SC/PARIS'] <- 'SC/Paris'
combi$TicketTitle[combi$TicketTitle == 'SOC'] <- 'SO/C'

unique(combi$TicketTitle)
combi$TicketTitle <- as.factor(combi$TicketTitle)
train <- combi[1:nrow(train), ]
table(train$TicketTitle, train$Survived)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID + TicketTitle,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 8th prediction
test <- combi[(nrow(train)+1):nrow(combi), ]
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_tickettitle_cforest.csv", row.names = FALSE)
# The score is the same as the 7th prediction
# Maybe the # of training cases for each TicketTitle is too small to impact the model

# Try using the sub title before '/'
get_ticket_title2 <- function(ticket_title)
{
  if (grepl('/', ticket_title, fixed = TRUE))
  {
    get_substr_before_last_occurrence_of_ch(ticket_title, '/')
  }
  else
  {
    ticket_title
  }
}

combi$TicketTitle <- as.character(combi$TicketTitle)
combi$TicketTitle2 <- sapply(combi$TicketTitle, FUN = get_ticket_title2)
table(combi$TicketTitle2)
# "A 2" comes from "A. 2. 39186", seems to be "A"
combi$TicketTitle2[combi$TicketTitle2 == "A 2"] <- "A"
combi$TicketTitle <- as.factor(combi$TicketTitle)
combi$TicketTitle2 <- as.factor(combi$TicketTitle2)
train <- combi[1:nrow(train), ]
table(train$TicketTitle2, train$Survived)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID + TicketTitle2,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 9th prediction
test <- combi[(nrow(train)+1):nrow(combi), ]
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_tickettitle2_cforest.csv", row.names = FALSE)
# The score is the same as the 7th prediction

combi$TicketHasTitle <- (combi$TicketTitle != '')
train <- combi[1:nrow(train), ]
prop.table(table(train$TicketHasTitle, train$Survived))
cor(train$TicketHasTitle, train$Survived)
# There is little correlation between TicketTitle and Survived
# That explains why the 8th and 9th prediction are not better than the 7th


## Cabin numbers
unique(combi$Cabin)
# There are some multiple Cabins
combi$HasCabin <- !is.na(combi$Cabin)
train <- combi[1:nrow(train), ]
prop.table(table(train$HasCabin, train$Survived))
cor(train$HasCabin, train$Survived)
# There is some impact of Cabin on Survived
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID + HasCabin,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 10th prediction
test <- combi[(nrow(train)+1):nrow(combi), ]
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_hascabin_cforest.csv", row.names = FALSE)
# Congratulations! The score is 0.81818, better than 7th prediction!

# Still, we can dig deeper into Cabin
# If a big family's cabins are far apart, some of the family members may be missing due to the chaos
library(dplyr)
filter(combi, combi$Cabin == "B52 B54 B56")$FamilyID
filter(combi, combi$Cabin == "B52 B54 B56")$FamilySize
# Weird, this guy is by himself but has 3 cabins

filter(combi, combi$Cabin == "C62 C64")$FamilyID
filter(combi, combi$Cabin == "C62 C64")$FamilySize
# This family has 2 members, still small family

# Since we only consider big families, let's filter out small families
unique(combi$Cabin[combi$FamilySize != 'Small'])

# Check the biggest family onboard
combi$Cabin[combi$FamilyID == '11Sage']
# This family doesn't have Cabin

# Check big families with Cabin
bigFamiliesWithCabin <- filter(combi, !is.na(combi$Cabin) & combi$FamilyID != 'Small')

# Create a table with both Cabin and FamilyID to see how many cabins a family has
familyCabinTable <- data.frame(table(bigFamiliesWithCabin$Cabin, bigFamiliesWithCabin$FamilyID))
familyCabinTable$InCabin <- as.integer(familyCabinTable$Freq > 0)
# Count how many cabins a family has
familyCabinCount <- aggregate(InCabin ~ Var2, data = familyCabinTable, FUN = sum)
names(familyCabinCount) <- c("FamilyID", "CabinCount")
combi <- merge(x = combi, y = familyCabinCount, by = "FamilyID", all.x = TRUE)
train <- combi[!is.na(combi$Survived), ]
prop.table(table(train$CabinCount, train$Survived))
# It seems families that all the members in only 1 cabin have higher chance of Survived

# There is some impact of CabinCount on Survived
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID +
                                     CabinCount,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 11th prediction
test <- combi[is.na(combi$Survived), ]
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_cabincount_cforest.csv", row.names = FALSE)
# The score is 0.81818, the same as the 10th prediction

# Check the deck of Cabin
combi$Deck <- as.factor(sapply(combi$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
table(combi$Deck)

train <- combi[!is.na(combi$Survived), ]
prop.table(table(train$Deck, train$Survived))
table(train$Deck)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                     Embarked + Title + FamilySize + FamilyID +
                                     HasCabin + Deck,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

# 12th prediction
test <- combi[is.na(combi$Survived), ]
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "../Prediction/predict_hascabin_deck_cforest.csv", row.names = FALSE)
# Awesome! The score is 0.82296!!