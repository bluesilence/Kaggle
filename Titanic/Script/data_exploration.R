setwd('C:/Users/bluesilence/Kaggle/Titanic/')

# requires following packages
require(data.table)
require(ggplot2)

train <- read.csv("Data/Raw/train.csv", sep = ',', stringsAsFactors = T)
#test <- read.csv("Data/Raw/test.csv", sep = ',', stringsAsFactors = T)

head(train)

summary(train)
median_age <- median(train$Age, na.rm = TRUE)
train$Age[is.na(train$Age)] <- median_age
train$Survived <- as.factor(train$Survived)
train$Pclass<- as.factor(train$Pclass)

p <- ggplot(data = train, aes(x = Age, colour = Survived)) + geom_density() + facet_grid(Sex ~ .)
ggsave("Plot/Age_Sex_Survived_Density.png", p, width = 15, height = 10)

p2 <- ggplot(data = train, aes(x = Age, y = Fare, colour = Survived)) + geom_density2d()
ggsave("Plot/Age_Fare_Survived_Density.png", p2, width = 15, height = 15)

p3 <- ggplot(data = train, aes(x = Age, y = Fare, colour = Survived)) + geom_point()
ggsave("Plot/Age_Fare_Survived_Point.png", p3, width = 15, height = 15)

p4 <- ggplot(data = train, aes(x = Fare, colour = Pclass)) + geom_density()
ggsave("Plot/Pclass_Fare_Density.png", p4, width = 15, height = 10)

p5 <- ggplot(data = train, aes(x = Fare, colour = Survived)) + geom_density() + facet_grid(Pclass ~ .)
ggsave("Plot/Pclass_Fare_Survived_Density.png", p5, width = 15, height = 10)

table(train$Embarked, train$Survived)
