# Script by Tim Esler for Paribas Kaggle competition using XGBoost.
# XG parameters have been pre-optimised for the minimal amount of feature
# engineering used (recoding of NAs and removal of correlated vars).
setwd('C:/Kaggle/BNPParibasCardifClaimsManagement')

library(data.table)
library(readr)
library(xgboost)



### Preprocessing
cat("Read the train and test data\n")
train <- read.table("data/raw/train.csv", header = T, sep = ",") 
y <- train[, 'target']
train <- train[, -2]
test <- read.table("data/raw/test.csv", header = T, sep = ",")



# Find factor variables and translate to numeric
# Because XGBoost gradient booster only recognizes numeric data
f <- c()
for(i in 1:ncol(train)) {
  if (is.factor(train[, i])) f <- c(f, i)
}

f.t <- c()
for(i in 1:ncol(test)) {
  if (is.factor(test[, i])) f.t <- c(f.t, i)
}

ttrain <- rbind(train, test)
for (col in f) {
  ttrain[, col] <- as.numeric(ttrain[, col]) 
}

### Set all NA to the mean of nonNA values in that column
head(ttrain)

for (colIndex in 2:ncol(ttrain)) {
  currCol <- ttrain[colIndex]
  currCol.nonNA <- currCol[!is.na(currCol)]
  # print(currCol.nonNA)
  colMean <- mean(currCol.nonNA)
  # print(colMean)
  ttrain[colIndex][is.na(ttrain[colIndex])] <- colMean
}

head(ttrain)

# V22 transformation: https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/forums/t/18734/looks-like-a-nice-challenge
# Very small improvement: 0.00014
levels <- unique(ttrain$v22)
levels <- levels[order(nchar(levels), tolower(levels))] # Order by length of chars, then by alphabet
ttrain$v22 <- as.integer(factor(ttrain$v22, levels = levels))

## v8 transformation: Only 2 v8 < 0 and they are very close to 0
## Not necessary since v8 is removed later
# train.v8 <- train$v8
# train.v8.nonNA <- train.v8[train.v8 > -997]
# length(train.v8.nonNA[train.v8.nonNA < 0])
# summary(train.v8.nonNA[train.v8.nonNA < 0])
# train.v8.nonNA[train.v8.nonNA < 0]
# 
# test.v8 <- test$v8
# test.v8.nonNA <- test.v8[test.v8 > -997]
# summary(test.v8.nonNA[test.v8.nonNA < 0])
# length(test.v8.nonNA[test.v8.nonNA < 0])
# test.v8.nonNA[test.v8.nonNA < 0]

# setV8to0 <- function(x) {
#   if (x > -997 && x < 0) {
#     0
#   } else {
#     x
#   }
# }
# 
# ttrain$v8 <- sapply(ttrain$v8, setV8to0)

## Analysis on v25, v46, v54, v63, v105, v8
# Their Cronbach's alpha is so high that it's probably the same information behind
library(psy)
cronbach(train[ , c(9, 26, 47, 55, 64, 106)])
  
# PCA on v8, v25, v46, v54, v63, v105
library(stats)
pca.1 <- princomp(ttrain[ , c(9, 26, 47, 55, 64, 106)], cor = TRUE, scores = TRUE)
summary(pca.1)
plot(pca.1)
pca.1$loadings
# Transformed dataset after PCA
head(pca.1$scores)
# Use only Comp.1 and Comp.2 from PCA.1 and remove the 6 columns later
ttrain$pca.1.Comp.1 <- pca.1$scores[ , "Comp.1"]
ttrain$pca.1.Comp.2 <- pca.1$scores[ , "Comp.2"]

## PCA on v10, v34, v40
# v34 has a perfect link with v10 and v40 using an intercept. Hence, you can do 3->2 variables.
# ggplot(data = train.label[v34 > -997], aes(x = v34, y = v10, color = y)) + geom_point()
# ggplot(data = train.label[v34 > -997], aes(x = v34, y = v40, color = y)) + geom_point()
cronbach(train[ , c(11, 35, 41)])
cor(train[ , c(11, 35, 41)])

pca.2 <- princomp(ttrain[ , c(11, 35, 41)], cor = TRUE, scores = TRUE)
summary(pca.2)
plot(pca.2)
pca.2$loadings
# Use only Comp.1 and Comp.2 from PCA.2 and remove the 3 columns later
ttrain$pca.2.Comp.1 <- pca.2$scores[ , "Comp.1"]
ttrain$pca.2.Comp.2 <- pca.2$scores[ , "Comp.2"]

## Analysis on v47, v110
# Lambda test
# library(rapportools)
# lambda.test(table(ttrain$v47, ttrain$v110), direction = 2)
# table(ttrain$v47, ttrain$v110)

## Analysis on v71, v79: Worse!
# table(ttrain$v71)
# table(ttrain$v79)
# # Contingency table
# table(ttrain$v71, ttrain$v79)
# # U test
# wilcox.test(ttrain$v71, ttrain$v79, correct = F)
# # Convert singular values to the mode of the category
# ConvertSingularOnV71 <- function(x) {
#   if (x == 1 || x == 4 || x == 8 || x == 10 || x == 11 || x == 12) {
#     5
#   } else {
#     x
#   }
# }
# ttrain$v71 <- sapply(ttrain$v71, ConvertSingularOnV71)
# 
# ConvertSingularOnV79 <- function(x) {
#   if (x == 12) { # Still can remove 6, 7, 9?
#     3
#   } else {
#     x
#   }
# }
# ttrain$v79 <- sapply(ttrain$v79, ConvertSingularOnV79)

## Feature Selection
train <- ttrain[1:nrow(train), ]
test <- ttrain[(nrow(train) + 1):nrow(ttrain), ]
train.label <- data.table(train, y)
ncol(train)

library(FSelector)
## Pearson Correlation
weights.pearson <- linear.correlation(y ~ ., train.label)
print(weights.pearson)
top.pearson <- cutoff.k(weights.pearson, 130)
print(top.pearson)
# Features not important in Pearson
setdiff(names(train), top.pearson)

## Spearman Correlation
weights.spearman <- rank.correlation(y ~ ., train.label)
print(weights.spearman)
top.spearman <- cutoff.k(weights.spearman, 130)
print(top.spearman)
# Features not important in Spearman
setdiff(names(train), top.spearman)

## Information Gain
weights.ig <- information.gain(y ~ ., train.label)
print(weights.ig)
top.ig <- cutoff.k(weights.ig, 130)
# Features not important in Information Gain
setdiff(names(train), top.ig)

# Features important in both Pearson and Spearman and Information Gain
intersect(intersect(top.pearson, top.spearman), top.ig)

# Plot feature against y
library(ggplot2)
ggplot(data = train.label, aes(x = v125)) + geom_density() + facet_grid(y ~ .)

## Plot v69 + v115
train.label$sumv69andv115 <- train.label$v69 + train.label$v115
# v69 + v115 ~= 20
ggplot(data = train.label, aes(x = sumv69andv115, color = y)) + geom_density() + xlim(18, 22)
cor(train.label$v69, train.label$v115)

# Plot the sum against v131
ggplot(data = train.label, aes(x = v131, y = sumv69andv115, color = y)) + geom_point()
cor(train.label$sumv69andv115, train.label$v131)
information.gain(v131 ~ sumv69andv115, train.label)
# Remove v115 doesn't improve AUC, so keep it

## Outlier Detection -- TOO SLOW!
# library(DMwR)
# # remove "Species", which is a categorical column
# iris2 <- iris[,1:4]
# outlier.scores <- lofactor(iris2, k=5)
# plot(density(outlier.scores))
# outliers <- order(outlier.scores, decreasing=T)[1:5]
# # who are outliers
# print(outliers)
### End of Exploratory Analysis

### Begin of Training
cat("Sample data for early stopping\n")
h <- sample(nrow(train.label), 1500)

cat("Get feature names\n")

cat("Remove highly correlated features\n")
# Removing all pearsonRemovals makes AUC worse
#pearsonRemovals <- c("v22", "v42", "v49", "v120", "v126")
pearsonRemovals <- c()
spearmanRemovals <- c("v22", "v49", "v77", "v105")
# None: 0.464669
# v122 only: 0.464596
# v125 only: 0.463621
# v126 only: 0.4644
# v127 only: 0.463639
# v122 + v125: worse than either of them
igRemovals <- c("v125") #"v122","v124","v125","v126","v127","pca.1.Comp.1")

highCorrRemovals <- c("v8","v10","v23","v25","v34","v36","v37","v40","v46",
                      "v51","v53","v54","v63","v73","v81",
                      "v82","v89","v92","v95","v105","v107",
                      "v108","v109","v116","v117","v118",
                      "v119","v123","v124","v128",
                      "sumv69andv115",
                      "y")

totalRemovals <- cbind(highCorrRemovals, pearsonRemovals, spearmanRemovals, igRemovals)

feature.names <- names(train.label)[c(2:(ncol(train.label)))] # Remove Id

feature.names <- feature.names[!(feature.names %in% totalRemovals)]
feature.names

# Filter out highly correlated features
tra <- subset(train.label, , feature.names)
tra.label <- cbind(tra, y = as.factor(train.label$y))
levels(tra.label$y)
library(plyr)
tra.label$y <- revalue(tra.label$y, c("1"="Y", "0"="N"))
tra.label$y <- as.factor(tra.label$y)

### Run in parallel
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

## Gbm
library(gbm)
gbm1 <-
  gbm(y ~ .,                         # formula
      data = tra.label,              # dataset
      distribution = "bernoulli",     # see the help for other choices
      n.trees = 500,                # number of trees
      shrinkage = 0.05,              # shrinkage or learning rate, 0.001 to 0.1 usually work
      interaction.depth = 3,         # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,            # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5,          # fraction of data for training, first train.fraction * N used for training
      n.minobsinnode = 10,           # minimum total weight needed in each node
      cv.folds = 3,                  # do 3-fold cross-validation
      #keep.data = TRUE,            # keep a copy of the dataset with the object
      verbose = TRUE,                # print out progress
      n.cores = 4)                   # use only a single core (detecting #cores is error-prone, so avoided here)

stopCluster(cl)

# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method="OOB")
print(best.iter)

# check performance using a train.fraction heldout test set
best.iter <- gbm.perf(gbm1, method="test")
print(best.iter)

# check performance using cv.folds cross-validation
gbm.best.iter <- gbm.perf(gbm1, method="cv")
print(gbm.best.iter)

# plot the performance # plot variable influence
summary(gbm1, n.trees = 1)         # based on the first tree
summary(gbm1, n.trees = best.iter) # based on the estimated best number of trees

## Control gbm using caret
library(caret)
fitControl <- trainControl(method = "cv", 
                           number = 5, 
                           repeats = 1, 
                           ## Estimate class probabilities 		   	  
                           classProbs = TRUE, 
                           ## Evaluate performance using  
                           ## the following function 	
                           summaryFunction = mnLogLoss) 

set.seed(17)

gbmGrid.baseline <- expand.grid(interaction.depth = 3,
                                n.trees = 1000,
                                shrinkage = 0.05,
                                n.minobsinnode = c(10))

cl <- makeCluster(detectCores())
registerDoParallel(cl)
system.time(gbm2.baseline <- train(y ~ ., data = tra.label, method = "gbm", 
                        trControl = fitControl, verbose = TRUE,
                        tuneGrid = gbmGrid.baseline,
                        metric = "logLoss"))
stopCluster(cl)

f.predict.baseline <- predict(gbm2.baseline$finalModel, newdata = test[, feature.names], n.trees = gbm2.baseline$bestTune$n.trees)
head(f.predict.baseline)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
system.time(gbm2.baseline <- train(y ~ ., data = tra.label, method = "gbm", 
                                   trControl = fitControl, verbose = TRUE,
                                   tuneGrid = gbmGrid.baseline))
stopCluster(cl)

summary(gbm2.baseline)
gbm2.baseline


cl <- makeCluster(detectCores())
getDoParWorkers()
registerDoParallel(cl)

gbmGrid <- expand.grid(interaction.depth = c(12), 	
                       n.trees = c(1100),
                       shrinkage = c(0.03),
                       n.minobsinnode = c(20)
                       )
# interaction.depth  n.minobsinnode  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD
# 5                  20              0.7480275  0.2083883  0.9602165  0.004140684  0.002331984  0.001988790
system.time(
            gbm2 <- train(y ~ ., data = tra.label, method = "gbm", 
                 trControl = fitControl, verbose = TRUE,
                 tuneGrid = gbmGrid,
                 distribution = "bernoulli",
                 metric = "logLoss"
              )
            )
stopCluster(cl)

gbm2
varImp(gbm2)

# plot the performance # plot variable influence
summary(gbm2, n.trees = 1)         # based on the first tree
summary(gbm2, n.trees = gbm2$bestTune$n.trees) # based on the estimated best number of trees
gbm2$finalModel
gbm2$bestTune$n.trees

# f.predict generally will be on the canonical scale (logit,log,etc.)
f.predict <- predict(gbm2$finalModel, newdata = test[, feature.names], n.trees = gbm2$bestTune$n.trees, type = 'response')
head(f.predict)



inputs.gbm <- c("nrounds" = gbm2$bestTune$n.trees,
            "max_depth" = gbm2$bestTune$interaction.depth,
            "shrinkage" = gbm2$bestTune$shrinkage,
            "n.minobsinnode" = gbm2$bestTune$n.minobsinnode)
print(inputs.gbm)

submission.gbm <- data.frame(ID = test$ID, PredictedProb = f.predict)

cat("Create submission file\n")
time <- format(Sys.time(),"%Y%m%dT%H%M%S")

submission.gbm <- submission.gbm[order(submission.gbm$ID), ]
write.csv(
         submission.gbm,
         paste("data/final/gbm_V22v8Transformed_PCA1_PCA2_RemovedSpearmanIG_AvgNA_",
               paste(as.character(inputs.gbm), collapse="_"),
               "_",
               time,
               ".csv",sep=""),
         row.names=F)


### Random Forest ### Not working!!!
cl <- makeCluster(detectCores())
getDoParWorkers()
registerDoParallel(cl)

rf_model <- train(y ~ ., data = tra.label, method = "rf",
                  trControl = fitControl,
                  verbose = TRUE,
                  prox = TRUE,
                  allowParallel = TRUE,
                  metric = "logLoss")
stopCluster(cl)

print(rf_model)
plot(rf_model)
varImp(rf_model)


### SVM
library(e1071)
svm_model <- svm(y ~ ., data = tra.label, method = "C-classification", kernal = "radial")
summary(svm_model)

pred <- predict(svm_model, tra, type = "probabilities", kernal = "radial")

## First pass
set.seed(1492)

#Train and Tune the SVM
grid <- expand.grid(sigma = c(.01),#, .015, 0.2),
                    C = c(0.75)#, 0.9, 1, 1.1, 1.25)
                    )

cl <- makeCluster(detectCores())
getDoParWorkers()
registerDoParallel(cl)
svm.tune <- train(y ~ ., data = tra.label,
                  method = "svmRadial",   # Radial kernel
                  preProc = c("center","scale"),  # Center and scale data
                  metric = "logloss",
                  tuneGrid = grid,
                  trControl = fitControl)
stopCluster(cl)
summary(svm.tune)
svm.tune$finalModel
svm.tune$bestTune

# Predict on training set
pred <- predict(svm.tune, tra, type = "prob", kernal = "radial")
head(pred, 10)
binarization <- function (row) { if (row[0] > row[1]) 0 else 1 }
pred.int <- sapply(pred, binarization)

# Predict on test set
svm.predict <- predict(svm.tune, test[, feature.names], type = "prob", kernal = "radial")
head(svm.predict, 10)
head(svm.predict$Y, 10)

inputs.svm <- c("sigma" = svm.tune$bestTune[1],
                "C" = svm.tune$bestTune[2])
print(inputs.svm)

submission.svm <- data.frame(ID = test$ID, PredictedProb = svm.predict$Y)
head(submission.svm, 10)

cat("Create submission file\n")
time <- format(Sys.time(),"%Y%m%dT%H%M%S")

submission.svm <- submission.svm[order(submission.svm$ID), ]
write.csv(
  submission.svm,
  paste("data/final/svm_V22v8Transformed_PCA1_PCA2_RemovedSpearmanIG_AvgNA_",
        paste(as.character(inputs.svm), collapse="_"),
        "_",
        time,
        ".csv",sep=""),
  row.names=F)
