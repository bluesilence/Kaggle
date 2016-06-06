# Script by Tim Esler for Paribas Kaggle competition using XGBoost.
# XG parameters have been pre-optimised for the minimal amount of feature
# engineering used (recoding of NAs and removal of correlated vars).
setwd('D:/Kaggle/BNPParibasCardifClaimsManagement')

library(data.table)
library(readr)
library(xgboost)



### Preprocessing
cat("Read the train and test data\n")
train <- read.table("data/raw/train.csv", header = T, sep = ",") 
y <- train[, 'target']
train <- train[, -2]
test <- read.table("data/raw/test.csv", header = T, sep = ",")

submission <- read.table("data/raw/sample_submission.csv", header=TRUE, sep=',')


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

## Xgboost
# Run settings
md <- 12 # 12: BEST
ss <- 0.76 # 0.76: BEST
mc <- 7 # 7: BEST -> 0.456036
cs <- 0.45 # 0.45 -> 0.455435
np <- 1

cat("Set seed\n")
set.seed(0)

dval <- xgb.DMatrix(data = data.matrix(tra[h, ]), label = train.label$y[h])
dtrain <- xgb.DMatrix(data = data.matrix(tra[-h, ]), label = train.label$y[-h])
dall <- xgb.DMatrix(data = data.matrix(tra), label = train.label$y)

watchlist <- list(val = dval, train = dtrain)
watchlist.all <- list(train = dall)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "logloss",
                eta                 = 0.01,
                max_depth           = md,
                subsample           = ss,
                colsample_bytree    = cs,
                min_child_weight    = mc,
                num_parallel_tree   = np
)

nrounds <- 1500 # CHANGE TO >1500
early.stop.round <- 300

cat("Train model with cv\n")
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = nrounds, 
                    verbose             = 1,  #1
                    early.stop.round    = early.stop.round,
                    watchlist           = watchlist,
                    maximize            = FALSE
                )

LL <- clf$bestScore
cat(paste("Best AUC: ", LL,"\n", sep=""))

# xgb.cv
bst.cv <- xgb.cv(param = param,
                 data = dall,
                 label = y, 
                 nfold = 3,
                 nrounds = nrounds,
                 prediction = TRUE,
                 verbose = 1)

min.logloss.idx = which.min(bst.cv$dt[, test.logloss.mean]) 
min.logloss.idx
bst.cv$dt[min.logloss.idx,]

ensemble.cv.l1 <- data.table(
                             "Model" = "Xgb",
                             "Test.Logloss.Mean" = bst.cv$dt[min.logloss.idx,]$test.logloss.mean,
                             "Test.Logloss.Std" = bst.cv$dt[min.logloss.idx,]$test.logloss.std
                             )
ensemble.cv.l1

## Gbm
library(gbm)
gbm1 <-
  gbm(y ~ .,                         # formula
      data = tra.label,              # dataset
      distribution = "gaussian",     # see the help for other choices
      n.trees = 1600,                # number of trees
      shrinkage = 0.05,              # shrinkage or learning rate, 0.001 to 0.1 usually work
      interaction.depth = 3,         # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,            # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5,          # fraction of data for training, first train.fraction * N used for training
      n.minobsinnode = 10,           # minimum total weight needed in each node
      cv.folds = 3,                  # do 3-fold cross-validation
      #keep.data = TRUE,            # keep a copy of the dataset with the object
      verbose = TRUE,                # print out progress
      n.cores = 4)                   # use only a single core (detecting #cores is error-prone, so avoided here)

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
                           number = 3, 
                           repeats = 1, 
                           ## Estimate class probabilities 		   	  
                           classProbs = TRUE, 
                           ## Evaluate performance using  
                           ## the following function 	
                           summaryFunction = twoClassSummary) 

set.seed(17)

gbmGrid.baseline <- expand.grid(interaction.depth = 3,
                                n.trees = 1000,
                                shrinkage = 0.05,
                                n.minobsinnode = c(10, 20))

gbm2.baseline <- train(y ~ ., data = tra.label, method = "gbm", 
                        trControl = fitControl, verbose = TRUE,
                        tuneGrid = gbmGrid.baseline)

summary(gbm2.baseline)

gbmGrid <- expand.grid(interaction.depth = c(9, 11), 	
                       n.trees = c(500, 650, 800),
                       shrinkage = 0.05,
                       n.minobsinnode = 20)
# interaction.depth  n.minobsinnode  ROC        Sens       Spec       ROC SD       Sens SD      Spec SD
# 5                  20              0.7480275  0.2083883  0.9602165  0.004140684  0.002331984  0.001988790
gbm2 <- train(y ~ ., data = tra.label, method = "gbm", 
                 trControl = fitControl, verbose = TRUE,
                 tuneGrid = gbmGrid
              )

gbm2
varImp(gbm2)

# plot the performance # plot variable influence
summary(gbm2, n.trees = 1)         # based on the first tree
summary(gbm2, n.trees = gbm2$bestTune$n.trees) # based on the estimated best number of trees
gbm2$finalModel
gbm2$bestTune$n.trees

# f.predict generally will be on the canonical scale (logit,log,etc.)
f.predict <- predict(gbm2$finalModel, newdata = test[, feature.names], n.trees = gbm2$bestTune$n.trees)
head(f.predict)



inputs.gbm <- c("nrounds" = gbm2$bestTune$n.trees,
            "max_depth" = gbm2$bestTune$interaction.depth,
            "shrinkage" = gbm2$bestTune$shrinkage,
            "n.minobsinnode" = gbm2$bestTune$n.minobsinnode)
print(inputs.gbm)

submission.gbm <- data.frame(ID = test$ID, PredictedProb = f.predict)

cat("Create submission file\n")
time <- format(Sys.time(),"%Y%m%dT%H%M%S")

submission.gbm <- submission.gbm[order(submission$ID), ]
write.csv(
         submission.gbm,
         paste("data/final/gbm_V22v8Transformed_PCA1_PCA2_RemovedSpearmanIG_AvgNA_",
               paste(as.character(inputs.gbm),collapse="_"),
               "_",
               time,
               ".csv",sep=""),
         row.names=F)


### Caret Ensemble
library("caret")
library("mlbench")
library("pROC")

set.seed(107)

inTrain <- createDataPartition(y = tra.label$y, p = .8, list = FALSE)
training <- tra.label[inTrain, ]
testing <- tra.label[-inTrain, ]

caretListControl <- trainControl(
                                  method = "cv", 
                                  number = 3, 
                                  savePredictions = "final",
                                  ## Estimate class probabilities 		   	  
                                  classProbs = TRUE,
                                  allowParallel = TRUE,
                                  index = createResample(tra.label$y, 2),
                                  ## Evaluate performance using  
                                  ## the following function
                                  summaryFunction = mnLogLoss
                                )

## Caret List
library("rpart")
library("knncat")
library("caretEnsemble")
library("doParallel")

cl <- makeCluster(detectCores())
registerDoParallel(cl)
model_list <- caretList(
  y ~ .,
  data = tra.label,
  trControl = caretListControl,
  methodList = c(
                 "glm" ######OK
                 #,"rpart"
                 #,"rf"
                 ,"nnet" ######OK
                 #,"knn"
                 #,"extraTrees"
                 #,"svmRadial"
                 #,"svmLinear"
                 #,"xgbTree"
                 )
#   tuneList = list(
#     knncat = caretModelSpec(method = "knncat", tuneLength = 2)
#   )
)
stopCluster(cl)

model_list
# Plot ROC for each resample for the 2 models in model_list
xyplot(resamples(model_list))
modelCor(resamples(model_list))


registerDoParallel(cl)
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
stopCluster(cl)

summary(greedy_ensemble)

## Predict with a list of models
model_preds <- lapply(model_list, predict, newdata = testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"Y"])
model_preds <- data.frame(model_preds)

## Predict with the ensemble of the models in the list above
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$y)

## Predict
final_preds <- predict(greedy_ensemble, newdata = test[, feature.names], type = "prob")
head(final_preds)

submission.ensemble <- data.frame(ID = test$ID, PredictedProb = final_preds)

cat("Create submission file\n")
time <- format(Sys.time(),"%Y%m%dT%H%M%S")

submission.ensemble <- submission.ensemble[order(submission$ID), ]
write.csv(
  submission.ensemble,
  paste("data/final/ensemble_V22v8Transformed_PCA1_PCA2_RemovedSpearmanIG_AvgNA_glm_nnet",
        "_",
        time,
        ".csv",sep=""),
  row.names=F)