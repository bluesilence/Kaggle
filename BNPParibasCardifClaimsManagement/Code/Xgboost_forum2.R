# Script by Tim Esler for Paribas Kaggle competition using XGBoost.
# XG parameters have been pre-optimised for the minimal amount of feature
# engineering used (recoding of NAs and removal of correlated vars).
setwd('C:/Users/quinzh/Documents/src/Kaggle/BNPParibasCardifClaimsManagement')

library(data.table)
library(readr)
library(xgboost)

# Run settings
md <- 13
ss <- 0.96
cs <- 0.4
mc <- 5
np <- 1

cat("Set seed\n")
set.seed(0)

### Preprocessing
cat("Read the train and test data\n")
train <- read.table("data/raw/train.csv", header = T, sep = ",") 
y <- train[, 'target']
train <- train[, -2]
test <- read.table("data/raw/test.csv", header = T, sep = ",") 
train[is.na(train)] <- -997
test[is.na(test)] <- -997

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

### End of Exploratory Analysis

### Begin of Training
train <- ttrain[1:nrow(train), ]
test <- ttrain[(nrow(train) + 1):nrow(ttrain), ]
train.label <- data.table(train, y)

cat("Sample data for early stopping\n")
h <- sample(nrow(train.label), 1500)

cat("Get feature names\n")
feature.names <- names(train.label)[c(2:(ncol(train.label)-1))] # Remove Id and y

cat("Remove highly correlated features\n")
highCorrRemovals <- c("v8","v10","v23","v25","v34","v36","v37","v40","v46",
                      "v51","v53","v54","v63","v73","v81",
                      "v82","v89","v92","v95","v105","v107",
                      "v108","v109","v116","v117","v118",
                      "v119","v123","v124","v128")
feature.names <- feature.names[!(feature.names %in% highCorrRemovals)]
feature.names

# Filter out highly correlated features
tra <- subset(train.label, , feature.names)

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

nrounds <- 1600 # CHANGE TO >1500
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
                 nfold = 5,
                 nrounds = nrounds,
                 prediction = TRUE,
                 verbose = 1)

min.merror.idx = which.min(bst.cv$dt[, test.merror.mean]) 
min.merror.idx
bst.cv$dt[min.merror.idx,]

clf.all <- xgb.train(   params              = param, 
                        data                = dall, 
                        nrounds             = clf$bestInd,
                        watchlist           = watchlist.all,
                        verbose             = 1,  #1
                        maximize            = FALSE
                    )

LL.all <- clf.all$bestScore
cat(paste("Best AUC on all training data: ", LL.all, "\n", sep=""))

inputs <- c("nrounds" = clf$bestInd,
            "eta" = param$eta,
            "max_depth" = param$max_depth,
            "subsample" = param$subsample,
            "colsample_bytree" = param$colsample_bytree,
            "min_child_weight" = param$min_child_weight,
            "num_parallel_tree" = param$num_parallel_tree)
print(inputs)

cat("Calculate predictions\n")
pred1 <- predict(clf.all,
                 data.matrix(test[, feature.names]),
                 ntreelimit = clf$bestInd)

pred2 <- predict(clf,
                 dval,
                 ntreelimit = clf$bestInd)

submission <- data.frame(ID = test$ID, PredictedProb = pred1)

cat("Create submission file\n")
time <- format(Sys.time(),"%Y%m%dT%H%M%S")

submission <- submission[order(submission$ID), ]
write.csv(
         submission,
         paste("data/final/xgboost_forum2_V22v8Transformed_PCA1_PCA2_",
               paste(as.character(inputs),collapse="_"),
               "_",
               as.character(LL),
               "_",
               time,
               ".csv",sep=""),
         row.names=F)