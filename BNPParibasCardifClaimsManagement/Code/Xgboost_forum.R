# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)
library(data.table)

require(mlbench)
require(caret)
require(corrplot)
require(Rtsne)
require(knitr)
knitr::opts_chunk$set(cache=TRUE)
#require(Boruta)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

setwd('D:/Kaggle-bak/BNPParibasCardifClaimsManagement')
source("D:/Kaggle/Kaggle/lib/FeatureEngineer/Stats/BasicStats.R")

### Preprocessing
# Any results you write to the current directory are saved as output.
train <- read.table("data/raw/train.csv", header=T, sep=",") 
y <- train[, 'target']
train <- train[, -2]
test <- read.table("data/raw/test.csv", header=T, sep=",") 
train[is.na(train)] <- -1
test[is.na(test)] <- -1

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

train <- ttrain[1:nrow(train), ]
test <- ttrain[(nrow(train)+1):nrow(ttrain), ]

## Check basic stats
train.feature.stats <- collectFeatureStats(train)

write.csv(train.feature.stats, "data/intermediate/train.feature.stats_V22Transformed.csv", row.names=T, quote=F)

# Collect stats by label
train.label <- data.table(train, y)
train.pos.feature.stats <- collectFeatureStats(train[train.label$y == 1, ])
train.neg.feature.stats <- collectFeatureStats(train[train.label$y == 0, ])

train.features.stats.byLabel <- rbind(data.table("Label" = 1, train.pos.feature.stats), data.table("Label" = 0, train.neg.feature.stats))

write.csv(train.features.stats.byLabel, "data/intermediate/train.features.stats.byLabel_V22Transformed.csv", row.names=T, quote=F)

## Find highly correlated features
# calculate correlation matrix
# correlationMatrix <- cor(train)
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally > 0.75)
# # WARNING: Too slow!
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.95)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)

## Rank features by importance
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model using Random Forest
model <- train(y ~ ., data = train.label, method = "rf", preProcess = "scale", trControl = control)
# estimate variable importance
importance <- varImp(model, scale = FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

## Check for feature's variance
zero.var = nearZeroVar(train, saveMetrics=TRUE)
zero.var
# V3, V38, V74 have (nzv == TRUE), indicating that they have near zero variance
# No improvement on logloss, even make it worse
train.reduced <- train[, -4][, -38][, -73]
head(train.reduced)

# Plot correlations (picture is too large!)
featurePlot(train[, -1], y, "scatter")

corrplot.mixed(cor(train), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

# t-Distributed Stochastic Neighbor Embedding
tsne = Rtsne(as.matrix(train), check_duplicates=FALSE, pca=TRUE, 
             perplexity=30, theta=0.5, dims=2)

embedding = as.data.frame(tsne$Y)
embedding$Class = y

ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of train data") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

## Run Boruta for feature selection
# WARNING: Super slow with this data size even with least maxRuns
# Boruta.train <- Boruta(y ~ ., data = train.label, doTrace = 1, maxRuns = 11)


### Start CV/Train
param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "binary:logistic"
  , "eval_metric" = "logloss"
  , "eta" = 0.01
  , "subsample" = 0.8
  , "colsample_bytree" = 0.8
  , "min_child_weight" = 1
  , "max_depth" = 10
)

### Run train and test
doTest <- function(y, train, test, param0, iter, nFold, isCV = FALSE) {
#   n <- nrow(train)
#   xgtrain <- xgb.DMatrix(as.matrix(train), label = y)
#   xgval = xgb.DMatrix(as.matrix(test))
#   watchlist <- list('train' = xgtrain)
#   
#   # set random seed, for reproducibility 
#   set.seed(867)
#   
#   if (isCV) {
#     # k-fold cross validation, with timing
#     system.time(bst.cv <- xgb.cv(param=param0, data=xgtrain, label = y, 
#                                  nfold = nFold, nrounds = iter, prediction = TRUE, verbose = FALSE))
#     
#     # index of minimum cv logloss
#     min.logloss.idx = which.min(bst.cv$dt[, test.logloss.mean]) 
#     #   min.logloss.idx with total iter == 1300:
#     #   1076
#     #   
#     # minimum merror
#     data.table("minIter" = min.logloss.idx, bst.cv$dt[min.logloss.idx,])
#   } else {
#     model = xgb.train(
#       nrounds = iter
#       , params = param0
#       , data = xgtrain
#       , watchlist = watchlist
#       , print.every.n = 100
#       , nthread = 8 
#     )
#     
#     p <- predict(model, xgval)
#     
#     rm(model)
#     gc()
#     
#     p
#   }
  
  n <- nrow(train)
  xgtrain <- xgb.DMatrix(as.matrix(train), label = y)
  xgval = xgb.DMatrix(as.matrix(test))
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 100
    , nthread = 8 
  )
  
  p <- predict(model, xgval)
  
  rm(model)
  gc()
  
  p
}

# Use CV to find best iter
doTest(y, train, test, param0, 1000, 5, TRUE)

#    > doTest(y, train, test, param0, 1100, 5)
#    train.logloss.mean train.logloss.std test.logloss.mean test.logloss.std
#    1:            0.30681           0.00068          0.461656         0.005307
#
#    > doTest(y, train.reduced, test, param0, 1100, 5)
#    train.logloss.mean train.logloss.std test.logloss.mean test.logloss.std
#    1:           0.306627          0.001259          0.461645         0.005245

# total analysis
submission <- read.table("data/raw/sample_submission.csv", header=TRUE, sep=',')
ensemble <- rep(0, nrow(test))

# change to 1:5 to get result
for (i in 1:5) {
  p <- doTest(y, train, test, param0, 1200, 5)
  # change to 1300 or 1200, test by trial and error, have to add to local check which suggests 900, 
  # but have another 20% training data to consider which gives longer optimal training time
  
  ensemble <- ensemble + p
}

submission$PredictedProb <- ensemble / i
write.csv(submission, "data/final/xgboost_forum_submission_V22Transformed_1200X5.csv", row.names=F, quote=F)
