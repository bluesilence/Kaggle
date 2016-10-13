library(data.table)
library(Matrix)
library(caret)
library(xgboost)

setwd('D:/kaggle/Bosch')
dt <- fread('Data/Raw/train_numeric.csv', drop = 'Id', nrows = 200000)

Y <- dt$Response
dt[, Response := NULL]

for (col in names(dt))
  set(dt, j = col, value = dt[[col]] + 2)

for (col in names(dt))
  set(dt, which(is.na(dt[[col]])), col, 0)

X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)

folds <- createFolds(as.factor(Y), k = 6)
valid = folds$Fold1
model <- c(1:length(Y))[-valid]

param <- list(objective = 'binary:logistic',
              eval_metric = 'auc',
              eta = 0.01,
              base_score = 0.005,
              col_sample = 0.5)

dmodel <- xgb.DMatrix(X[model, ], label = Y[model])
dvalid <- xgb.DMatrix(X[valid, ], label = Y[valid])

m1 <- xgb.train(data = dmodel, param, nrounds = 20,
                watchlist = list(mod = dmodel, val = dvalid))

imp <- xgb.importance(model = m1, feature_names = colnames(X))
cols <- imp$Feature
length(cols)

head(cols, 10)

rm(list = setdiff(ls(), "cols"))





dt <- fread('Data/Raw/train_numeric.csv', 
            select = c(cols, "Response"),
            showProgress = TRUE)

Y <- dt$Response
dt[ , Response := NULL]

for (col in names(dt))
  set(dt, j = col, value = dt[[col]] + 2)

for (col in names(dt))
  set(dt, which(is.na(dt[[col]])), col, 0)

X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)

