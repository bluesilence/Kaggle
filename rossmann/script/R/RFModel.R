cat("reading the train and test data (with data.table) \n")
train <- fread("data/raw/train.csv", stringsAsFactors = T)
test  <- fread("data/raw/test.csv", stringsAsFactors = T)
store <- fread("data/raw/store.csv", stringsAsFactors = T)
train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set

## See Scripts discussion from 10/8 for more explanation.
train <- merge(train, store, by = "Store")
test <- merge(test, store, by = "Store")

cat("train data column names and details\n")
summary(train)
cat("test data column names and details\n")
summary(test)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
train[, Date := as.Date(Date)]
test[, Date := as.Date(Date)]

# seperating out the elements of the date column for the train set
train[, month := as.integer(format(Date, "%m"))]
train[, year := as.integer(format(Date, "%y"))]
train[, Store := as.factor(as.numeric(Store))]

test[, month := as.integer(format(Date, "%m"))]
test[, year := as.integer(format(Date, "%y"))]
test[, Store := as.factor(as.numeric(Store))]

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[, logSales := log1p(Sales)]

cat("saving the submission file\n")
write.csv(submission, "data/output/rf.csv", row.names = F)