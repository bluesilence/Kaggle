#https://www.kaggle.com/rsakata/3rd-place-solution-simple-version

setwd('D:/repos/Kaggle/FBPredictCheckIns')

library(needs)
needs(data.table, dplyr, xgboost)

data.train <- fread('./RawData/train.csv', colClasses = list(character = 'place_id'))
data.test <- fread('./RawData/test.csv')

train.end <- max(data.train$time)
test.end <- max(data.test$time)

############################### Step 1: Get Candidate place_ids within each grid #######################################
cat('step: 1/5\n')

num.x1 <- 100
num.y1 <- 200

# Discretize and rescale x and y into grids
temp <- data.train[, .(place_id, new.x1 = as.integer(floor(x/10 * num.x1)), new.y1 = as.integer(floor(y/10 * num.y1)))]
# Count row # in each small grid
temp <- temp[, .(count = .N), by = c('new.x1', 'new.y1', 'place_id')]

# Sort by # check-ins in each grid
temp <- temp[order(new.x1, new.y1, desc(count))]
# The place_id with largest count in each grid gets ranked 1, other place_ids get 0
temp$rank <- ifelse(duplicated(paste(temp$new.x1, temp$new.y1)), 0, 1)

# Select place_ids with at least 2 check-ins
temp <- temp[count >= 2]
# Select the grid with the most distinct place_ids
n.candidate <- max(temp[, list(count = .N), by = c('new.x1', 'new.y1')]$count)

i <- 2
# While there are still rows not ranked
while(sum(temp$rank == 0) > 0)
{
  # Update ranks from the 2nd ranked place_id through to the last ranked
  # c(0, temp$rank[-nrow(temp)]) is a right shift, to see if position (i-1) has been ranked (i-1) and current position hasn't been ranked
  temp$rank <- ifelse(temp$rank == 0 & c(0, temp$rank[-nrow(temp)]) == i-1, i, temp$rank)
  i <- i+1
}

# Select the most visited place_id within each grid
candidate <- temp[rank == 1, .(new.x1, new.y1, place_id)]
for (i in 2:n.candidate)
{
  # Flatten all the place_ids into 1 row per grid, order by check-in count DESC
  candidate <- merge(candidate, temp[rank == i, 1:3, with = FALSE], by = c('new.x1', 'new.y1'), all.x = TRUE, suffixes = c('', paste0('_', i)))
}

colnames(candidate)[3:ncol(candidate)] <- paste0('candidate_', 1:n.candidate)
write.csv(candidate, './Output/candidate.csv', row.names = FALSE)

rm(temp)

############################### Step 2 #######################################
cat("step: 2/5\n")

predict.interval <- 24 * 60 * 7 * 2 # 2 weeks

# Count visits per place per interval
freq <- data.table(place_id = unique(data.train$place_id))
for (i in 1:(train.end / predict.interval))
{
  temp <- data.train[train.end - predict.interval*i < time & time <= train.end - predict.interval*(i - 1), .N, by = "place_id"]
  freq <- merge(freq, temp, by = "place_id", all.x = TRUE)
  colnames(freq)[ncol(freq)] <- paste0("freq_", as.integer(train.end - predict.interval*i), "-", as.integer(train.end - predict.interval*(i-1)))
}
freq[is.na(freq)] <- 0
head(freq)

params <- list("eta" = 0.1, "max_depth" = 6, "min_child_weight" = 100, "objective" = "reg:linear", "eval_metric" = "rmse")

pr.place.table <- freq[, .(place_id)]

total_epoch <- ceiling((test.end - train.end) / predict.interval)
for (i in 1:total_epoch)
{
  cat(paste0("    substep: ", i, "/", total_epoch, "\n"))
  
  x.train <- freq[, (i+2):ncol(freq), with = FALSE] # Train on place with visits in all the history except for the latest i intervals
  y.train <- freq[[2]][apply(x.train, 1, sum) > 0] # Prediction target is the last interval & Filter out intervals without visits
  x.train <- x.train[apply(x.train, 1, sum) > 0]
  
  x.test <- freq[, 2:(ncol(freq) - i), with = FALSE] # Test on all the history except for the earliest i intervals
  
  x.train <- as.matrix(x.train) * 1.0
  x.test <- as.matrix(x.test) * 1.0
  
  if (i == 1)
  {
    nrounds <- 100
  }
  else if (i == 2)
  {
    nrounds <- 30
  }
  else
  {
    nrounds <- 10
  }
  
  set.seed(0)
  model.xgb <- xgb.train(param = params, data = xgb.DMatrix(x.train, label = y.train), nrounds = nrounds)
  # xgb requires test data to have the same feature names as the training data
  colnames(x.test) <- model.xgb[["feature_names"]]
  
  temp <- predict(model.xgb, x.test)
  temp[temp < 0] <- 0
  temp <- temp / sum(temp)
  pr.place.table <- cbind(pr.place.table, temp)
  colnames(pr.place.table)[ncol(pr.place.table)] <- paste0("pr_", as.integer(train.end + predict.interval*(i - 1)), "-", as.integer(train.end + predict.interval*i))
}

write.csv(pr.place.table, "./Output/pr_place.csv", row.names = FALSE)

rm(freq)
rm(x.train)
rm(x.test)
rm(temp)

############################### Step 3 #######################################

############################### Step 4 #######################################

############################### Step 5 #######################################