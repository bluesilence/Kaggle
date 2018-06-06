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

############################### Step 3 #######################################

############################### Step 4 #######################################

############################### Step 5 #######################################