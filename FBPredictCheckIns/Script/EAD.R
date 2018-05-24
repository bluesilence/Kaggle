#https://www.kaggle.com/msjgriffiths/exploratory-data-analysis/code

setwd('D:/Kaggle/FBPredictCheckIns')

library(needs)
needs(dplyr, tidyr, stringr, lubridate, readr, ggplot2, MASS, pander, formattable, viridis)

list.files('./RawData') %>% as.list %>% pander

train <- read_csv('./RawData/train.csv')
glimpse(train)
N <- nrow(train)

# row_id seems to be … a row ID. It is TRUE that the number of unique row_ids is the same as the number of rows in the data frame.
N_unique_row_id <- length(unique(train$row_id))
N == N_unique_row_id

# x is presumably bounded between [0, 10] as the x-axis on the 10-km square.
summary(train$x)

# y looks to be the same as x, just the other dimension.
summary(train$y)

# accuracy is intersting: it’s all over the place. The smallest value is 1.00; the biggest value is 1,033.00. We’ll have to look into that.
summary(train$accuracy)

# time has no units. Since Facebook notes that time an accuracy are "intentionally left vague in their definitions.", we will have to look into that.
summary(train$time)

# place_id is probably a unique identifier. There 108390 unique values.
N_unique_place_id <- length(unique(train$place_id))
N / N_unique_place_id
