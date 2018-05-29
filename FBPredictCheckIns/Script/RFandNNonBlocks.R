#https://www.kaggle.com/apapiu/random-forest-on-a-few-blocks

setwd('D:/repos/Kaggle/FBPredictCheckIns')

library(needs)
needs(data.table, dplyr, ggplot2, ranger, plotly, tidyr, FNN, xgboost)

library(data.table)
library(dplyr)
library(ggplot2)
library(ranger)
library(plotly)
library(tidyr)
library(FNN)
library(xgboost)

fb <- fread('./RawData/train.csv', integer64 = "character")

# Pick a randome 250m X 250m square
fb <- fb %>% filter(x > 1, x < 1.25, y > 2.5, y < 2.75)
head(fb, 3)

# Extract new date/time features from time (almost certainly minutes)
one_hour <- 60
hours_daily <- 24
days_weekly <- 7
days_monthishly <- 30
days_yearly <- 365
months_yearly <- 12
one_day <- one_hour * hours_daily
one_month <- one_day * days_monthishly
one_year <- one_day * days_yearly

fb$hour = (fb$time / one_hour) %% hours_daily
fb$weekday = (fb$time / one_day) %% days_weekly
fb$month = (fb$time / one_month) %% months_yearly
fb$year = (fb$time / one_year)
fb$day = (fb$time / one_day) %% days_yearly

head(fb, 3)

# Check dist of the new date/time features
ggplot(data = fb) + geom_density(aes(x = fb$hour))
ggplot(data = fb) + geom_histogram(aes(x = fb$weekday), binwidth = 7)
# Interesting: 2 major drops, half years twice count than the other half (since it's 1.5 years)
ggplot(data = fb) + geom_density(aes(x = fb$month))
# Interesting: 2 major drops. Range is (0, 1.5)
summary(fb$year)
ggplot(data = fb) + geom_density(aes(x = fb$year))
# Interesting: 2 major drops, half years twice count than the other half (since it's 1.5 years)
ggplot(data = fb) + geom_density(aes(x = fb$day))

# Split dataset into 90% training and 10% validation set(more recent check-ins)
ten_percentiles <- quantile(fb$time, probs = seq(0, 1, 0.1))
train_val_cutoff <- ten_percentiles[10]
small_train <- fb[fb$time < train_val_cutoff, ]
small_val <- fb[fb$time >= train_val_cutoff, ]

##################EAD###################
ggplot(small_train, aes(x, y)) +
  geom_point(aes(color = place_id)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Check-ins colored by place_id")

# The clusters are pretty visible, however the place_id's are not separable
# Plot using date/time feature as the 3rd variable, only look at the most popular clusters
popular_place_ids <- small_train %>% count(place_id) %>% filter(n > 500)
popular_place_small_train <- small_train[small_train$place_id %in% popular_place_ids$place_id, ]
head(popular_place_small_train, 3)

# Plot using hour
plot_ly(popular_place_small_train, x = ~x, y = ~y, z = ~hour, color = ~place_id, type = "scatter3d", mode = "markers", marker = list(size = 5)) %>% layout(title = "Place_ids by position and Hour of Day")

# Plot using day of week
plot_ly(popular_place_small_train, x = ~x, y = ~y, z = ~weekday, color = ~place_id, type = "scatter3d", mode = "markers", marker = list(size = 5)) %>% layout(title = "Place_ids by position and Day of Week")
# It looks like weekday 2 and 3 represents Sat and Sun, when there are more check-ins

# There are still too many classes for RF to work at its best
# Remove places that have <= 3 occurences to reduce classes
length(unique(small_train$place_id))
places_more_than_3_checkins <- small_train %>% count(place_id) %>% filter(n > 3)
small_train <- small_train[small_train$place_id %in% places_more_than_3_checkins$place_id, ]
nrow(small_train)
length(unique(small_train$place_id))
       
#################KNN#################
# KNN is sensitive to the magnitude of variables
# The weights below are a combination of using a validation set (not the one here) and eye-balling
scaling_factor_y = 1/2
scaling_factor_hour = 125
scaling_factor_w = 500

create_matrix = function(train) {
  cbind(train$y / scaling_factor_y,
        train$x,
        train$hour / scaling_factor_hour,
        train$weekday / scaling_factor_w,
        train$year / scaling_factor_w,
        train$month / scaling_factor_w,
        train$time / (scaling_factor_w * one_hour * hours_daily * days_weekly)
  )
}

X = create_matrix(small_train)
X_val = create_matrix(small_val)

model_knn = FNN::knn(train = X, test = X_val, cl = small_train$place_id, k = 15)

preds <- as.character(model_knn)
truth <- as.character(small_val$place_id)
mean(truth == preds)
# That's a pretty good accuracy for a model as simple as knn

#################RF#################
set.seed(131L)
# ranger needs factors for classification
small_train$place_id <- as.factor(small_train$place_id)
model_rf <- ranger(place_id ~ x + y + accuracy + hour + weekday + month + year,
                   small_train,
                   num.trees = 100,
                   write.forest = TRUE,
                   importance = "impurity"
            )

pred_place_ids = predict(model_rf, small_val)$predictions
accuracy = mean(pred_place_ids == small_val$place_id)
accuracy
# Got an accuracy of 0.5573123

# Take a look at the predictions on the validation set
small_val$Correct = (pred_place_ids == small_val$place_id)

ggplot(small_val, aes(x, y)) + geom_point(aes(color = Correct)) + theme_minimal() + scale_color_brewer(palette = "Set1")
# It does seem that the correctly identified check-ins are more “clustered” while the wrongly identified ones are more uniformly distributed
# But other than that no clear patterns here

# Take a look at what kind of ids our RF gets wrong
# Sort the levels by counts DESC
small_val$place_id <- factor(small_val$place_id, levels = names(sort(table(small_val$place_id), decreasing = TRUE)))

# Our model is doing actually really great on the more popular ids
# However, it loses on ids that appear only a few times
small_val %>%
  ggplot(aes(x = place_id)) + geom_bar(aes(fill = Correct)) +
  theme_minimal() + theme(axis.text.x = element_blank()) +
  ggtitle("Prediction Accuracy by ID and Popularity") +
  scale_fill_brewer(palette = "Set1")

# Importance of features
data.frame(as.list(model_rf$variable.importance)) %>% gather() %>%
  ggplot(aes(x = reorder(key, value), y = value)) +
  geom_bar(stat = "identity", width = 0.6, fill = "grey") +
  coord_flip() + theme_minimal() +
  ggtitle("Variable Importance (Gini Index)") +
  theme(axis.title.y = element_blank())
# The location of a check-in should be more important than the time of the check-in
# Accuracy is important