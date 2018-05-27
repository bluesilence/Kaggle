#https://www.kaggle.com/msjgriffiths/exploratory-data-analysis/code

setwd('D:/repos/Kaggle/FBPredictCheckIns')

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

################# Accuracy ####################
train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = accuracy)) +
  geom_density()

train %>%
  sample_frac(.01) %>%
  ggplot(aes(x, y, z = accuracy)) +
  stat_summary_2d(fun = mean, bins = 50) +
  scale_fill_viridis()

train %>%
  filter(accuracy > 200) %>%
  sample_frac(0.05) %>%
  ggplot(aes(x, y)) +
  geom_bin2d(bins = 50) + 
  scale_fill_viridis() +
  lims(x = c(0, 10), y = c(0, 10))

################# Time ####################
train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = time)) +
  geom_density()

train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = x, y = time)) +
  geom_bin2d(bins = 50)

train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = y, y = time)) +
  geom_bin2d(bins = 50)

################# Place ID ####################
train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = place_id)) +
  geom_density()

print(sprintf("Correlation of place_id and x: %s", cor(train$place_id, train$x)))
print(sprintf("Correlation of place_id and y: %s", cor(train$place_id, train$y)))

# New trick learnt: checking the dist of freq of some feature
place_ids <-
  train %>%
  sample_frac(0.05) %>%
  group_by(place_id) %>%
  summarise(freq = n())

# It looks like a Poisson dist
place_ids %>%
  ggplot(aes(x = freq)) +
  geom_density()

fitted_distr <- fitdistr(place_ids$freq, "Poisson")
print(sprintf("Log Likelihood: %s", fitted_distr$loglik))
print(sprintf("Estimated Poisson mean: %s", fitted_distr$estimate[[1]]))
pois_samples <- rpois(nrow(place_ids), fitted_distr$estimate[[1]])

# Obviously, it's not Poisson dist
place_ids %>%
  mutate(simulated = pois_samples) %>%
  ggplot(aes(x = freq)) +
  geom_density() +
  geom_density(aes(x = simulated), colour = "red")

# Is it a Cauchy dist?
fitted_distr <- fitdistr(place_ids$freq, "cauchy")
print(sprintf("Log Likelihood: %s", fitted_distr$loglik))
print(sprintf("Estimated Cauchy location: %s, scale: %s", fitted_distr$estimate[[1]], fitted_distr$estimate[[2]]))

cauchy_samples <- rcauchy(nrow(place_ids), fitted_distr$estimate[[1]], fitted_distr$estimate[[2]])

place_ids %>%
  mutate(simulated = cauchy_samples) %>%
  ggplot(aes(x = freq)) +
  geom_density() +
  geom_density(aes(x = simulated), colour = "red") +
  xlim(0, 100)

# Take a handful (10 places) that have a frequency of more than 10 (in our sample) and find the x and y parameters for each
place_ids %>%
  filter(freq > 10) %>%
  top_n(n = 10, wt = freq) %>%
  inner_join(train %>% sample_frac(0.05)) ->
  places

places %>%
  ggplot(aes(x, y, colour = as.factor(place_id))) +
  geom_point() +
  lims(x = c(0, 10), y = c(0, 10))

# How does accuracy play into this?
places %>%
  ggplot(aes(x, y, colour = as.factor(place_id))) +
  geom_point(aes(size = 1 / accuracy), alpha = 0.6) +
  lims(x = c(0, 10), y = c(0, 10))

# much more variation in x than y
places %>%
  group_by(place_id) %>%
  summarise(mean_x = mean(x), sd_x = sd(x), mean_y = mean(y), sd_y = sd(y)) %>%
  arrange(desc(sd_x)) %>%
  mutate_at(vars(ends_with("x")), funs(comma)) %>%
  mutate_at(vars(ends_with("y")), funs(comma)) %>%
  formattable(
    list(sd_x = color_bar("orange", fun = "proportion"),
         sd_y = color_bar("pink", fun = "proportion")),
    align = 'l'
  )