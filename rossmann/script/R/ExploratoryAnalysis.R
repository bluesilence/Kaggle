cat("reading the train and test data (with data.table) \n")
train <- fread("data/raw/train.csv", stringsAsFactors = T)
test  <- fread("data/raw/test.csv", stringsAsFactors = T)
store <- fread("data/raw/store.csv", stringsAsFactors = T)
train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set

# Display the internal structure of an R object
str(train)
str(test)
str(store)

head(train); tail(train)
head(test); tail(test)

train[, Date := as.Date(Date)]
test[, Date := as.Date(Date)]
store

train <- train[order(Date)]
test <- test[order(Date)]

summary(train)
summary(test)

test[is.na(test$Open), ] # Only store 622
test$Open[test$Store == 622]

# Assume the store is open since 0 sale won't count in lb
test[is.na(test$Open)]$Open <- 1

# Unique values per column
train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]

# Are there test stores not in the train data? No.
sum(unique(test$Store) %in% unique(train$Store))

# How many train stores are there not in the test data?
sum(!(unique(train$Store) %in% unique(test$Store)))

# Percent Open in train data
table(train$Open) / nrow(train)

# Percent Open in test data
table(test$Open) / nrow(test)

# Percent Promo in train data
table(train$Promo) / nrow(train)

# Percent Promo in test data
table(test$Promo) / nrow(test)

# Percent Holiday in train data
table(train$StateHoliday) / nrow(train)

# Percent Holiday in test data
table(test$StateHoliday) / nrow(test) # no b and c = no easter holiday and no christmas

# Percent School Holiday in train data
table(train$SchoolHoliday) / nrow(train)

# Percent School Holiday in test data
table(test$SchoolHoliday) / nrow(test)

plot(train$Date, type = "l")
plot(test$Date, type = "l")

# As expected all 856 stores to be predicted daily
all(table(test$Date) == 856)

hist(train$Sales, 100)
hist(aggregate(train$Sales, by = list(train$Store), mean)$x, 100, main = "Mean sales per store when store's sale > 0")

hist(train$Customers, 100)
hist(aggregate(train$Customers, by = list(train$Store), mean)$x, 100, main = "Mean customers per store when store's sale > 0")

ggplot(train, aes(x = factor(SchoolHoliday), y = Sales)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(train, aes(x = log(Customers), y = log(Sales))) + geom_point(alpha = 0.2) + geom_smooth()

ggplot(train, aes(x = factor(Promo), y = Sales)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
ggplot(train, aes(x = factor(Promo), y = Customers)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

with(train[train$Promo == 0], mean(Sales / Customers))
with(train[train$Promo == 1], mean(Sales / Customers))

table(ifelse(train$Promo, "Promo", "No promo"))

plot(train[Store == 972, Sales], ylab = "Sales", xlab = "Days", main = "Store 972")
plot(train[Store == 103, Sales], ylab = "Sales", xlab = "Days", main = "Store 103")
plot(train[Store == 708, Sales], ylab = "Sales", xlab = "Days", main = "Store 708")

ggplot(train[Store == 85], aes(x = Date, y = Sales, color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + geom_point(size = 3) + ggtitle("Sales of store 85 (True if sunday)")
ggplot(train[Store == 262], aes(x = Date, y = Sales, color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + geom_point(size = 3) + ggtitle("Sales of store 262 (True if sunday)")

ggplot(train, aes(x = factor(DayOfWeek), y = Sales)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)


summary(store)
table(store$StoreType)
table(store$Assortment)

# Only StoreType b has Assortment b
table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))


hist(store$CompetitionDistance, 100)

store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, store$CompetitionOpenSinceMonth, sep = "-"))
# One competitor opened 1900
hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, main = "Years since opening of nearest competition")

# Convert the Promo2Since... variables to one Date variable
# Assume that the promo starts on the first day of the week
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, store$Promo2SinceWeek, 1, sep = "-"), format = "%Y-%U-%u")
hist(as.numeric(as.POSIXct("2015-10-01", format = "%Y-%m-%d") - store$Promo2Since), 100, main = "Days since start of promo2")

table(store$PromoInterval)

# Merge store and train 
train_store <- merge(train, store, by = "Store")
ggplot(train_store, aes(x = factor(PromoInterval), y = Sales)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(train_store, aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + geom_smooth(size = 2)
ggplot(train_store, aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + geom_smooth(size = 2)

ggplot(train_store, aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + geom_smooth(size = 2)
ggplot(train_store, aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + geom_smooth(size = 2)

salesByDist <- aggregate(train_store[!is.na(CompetitionDistance)]$Sales, by = list(train_store[!is.na(CompetitionDistance)]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + geom_point() + geom_smooth()

ggplot(train_store, aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) + geom_jitter(alpha = 0.1) + geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) + ggtitle("Any competition?")

# Sales before and after competition opens
train_store$DateYearMon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]
timespan <- 100 # Days to collect before and after Opening of competition

beforeAndAfterComp <- function(s) {
  x <- train_store[Store == s]
  daysWithComp <- x$CompetitionOpenSince >= x$DateYearMon
  
  if (any(!daysWithComp)) {
    compOpening <- head(which(!daysWithComp), 1) - 1
    
    if (compOpening > timespan & (compOpening + timespan) < nrow(x)) { # if the store has data for at least 100 days before & after the first day competition is opened
      x <- x[(compOpening - timespan):(compOpening + timespan), ]
      x$Day <- 1:nrow(x)
      
      return(x)
    }
  }
}

temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)
temp <- do.call(rbind, temp)
# 132 stores first had no competition but at least 100 days before the end
# of the data set
length(unique(temp$Store))

ggplot(temp, aes(x = Day, y = Sales)) + geom_smooth() + ggtitle(paste("Competition opening around day", timespan))
