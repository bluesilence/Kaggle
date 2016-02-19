setwd('D:/Kaggle/BNPParibasCardifClaimsManagement')

# Any results you write to the current directory are saved as output.
train <- read.table("data/raw/train.csv", header=T, sep=",")
train[is.na(train)] <- -1

head(train)

# Plot 2-dimensional graphs for each pair of feature 3 - 10
pairs(train[3:10])

train.allFeatures <- train[ , 3:ncol(train)]
isNumeric <- sapply(train.allFeatures, is.numeric)
train.numericColumns <- data.table("ID" = train$ID, "target" = train$target, train.allFeatures[ , isNumeric])
head(train.numericColumns)

train.nonNumericColumns <- data.table("ID" = train$ID, train.allFeatures[ , !isNumeric])
names(train.nonNumericColumns)

getOneWayANOVAFValue <- function(col, target, data) {
  results = aov(col ~ target, data = data)
  
  # Get F value of result object
  result.summary <- summary(results)
  result.summary[[1]]$`F value`[1]
}

getOneWayANOVAFValue(train.numericColumns$v1, train.numericColumns$target, train.numericColumns)


getTopKFValue <- function(data, K) {
  train.ANOVA.FValue <- data.table()
  for(i in 3:ncol(train.numericColumns)) {
    fValue <- getOneWayANOVAFValue(train.numericColumns[[i]], train.numericColumns$target, train.numericColumns)
    print(paste('Get fValue for column ', i, ': ', fValue))
    train.ANOVA.FValue <- rbind(train.ANOVA.FValue, data.table("Col" = names(train.numericColumns)[[i]], "FValue" = fValue))
  }
  
  fValues <- train.ANOVA.FValue[order(-FValue), ]
  
  if (K > 0) {
    fValues[1:K, ]
  } else {
    fValues
  }
}

train.ANOVA.FValue <- data.table()
for(i in 3:ncol(train.numericColumns)) {
  fValue <- getOneWayANOVAFValue(train.numericColumns[[i]], train.numericColumns$target, train.numericColumns)
  # print(paste('Get fValue for column ', i, ': ', fValue))
  train.ANOVA.FValue <- rbind(train.ANOVA.FValue, data.table("Col" = names(train.numericColumns)[[i]], "FValue" = fValue))
}

topFValueSignificantNumericColumns <- getTopKFValue(train.numericColumns, 100)
topFValueSignificantNumericColumns$Col

nonNumericCols <- paste(list(names(train.nonNumericColumns)[-1])[[1]], sep = "")
selectedCols <- append(nonNumericCols, topFValueSignificantNumericColumns$Col)
selectedCols.all <- append("target", append("ID", selectedCols))
selectedColsFilter <- paste(selectedCols.all, sep = "")
train.allFeatures.topFValueSignificant <- train[ , selectedColsFilter]

write.csv(train.allFeatures.topFValueSignificant, "data/intermediate/topFValues100.csv", quote=F)
