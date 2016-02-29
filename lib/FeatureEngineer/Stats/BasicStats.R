## Check basic stats on each feature: nr. of 0's, nr. of NAs, max, min, mean, std, distinct values
calcBasicStats <- function(col, na.Value) {
  nrs <- length(col)
  if (nrs == 0) {
    data.table(c())
  } else {
    numbers.table <- as.data.frame(table(col))
    nr0s <- numbers.table[col == 0, "Freq"][1]
    nrNAs <- numbers.table[col == na.Value, "Freq"][1]
    
    numbers.table.removeNAs <- numbers.table[numbers.table$col != na.Value, ]
    numbers.table.removeNAs <- numbers.table.removeNAs[!is.na(numbers.table.removeNAs$Freq), ]
    numbers.table.removeNAs <- numbers.table.removeNAs[order(numbers.table.removeNAs$col), ]
    
    col.removeNAs <- col[which(col != na.Value)]
    nrNonNAs <- length(col.removeNAs)
    
    nDistincts <- nrow(numbers.table.removeNAs)
    distinctRatio <- nDistincts / nrNonNAs
    
    maxValue <- numbers.table.removeNAs[nrow(numbers.table.removeNAs), "col"]
    minValue <- numbers.table.removeNAs[1, "col"]
    meanValue <- mean(col.removeNAs)
    stdValue <- sd(col.removeNAs)
    
    data.table(
                "nrows" = nrs,
                "nr0s" = nr0s,
                "nrNAs" = nrNAs,
                "nrNonNAs" = nrNonNAs,
                "nDistincts" = nDistincts,
                "distinctRatio" = distinctRatio,
                "max" = maxValue,
                "min" = minValue,
                "mean" = meanValue,
                "std" = stdValue
              )
  }
}

## Generate feature stats for all columns in a data table
collectFeatureStats <- function(dt) {
  all.feature.stats <- data.table()
  ncols <- ncol(dt)
  
  # Assume 1st column is ID
  for (i in 2:ncols) {
    feature.col <- dt[[i]]
    
    feature.stats <- cbind("Col" = names(dt)[i], calcBasicStats(as.vector(as.matrix(feature.col)), -1))
    print(feature.stats)
    
    all.feature.stats <- rbind(all.feature.stats, feature.stats)
  }
  
  all.feature.stats
}
