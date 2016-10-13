library(data.table)

setwd('D:/kaggle/Bosch')

catcols <- fread('Data/Raw/train_categorical.csv', nrows = 0, skip = 0)
catnames <- names(catcols)
catclassvector <- c('integer', rep.int('character', ncol(catcols) - 1))

numcols <- fread('Data/Raw/train_numeric.csv', nrow = 0, skip = 0)
numnames <- names(numcols)
numclassvector <- c('integer', rep.int('numeric', ncol(numcols) - 1))

# define the aggregating function
getstations <- function(chunksize, s) {
  
  cats <- fread("Data/Raw/train_categorical.csv", colClasses = catclassvector, na.strings = ""
                , stringsAsFactors = FALSE, nrows = chunksize, skip = s)  
  setnames(cats, catnames)
  
  cats2 = melt(cats, 'Id', variable.name = 'feature',  variable.factor = FALSE, value.name = 'measurement')
  cats2[, measurement := gsub("T", "", measurement)]
  cats2[, measurement := as.numeric(measurement)] 
  cats2[, station := substr(feature, 1L, 6L)]
  cats2[, feature := NULL] 
  
  nums <- fread("Data/Raw/train_numeric.csv", colClasses = numclassvector, na.strings = ""
                , stringsAsFactors = FALSE, nrows = chunksize, skip = s)
  setnames(nums, numnames)
  
  resps <- nums[, .(Id, Response)] # saving this for later
  nums[, Response := NULL] 
  
  nums2 = melt(nums, 'Id', variable.name = 'feature',  variable.factor = FALSE, value.name = 'measurement')
  nums2[, station := substr(feature, 1L, 6L)]
  nums2[, feature := NULL]
  
  cats2 <- rbind(cats2, nums2)
  
  partssum <- cats2[, .(meas = mean(as.numeric(measurement), na.rm = TRUE)), by = .(station, Id)]
  
  return(partssum)
}

# loop through files and get a crude sample of aggregated data
chunksize = 10000
skip = chunksize * 10
s = 0
psample <- data.table(station = character(), Id = integer(), meas = numeric())

p <- NULL
for (i in 1:11) {
  p <- getstations(chunksize, s)
  psample <- rbind(psample, p)
  s = s + skip
  
  cat(i)
}

rm(p)

#re-reshape and clean up
psample =  dcast(psample, Id ~ station, value.var = "meas")
setnames(psample, c("L0_S0_", "L0_S1_", "L0_S2_", "L0_S3_", "L0_S4_", "L0_S5_"
                    , "L0_S6_" , "L0_S7_", "L0_S8_", "L0_S9_")
         , c("L0_S00", "L0_S01", "L0_S02", "L0_S03", "L0_S04", "L0_S05" 
             , "L0_S06", "L0_S07", "L0_S08", "L0_S09")
)

pnames <- sort(names(psample))
setcolorder(psample, pnames)

###################################
##### produce the Visualization ###
###################################

setDF(psample)
library(VIM)

png(filename="Plots\flowpaths.png",  # use this device for scalable, high-res graphics
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

# show the data by volume
miceplot <- aggr(psample[, -c(1)], col=c("dodgerblue","lightgray"),
                 numbers=TRUE, combined=TRUE, varheight=TRUE, border=NA,
                 sortVars=FALSE, sortCombs=FALSE, ylabs=c("Product Families"),
                 labels=names(psample[, 1]), cex.axis=.7)
dev.off()


#####################################
#### grab unique flow paths ####       
#####################################

library(tidyr)
setDT(psample)

# convert everything to 1s and 0s
Id <- psample[, Id]
psample[!is.na(psample)] <- 1
psample[is.na(psample)] <- 0
psample <- cbind(Id, psample[, 2:53, with = FALSE])

# concatenate the 1s and 0s
paths <- unite(psample, path, L0_S00:L3_S51, sep = "", remove = TRUE)

# count the 1s
stations <- (paths$path)
g2 <- sapply(regmatches(stations, gregexpr("1", stations)), length)
paths[, stationcount := g2]

# aggregate by path
flowpaths <- paths[, .(pct = .N/nrow(paths)), by = path]

# fwrite(flowpaths, "flowpathsample.csv")
write.csv(flowpaths, "Data/Intermediate/flowpathsample.csv", quote = FALSE, row.names = FALSE)
