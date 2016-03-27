submission.good <- ensemble.avg('D:/Kaggle/BNPParibasCardifClaimsManagement/data/ensemble/Input/Good')
submission.bad <- ensemble.avg('D:/Kaggle/BNPParibasCardifClaimsManagement/data/ensemble/Input/Bad')
submission.weighted <- merge(submission.good, submission.bad, by = "ID", all = TRUE)
# 7 good models and 4 bad models, give good model 3 times weight against bad model
modelCount.good <- 7
modelCount.bad <- 4
modelWeight.good <- 3
modelWeight.bad <- 1
submission.weighted$PredictedProb <- with(submission.weighted, (modelWeight.good * modelCount.good * PredictedProb.x + modelWeight.bad * modelCount.bad * PredictedProb.y) / (modelCount.good + modelCount.bad))
submission <- submission.weighted[ , c(1, ncol(submission.weighted))]
head(submission)

time <- format(Sys.time(),"%Y%m%dT%H%M%S")

write.csv(
  submission.good,
  paste("../../final/xgboost_ensemble_11_avg_weighted",
        "_",
        time,
        ".csv",sep=""),
  row.names=F)

ensemble.avg <- function(folder) {
  originalWd <- getwd()
  print(paste("Changing wd from ", originalWd, " to ", folder))
  setwd(folder)
  
  # Start merging
  preds <- data.table()
  modelFiles <- list.files(folder, recursive = TRUE)
  i <- 0
  for (file in modelFiles) {
    print(paste("Read model file", file))
    pred <- read.table(file, header = T, sep = ",")
    
    i <- i + 1
    if (nrow(preds) == 0) {
      preds <- pred
      names(preds) <- c(names(pred)[1], paste("Pred", i))
    } else {
      preds <- merge(x = preds, y = pred, by = names(preds)[1], all = TRUE)
      names(preds)[length(names(preds))] <- paste("Pred", i)
    }
    
    print(paste("Merged model", i))
  }
  
  preds.merged <- rep(0, nrow(preds))
  for (j in 2:ncol(preds)) {
    preds.merged <- preds.merged + preds[ , j]
  }
  
  preds.merged <- preds.merged / i
  
  submission <- data.frame(ID = preds[ , 1], PredictedProb = preds.merged)
  submission <- submission[order(submission$ID), ]
  print(head(submission))
  
  # Revert to original working directory
  setwd(originalWd)
  
  submission
}