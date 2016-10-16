library(data.table)

# works for both a single number and vectors
mcc <- function(TP, FP, FN, TN)
{
  num <- (TP*TN) - (FP*FN)
  den <- (TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)
  
  mccs <- num / sqrt(den)
  if (length(mccs) > 1)
  {
    mccs[!is.finite(mccs)] <- 0
  }
  else
  {
    if (!is.finite(mccs))
      mccs <- 0
  }
  
  return(mccs)
}

eval_mcc <- function(y_true, y_prob, plot=T)
{
  idx_order <- order(y_prob)
  y_true_order <- y_true[idx_order]
  y_prob_order <- y_prob[idx_order]
  
  n <- length(y_true)
  best_mcc <- 0
  best_cutoff <- -1
  
  total_p <- sum(y_true)
  total_n <- n - total_p
  
  tn <- cumsum(y_true_order == 0)
  fp <- cumsum(y_true_order == 1)
  fn <- total_n - tn
  tp <- total_p - fp
  
  mccs <- mcc(TP = tp, FP = fp, FN = fn, TN = tn)
  
  best_idx <- which.max(mccs)
  best_mcc <- mccs[best_idx]
  best_cutoff <- y_prob_order[best_idx]
  
  if (plot) { plot(mccs) }
  
  cat("best cutoff: ", best_cutoff, "\n")
  cat("best mcc: ", best_mcc, "\n")
  
  return(data.table('Best_MCC' = best_mcc, 'Best_Cutoff' = best_cutoff))
}

set.seed(235)

y_prob0 <- runif(10000, 0, 1)
y_prob <- y_prob0 + 0.5 * runif(10000, 0, 1) - 0.02

y_true <- ifelse(y_prob0 > 0.6, 1, 0)

# Example of a vector
eval_mcc(y_true, y_prob)

# Example of a single number
mcc(10, 2, 3, 5)
