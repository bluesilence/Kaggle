library(data.table)

mcc <- function(TP, FP, FN, TN)
{
  num <- (TP*TN) - (FP*FN)
  den <- (TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)
  
  if (den == 0)
  {
    return(0)
  }else
  {
    return(num / sqrt(den))
  }
}

eval_mcc <- function(y_true, y_prob, plot=T)
{
  y_true_order <- y_true[order(y_prob)]
  
  n <- length(y_true)
  best_mcc <- 0
  best_cutoff <- -1
  mccs <- c()
  
  for (i in 1:(n-1))
  {
    tp <- as.numeric(sum(y_true_order[(i+1):n]))
    fp <- n - i - tp
    fn <- as.numeric(sum(y_true_order[1:i]))
    tn <- i - fn
    
    new_mcc <- mcc(tp, fp, fn, tn)
    mccs[i] <- new_mcc
    
    if (new_mcc >= best_mcc)
    {
      best_mcc <- new_mcc
      best_cutoff <- y_prob[order(y_prob)[i]]
    }
  }
  
  if (plot) { plot(mccs) }
  
  cat("best cutoff: ", best_cutoff, "\n")
  
  return(best_mcc)
}

set.seed(235)

y_prob0 <- runif(10000, 0, 1)
y_prob <- y_prob0 + 0.5 * runif(10000, 0, 1) - 0.02

y_true <- ifelse(y_prob0 > 0.6, 1, 0)

eval_mcc(y_true, y_prob)
