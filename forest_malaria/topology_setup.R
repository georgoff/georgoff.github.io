number_forests <- 7

distribute_travel <- function(number_forests) {
  results <- vector("numeric", length = number_forests)
  
  for (i in 1:number_forests) {
    if (i == 1) {
      max = 1
    }
    else {
      max = results[i-1]
    }
    
    cat("max = ", max, "\n")
    
    results[i] <- runif(1, min = 0, max = max) * (max)
  }
  
  return(results)
}
