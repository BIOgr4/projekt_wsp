Random_Forest <- function(filtered) {
  #load data
  x <- t(exprs(filtered))
  y <- factor(pData(filtered)$CLASS)

  #learning
  set.seed(111)
  learn <- sample(length(y), size=length(y))

  #Random Forest
  result <- rfCMA(x, y, learnind=learn, varimp = FALSE)

  #results
  show(result)
  ftable(result)
  plot(result)
  title("Classification based on Random Forests")
  
}