filtracja2 <- function(ExampleSet, cutoff, mode) {
  
  Iqr<- apply (exprs(ExampleSet), 1, IQR)
  if (mode == "up") {
    selected <- (Iqr >  cutoff)
  } else if (mode == "down") {
    selected <- (Iqr < cutoff)
  }
  filtered<- ExampleSet[selected, ]
  save(filtered, file="ExampleSet_filtered.Rda")
  return(filtered)
}