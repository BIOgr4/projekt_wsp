setwd("C:/Users/Monika/Desktop/studia/I/wsp/laborkiii")

source("https://bioconductor.org/biocLite.R")
biocLite("randomForest")
biocLite("golubEsets")
biocLite("CMA")
library(CMA)
library(randomForest)
library(pathClass)
library(golubEsets)
library(Biobase)

Random_Forest <- function(filtered){
  #load data
  x <- t(exprs(filtered))
  y <- factor(pData(filtered)$CLASS)

  #learning
  set.seed(111)
  learn <- sample(length(y), size=length(y))

  #Random Forest
  rfresult <- rfCMA(x, y, learnind=learnind, varimp = FALSE)

  #results
  show(result)
  ftable(result)
  res<-plot(result)
  title("Classification based on Random Forests")

  return(res)
}