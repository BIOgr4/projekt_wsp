
Funkcja = function(ExampleSetExp,ilosc_Kl){
  
  X=ExampleSetExp
  X2=log(X)
  
  # Principal Component Analysis 
  PCA=prcomp(X, cor = FALSE, scores = TRUE)  #sortowanie danych wdl. malej?cych wariancji
  PCA2=prcomp(X2, cor = FALSE, scores = TRUE)
  
  # Skalowanie wielowymiarowe
  manDist=dist(X)
  cmds = cmdscale(manDist)
  manDist2=dist(X2)
  cmds2 = cmdscale(manDist2)

  #klasteryzacja K-Means
  kl=kmeans(X,ilosc_Kl)$cluster
  kl2=kmeans(X2,ilosc_Kl)$cluster
  
  #plotowanie 
  par(mfrow = c(2, 2))
  plot(PCA$x[,1], PCA$x[,2], main="Dane wyjsciowe", col=kl)
  plot(PCA2$x[,1], PCA2$x[,2], main="Dane zlogarytmowane", col=kl2)
  plot(cmds[,1], cmds[,2], main="Dane wyjsciowe", col=kl)
  plot(cmds2[,1], cmds2[,2], main="Dane zlogarytmowane", col=kl2)
  title("K-Means",outer=TRUE)
   
}

Cluster_function <- function(ExampleSet, ilosc_Kl){
  
  data <- ExampleSet
  cl <- labels(data)[[1]]
  d <- dist(data)
  par(mfrow = c(1,2))
  image(as.matrix(d))
  hc <- hclust(d, method = 'complete')
  plot(hc, labels = cl)
  groups <- cutree(hc, k = ilosc_Kl)
  result <- as.matrix(groups, labels(cl))
  
  return(result)
}


