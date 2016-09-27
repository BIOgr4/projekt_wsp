
Funkcja = function(ExampleSet,iloœæ_Kl){
  
  Dane=exprs(ExampleSet)
  X=Dane[1:4,1:4]  # sa tylko 4 pliki CEL w ExpressionSet
  X2=log(X)
  
  # Principal Component Analysis 
  PCA=prcomp(X, cor = FALSE, scores = TRUE)  #sortowanie danych wdl. malej¹cych wariancji
  PCA2=prcomp(X2, cor = FALSE, scores = TRUE)
  
  # Skalowanie wielowymiarowe
  manDist=dist(X)
  cmds = cmdscale(manDist)
  manDist2=dist(X2)
  cmds2 = cmdscale(manDist2)

  #klasteryzacja K-Means
  kl=kmeans(X,iloœæ_Kl)$cluster
  kl2=kmeans(X2,iloœæ_Kl)$cluster
  
  #plotowanie 
  oldpar <- par(mfrow = c(1, 2))
  x11();par(las=1, cex=0.7, mfcol=c(2,2), oma=c(0,0,0,0))
  plot(PCA$x[,1], PCA$x[,2], main="Dane wyjsciowe", col=kl)
  plot(PCA2$x[,1], PCA2$x[,2], main="Dane zlogarytmowane", col=kl2)
  plot(cmds[,1], cmds[,2], main="Dane wyjsciowe", col=kl)
  plot(cmds2[,1], cmds2[,2], main="Dane zlogarytmowane", col=kl2)
  title("K-Means",outer=TRUE)
  par(oldpar)  
}

