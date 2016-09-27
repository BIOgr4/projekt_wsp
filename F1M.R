#działamy wywołując poszczególne funkcje zwracające nam głównie wyniki w postaci różnego rozaju wykresów
#zmodyfikowane w taki sposób by dało się wybrac zakres "data" jaki nas interesuje w prezentacji wyników
# poniżej stosowane biblioteki 

#library("ALL")
library("affyPLM")
library("affy")
library("hexbin")
library("geneplotter")
library("RColorBrewer")
library("limma")


MVA <-function(X,zakres,skala){
  if (isTRUE(skala)) mva.pairs(X[,zakres])
  
  else if (!isTRUE(skala)) mva.pairs(X[,zakres],log.it=FALSE)
}

rM <- function(X,zakres,parametr){ #4 wykresy
  #rM - HIST
  rM <- apply(X[,zakres],1,mad)
  par(mfrow=c(2,2))
  hist(rM)
  
  rM2 <- X[rM>parametr,zakres]
  hist(rM2)
}

HB <- function(X,zakres,skala){
  ## HEXBIN
  if (isTRUE(skala)) {
    hb<-hexbin(log2(X)[,zakres],xbins=50)
  } else if (!isTRUE(skala)) {
    hb<-hexbin(X[,zakres],xbins=50)
  }
  plot(hb, colramp=colorRampPalette(brewer.pal(9, "YlGnBu")[-1]))
}

tabelaklas<- function(x){
  #PODZIA? ZE WZGL?DU NA KLASY PR?B
  table(pData(x)$CLASS) #podzia? ze wzgl?du na klasy
}


BOXY <- function(X,zakres,skala){
  #BOXPLOT-y
  par(mfrow=c(2,1))
  if (isTRUE(skala)) {
    y<-log2(exprs(X)[,zakres])}
  else if (!isTRUE(skala)) {
    y<-exprs(X)[,zakres]
  }
  boxplot(y, col=as.numeric(pData(X)$CLASS)+1)
 
}

SCATTER <-function(X,zakres,skala){
  #SCATTERPLOT-y
  
  par(mfrow=c(3,1))
  if (isTRUE(skala)) {
    y<-log2(X[,zakres])
  } else if (!isTRUE(skala)){
    y<-X[,zakres]
  }
  plot(y, pch = ".", main = "x")
  hist(y)
}

HM<- function(X,zakres,order){
  # heatmap
  rsd <- apply(X, 1, sd)
  
  if (isTRUE(order)) {
    sel <- order(rsd, decreasing = TRUE)[1:100]
  } else if (!isTRUE(order)) {
    sel <- order(rsd, decreasing = FALSE)[1:100] 
  }
  heatmap(X[sel,zakres], col=topo.colors(100))
}


DEN <- function(x,zakres,skala){
  #density plot
  df <- data.frame(Expression=as.vector(exprs(x)), Sample=sampleNames(x)[col(x)])
  if (isTRUE(scale)) {
    EXP<-log2(Expression)
  } else if (!isTRUE(scale)) {
    EXP<-Expression
  }
  densityplot(~EXP|Sample, df, plot.points=FALSE, ylab = "density", xlab="intensity")
  legend(c(Sample))
}


MAPLOTS <- function(x,zakres, skala){
  #Create MA plots using a reference array 
  par(mfrow=c(4,1))
  if (isTRUE(skala)) {
    MAplot(x[,zakres],pch=".",logMode=T,pairs=TRUE)
  } else if (!isTRUE(skala)) {
    MAplot(x[,zakres],pch=".",logMode=F,pairs=TRUE)
  }
  return(MAplot(x,plot.method="smoothScatter"))
}


