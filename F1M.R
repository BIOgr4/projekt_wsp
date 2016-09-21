x<-exampleSet #wczytanie ExpressionSet
X<-exprs(x)

#działamy wywołując poszczególne funkcje zwracające nam głównie wyniki w postaci różnego rozaju wykresów
#zmodyfikowane w taki sposób by dało się wybrac zakres "data" jaki nas interesuje w prezentacji wyników
# poniżej stosowane biblioteki 

library("ALL")
library("affyPLM")
library("affy")
library("hexbin")
library("geneplotter")
library("RColorBrewer")
library("limma")


F1M <- function(x){
  show(x)
  dim(exprs(x))
  names(pData(x))
  return(x)
}


MVA <-function(X,zakres){
  #MVA PLOTS
  name=paste("MVA",zakres,".jpg",sep="")
  jpeg(name)
  mva.pairs(X[,zakres])
  dev.off()
    name=paste("MVAf",zakres,".jpg",sep="")
  jpeg(name)
  mva.pairs(X[,zakres],log.it=FALSE)
  dev.off()
}

rM <- function(X,zakres,parametr){
#rM - HIST
rM <- apply(X[,zakres],1,mad)
name=paste("rM",zakres,".jpg",sep="")
jpeg(name)
hist(rM)
dev.off()
name=paste("rM",zakres,parametr,".jpg",sep="")
jpeg(name)
rM2 <- X[rM>parametr,]
dim(rM2)
hist(rM2)
dev.off()
}

HB <- function(X,zakres){
## HEXBIN
name=paste("HB",zakres,".jpg",sep="")
jpeg(name)
hb<-hexbin(log2(X)[,zakres],xbins=50)
plot(hb, colramp=colorRampPalette(brewer.pal(9, "YlGnBu")[-1]))
dev.off()
}

tabelaklas<- function(x){
#PODZIA? ZE WZGL?DU NA KLASY PR?B
table(pData(x)$CLASS) #podzia? ze wzgl?du na klasy
}


BOXY <- function(X,zakres){
#BOXPLOT-y
  name=paste("BOX",zakres,".jpg",sep="")
  jpeg(name)
  boxplot(X[,zakres],col=as.numeric(pData(x)$CLASS)+1)
  dev.off()
  name=paste("BOXlog",zakres,".jpg",sep="")
  jpeg(name)
  y<-log2(X[,zakres])
  boxplot(y, col=as.numeric(pData(x)$CLASS)+1)
  dev.off()
}


SCATTER <-function(X,zakres){
  #SCATTERPLOT-y
  name=paste("SCT",zakres,".jpg",sep="")
  jpeg(name)
  plot(X[,zakres], pch = ".", main = "x")
  dev.off()
  
  y<-log2(X[,zakres])
  name=paste("SCTlog",zakres,".jpg",sep="")
  jpeg(name)
  plot(y, pch = ".", main = "x")
  dev.off()
  
  name=paste("SCTlogHIST",zakres,".jpg",sep="")
  jpeg(name)
  #histogramy
  hist(y[,zakres])
  dev.off()
}


HEAT <- function(x,X,zakres){
  name=paste("HEAT.jpg",sep="")
jpeg(name)
design <- model.matrix(~factor(x$CLASS))
y<-log2(X)
fit <- lmFit(y, design)
ebayes <- eBayes(fit)
tab <- topTable(ebayes, coef=2, adjust="fdr", n=150)
heatmap(y[rownames(tab),zakres])
dev.off()
#selected na podstawie p-value <0.05
name=paste("HEATsel.jpg",sep="")
jpeg(name)
selected<-p.adjust(ebayes$p.value[,2])<0.05
esetSel<-y[selected,]
heatmap(y[rownames(esetSel),zakres],col=topo.colors(100))
dev.off()
}

HM<- function(X,ile){
rsd <- apply(X, 1, sd)
sel <- order(rsd, decreasing = TRUE)[1:ile] #najwyzsze
name=paste("HMmax.jpg",sep="")
jpeg(name)
heatmap(X[sel, ])
dev.off()

name=paste("HMmin.jpg",sep="")
jpeg(name)
sel2 <- order(rsd, decreasing = FALSE)[1:ile] #najnizsze
heatmap(X[sel2, ])
dev.off()
}

PCCA <-function(x,X,zakres){
color=c('green','red','blue', 'red')
data.PC=prcomp(t(X),scale.=TRUE)
name=paste("PCA",zakres,".jpg",sep="")
jpeg(name)
plot(data.PC$x[zakres],col=color)
dev.off()
}


DEN <- function(z){
#density plot
  name=paste("Density",zakres,".jpg",sep="")
  jpeg(name)
df <- data.frame(Expression=as.vector(exprs(x)),
                 Sample=sampleNames(x)[col(x)])
densityplot(~log2(Expression)|Sample, df, plot.points=FALSE)
dev.off()
}


MAPLOTS <- function(x,zakres,REF){
#Create MA plots using a reference array 
  name=paste("MAT",zakres,".jpg",sep="")
  jpeg(name)
MAplot(x[,zakres],pch=".",main="vs pseudo-median reference chip",pairs=TRUE)
dev.off()

return(MAplot(x,plot.method="smoothScatter"))

return(MAplot(x,plot.method="smoothScatter")) #referencyjn? jest pierwsza

}


