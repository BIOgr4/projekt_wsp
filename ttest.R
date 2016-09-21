ttest<- function(rma, alfa) {
  #rma - dane po expr
  #alfa - do p-value
  
  #funkcja liczy test t wprowadzonych danych i podaje geny roznicujące
  
  
  #source("http://bioconductor.org/biocLite.R")
  #biocLite()
  #biocLite(c("affy"))
  #library("affy")
  #biocLite(c("hgu95av2.db"))
  #library("hgu95av2.db")
  #biocLite(c("gahgu95av2.db"))
  #library("gahgu95av2.db")
  #biocLite(c("hgu95av2cdf"))
  #library("hgu95av2cdf")
  #biocLite(c("lattice"))
  #library("lattice")
  #biocLite(c("limma"))
  #library("limma")
  #biocLite('genefilter')
  #library('genefilter')
  #biocLite('hgu133a.db')
  #library('hgu133a.db')
  
  #wielokrotny test t
  p_val=alfa/nrow(rma) #0.05/iloœæ sond
  t_rma=rowttests(rma, tstatOnly = FALSE)
  mnt=t(t_rma[3]<p_val)
  #wybranie genów ró¿nicuj¹cych
  gen=which(mnt==TRUE)
  roz=mnt[1,gen]
  
  return(roz)
}