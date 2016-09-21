 krot <- function(rma, kr1, kr2) {
   #rma - dane po expr
   #kr1, kr2 - do liczenia krotności 
   
   #funkcja zwraca macierz ze zmianą krotności między dwoma wybranymi próbkami

   
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

#krotnoœæ 
kr_rma=(rma[,kr2]-rma[,kr1])

#wykresy zmian krotnoœci
plot(kr_rma, main='Krotnosci zmian')

return(kr_rma)
#list(RD=round(RD,3), przedzial_ufnosci_RD=c(round(RDp,3), round(RDk,3)), RR=round(RR,3), przedzial_ufnosci_RR=c(round(RRp,3), round(RRk,3)), OR=round(OR,3), przedzial_ufnosci_OR=c(round(ORp,3), round(ORk,3)) )
}