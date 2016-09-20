setwd('D:/Magsterka/WSP/ftp')
getwd()

source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite(c("affy"))
library("affy")
biocLite(c("hgu95av2.db"))
library("hgu95av2.db")
biocLite(c("gahgu95av2.db"))
library("gahgu95av2.db")
biocLite(c("hgu95av2cdf"))
library("hgu95av2cdf")
biocLite(c("hgu95av2.db"))
library("hgu95av2.db")
biocLite(c("gahgu95av2.db"))
library("gahgu95av2.db")
biocLite(c("hgu95av2cdf"))
library("hgu95av2cdf")
biocLite(c("lattice"))
library("lattice")
biocLite(c("limma"))
library("limma")
biocLite('genefilter')
library('genefilter')
biocLite('hgu133a.db')
library('hgu133a.db')

dane=ReadAffy()

#degradacja RNA
#degRNAcale=AffyRNAdeg(dane,log.it=TRUE)
#kolory=c('Black', 'Red', 'Green','Blue' )
#plotAffyRNAdeg(degRNAcale, transform = "shift.scale", col=kolory)
#legend(x="topleft",legend=c("E08_HCT116p_C_1h.CEL","E08_HCT116p_ICM_12h.CEL", 'E09_HCT116p_C_1h.CEL', 'E09_HCT116p_ICM_12h.CEL'),pch=15,col=c(1:4),bty="n")



#normalizacja
esetrma <- expresso(dane, bgcorrect.method="rma",
                    normalize.method="quantiles",pmcorrect.method="pmonly",
                    summary.method="medianpolish")


#wyci¹gniêcie danych
rma=exprs(esetrma)


#histogram RNA 
plotDensity(rma, main='Histogram dla danych znormalizowane algorytmem RMA', ylab = "density", xlab="Intensity", type="l", col=1:6, na.rm = TRUE)
legend(x="topright",legend=c("E08_HCT116p_C_1h.CEL","E08_HCT116p_ICM_12h.CEL", 'E09_HCT116p_C_1h.CEL', 'E09_HCT116p_ICM_12h.CEL'),pch=15,col=c(1:4),bty="n")

boxplot(rma, main='Wykresy pude³kowe dla znormalizowanych danych')



#krotnoœæ np miêdzy 1 i 2
kr_rma=(rma[,2]-rma[,1])

#wykresy zmian krotnoœci
plot(kr_rma, main='Krotnoœci zmian')

#wielokrotny test t
p_val=0.05/nrow(dane@featureData@data) #0.05/iloœæ sond
t_rma=rowttests(rma, tstatOnly = FALSE)
mnt=t(t_rma[3]<p_val)
#wybranie genów ró¿nicuj¹cych
gen=which(mnt==TRUE)
roz=mnt[1,gen]

x=c()
# do pozyskania nazwy genu
x <- hgu133aENTREZID
# Get the probe identifiers that are mapped to an ENTREZ Gene ID
mapped_probes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_probes])
if(length(xx) > 0) {
  # Get the ENTREZID for the first five probes
  xx[1:5]
  # Get the first one
  xx[[1]]
}

#np.
gen1=xx[[roz[1]]]
