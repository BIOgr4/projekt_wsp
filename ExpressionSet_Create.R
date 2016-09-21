path <- setwd("~/R/x86_64-pc-linux-gnu-library/3.2/rstudio/R/na czysto WSP")

source("http://bioconductor.org/biocLite.R")
#biocLite(c('hgu95av2.db','gahgu95av2.db'))
library('Biobase')
library('affy')
library('hgu95av2.db')
library('gahgu95av2.db')

pData_path <- file.path(path,'pheno.txt')
pData <- read.table(pData_path, header = TRUE, sep = "\t")

all(rownames(pData) == colnames(exprs)) #sprawdzenie poprawnosci
phenoData=new("AnnotatedDataFrame", data = pData)
phenoData=phenoData[1:4,]

celFiles <- list.celfiles()
colnames(phenoData) <- c('simple','annotation','CLASS','Sample scan')
rownames(phenoData) <- c(celFiles[1:4])

abatch <- ReadAffy(filenames=celFiles[1:4])

abatch@cdfName<-"gaHG_U95AV2"
abatch@annotation<-"gaHG_U95AV2"

rma <-  expresso(abatch,bgcorrect.method="rma", normalize.method="quantiles",
                 pmcorrect.method="pmonly", summary.method="medianpolish")

RMA <- exprs(rma)
data_RMA <- as.data.frame(RMA) # ramka danych RMA
colnames(RMA)<-c(celFiles[1:4])

#tmp <- new ("ExpressionSet", phenoData = phenoData(abatch),
#            featureData = featureData(abatch),
#            experimentData = experimentData(abatch),
#            annotation =  annotation(abatch),
#            assayData= assayData(abatch))

fData <- data.frame('names'= featureNames(abatch))
rownames(fData) <- c(rownames(RMA))
#featureData <- new("AnnotatedDataFrame",data=fData)
featureData <- new("AnnotatedDataFrame",data=fData)


experiment <- new("MIAME",name="Emila Manko",
                          lab="Polsl",
                          contact="fruzia_juzia",
                          title="Super_eksperyment",
                         url="www.zombie.pl")
proto <- data.frame('names'= featureNames(abatch));
rownames(proto) <- c(rownames(RMA))
#protocol <- new("AnnotatedDataFrame", data="trol")

ExampleSet <- ExpressionSet(assayData=RMA, phenoData=phenoData,
                        featureData = featureData,
                        experimentData=experiment,
                        annotation=annotation(abatch))

save(ExampleSet, file="ExpressionSet.Rda")
