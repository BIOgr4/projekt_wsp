path <- setwd("~/Desktop/projekt_wsp/cel_files")

#source("http://bioconductor.org/biocLite.R")
#library(BiocInstaller)
#biocLite(c('hgu95av2.db','gahgu95av2.db','Biobase','affy'))
library('Biobase')
library('affy')
library('hgu95av2.db')
library('gahgu95av2.db')
library(dplyr)
library(tools)
pData_path <- file.path(path,'/pheno.txt')
pData <- read.table(pData_path, header = TRUE, sep = "\t")
celFiles <- list.celfiles()
macierze<-file_path_sans_ext(celFiles)
all(rownames(pData) == colnames(exprs)) #sprawdzenie poprawnosci
#wyrzucenie wierszy dla ktorych brakuje plikow z pData
pData<-
  pData %>% filter(scan %in% macierze)

phenoData=new("AnnotatedDataFrame", data = pData)


colnames(phenoData) <- c('simple annotation','CLASS','Sample',' scan')
rownames(phenoData) <- c(celFiles)

abatch <- ReadAffy(filenames=celFiles)

#reannotacja Ferrari:
abatch@cdfName<-"gahgu95av2.db"
abatch@annotation<-"gahgu95av2.db"

rma <-  expresso(abatch,bgcorrect.method="rma", normalize.method="quantiles",
                 pmcorrect.method="pmonly", summary.method="medianpolish")

RMA <- exprs(rma)
data_RMA <- as.data.frame(RMA) # ramka danych RMA
colnames(RMA)<-c(celFiles)

#tmp <- new ("ExpressionSet", phenoData = phenoData(abatch),
#            featureData = featureData(abatch),
#            experimentData = experimentData(abatch),
#            annotation =  annotation(abatch),
#            assayData= assayData(abatch))

fData <- data.frame('names'= featureNames(abatch))
rownames(fData) <- c(rownames(RMA))
#featureData <- new("AnnotatedDataFrame",data=fData)
featureData <- new("AnnotatedDataFrame",data=fData)


experiment <- new("MIAME",name="WSP grupa 4",
                          lab="Polsl",
                          contact="github",
                          title="Super_eksperyment",
                         url="https://github.com/BIOgr4/projekt_wsp")
proto <- data.frame('names'= featureNames(abatch));
rownames(proto) <- c(rownames(RMA))
#protocol <- new("AnnotatedDataFrame", data="trol")

ExampleSet <- ExpressionSet(assayData=RMA, phenoData=phenoData,
                        featureData = featureData,
                        experimentData=experiment,
                        annotation=annotation(abatch))

save(ExampleSet, file="ExpressionSet.Rda")
