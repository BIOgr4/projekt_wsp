
source("http://bioconductor.org/biocLite.R")

biocLite('Biobase')
library('Biobase')

biocLite('MASS')
library('MASS')

setwd('C:/Users/SIKORA/Documents/projekt_wsp')
source('C:/Users/SIKORA/Documents/projekt_wsp/Klasteryzacja.R')

load("ExpressionSet.Rda")

Funkcja(ExampleSet,2)
