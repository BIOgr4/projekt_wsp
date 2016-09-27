 krot <- function(rma, kr1, kr2) {
   #rma - dane po expr
   #kr1, kr2 - do liczenia krotności 
   
kr_rma=(rma[,kr2]-rma[,kr1])

#wykresy zmian krotnosci
plot(kr_rma, main='Krotnosci zmian')

return(kr_rma)
#list(RD=round(RD,3), przedzial_ufnosci_RD=c(round(RDp,3), round(RDk,3)), RR=round(RR,3), przedzial_ufnosci_RR=c(round(RRp,3), round(RRk,3)), OR=round(OR,3), przedzial_ufnosci_OR=c(round(ORp,3), round(ORk,3)) )
}