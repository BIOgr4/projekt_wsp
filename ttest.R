ttest<- function(rma, alfa) {
  
  #wielokrotny test t
  p_val=alfa/nrow(rma) #0.05/iloœæ sond
  t_rma=rowttests(rma, tstatOnly = FALSE)
  mnt=t(t_rma[3]<p_val)
  #wybranie genów ró¿nicuj¹cych
  gen=which(mnt==TRUE)
  roz=mnt[1,gen]
  
  return(roz)
}