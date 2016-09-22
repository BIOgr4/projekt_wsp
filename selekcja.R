#selekcja cech 

ekspresje=exprs(ExampleSet)
ExampleSet@phenoData@data$annotation

roznicujace=vector(length = nrow(ekspresje))
pwart=vector(length = nrow(ekspresje))
fold=vector(length = nrow(ekspresje))
for(i in 1:nrow(ekspresje))
{
  pwart[i]=wilcox.test(ekspresje[i,ExampleSet@phenoData@data$CLASS=="NORMAL"],ekspresje[i,ExampleSet@phenoData@data$CLASS!="NORMAL"],alternative = "two.sided")$p.value
  fold[i]=mean(ekspresje[i,ExampleSet@phenoData@data$CLASS=="NORMAL"])-mean(ekspresje[i,ExampleSet@phenoData@data$CLASS!="NORMAL"])
  }

prog=1

roznicujace=vector(length=nrow(ekspresje))
roznicujace[pwart>0.05&abs(fold)>prog]=TRUE

indeksy_roznicujace=which(roznicujace)