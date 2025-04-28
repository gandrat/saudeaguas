##Pré processamento de dados

library(dplyr)
library(sf)

d<-read.csv2('input_data/sinannet_cnv_leptobr161355200_132_11_251.csv',sep=';',header=TRUE,fileEncoding='iso-8859-1')

df <- as.data.frame(sapply(d%>%select(-1), as.numeric))

df$muni<-d$Município.de.residência

#modificar nomes das variáveis
names(d)<-c('muni','jan','fev','mar','abr')