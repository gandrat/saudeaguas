#importacao de tabelas do datasus

# install.packages('data.table')

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)


lista_tabelas<-list.files('input_data',full.names = T)

i=2

dff<-list()

for(i in 1:length(lista_tabelas)){
  df<-read_xlsx(lista_tabelas[i],skip = 3)
  
  df<-df%>%mutate(geocod=as.numeric(substr(`Município de Infecção`,1,6)),
                  ano=2023,
                  id_doenca=substr(lista_tabelas[i],12,13),
                  uf=substr(`Município de Infecção`,1,2))%>%select(-`Município de Infecção`,-Total)
  
  df<-as.data.frame(sapply(df, as.numeric ))
  df$uf<-as.factor(df$uf)
  df$id_doenca<-as.factor(df$id_doenca)
  dfp<-df%>%pivot_longer(c(-geocod,-ano,-uf,-id_doenca),names_to = 'mes',values_to = 'casos')
  # dff<-dfp
  dff[[i]]<-dfp
}

dff<- rbindlist(dff)

ggplot(dff,aes(x=mes,y=casos,fill=uf))+geom_col()+
  facet_wrap(~id_doenca,ncol=1)+
  theme_bw()

