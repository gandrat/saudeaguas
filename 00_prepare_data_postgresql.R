library(DBI)
library(RPostgres)
library(dplyr)

con <- dbConnect(
  RPostgres::Postgres(), # Or RPostgreSQL::PostgreSQL()
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305, # Or your specific port
  user = "guardia",
  password = "gda_2025"
)

load('output_data/doencas_dados.rda')

##Testando conexão
# casos_mun<-dbGetQuery(con,"select * from caso");
casos_uf<-dbGetQuery(con,"select * from casos_uf;")
casos_uf<-casos_uf%>%mutate(ano=format(data,'%Y'),
                            mes=format(data,'%m'))


uf<-dbGetQuery(con,"select * from uf;")


save(casos_uf, uf, file='output_data/doencas_dados_v2.rda')


save(casos_uf,file='output_data/doencas_dados_v2.rda')

