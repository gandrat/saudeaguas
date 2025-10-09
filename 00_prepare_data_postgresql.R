library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(), # Or RPostgreSQL::PostgreSQL()
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305, # Or your specific port
  user = "guardia",
  password = "guardia_2025"
)

load('output_data/doencas_dados.rda')

##Testando conexão
# casos_mun<-dbGetQuery(con,"select * from caso");
casos_uf<-dbGetQuery(con,"select * from casos_uf;")
casos_uf<-casos_uf%>%mutate(ano=format(data,'%Y'),
                            mes=format(data,'%m'))


save(casos_uf,file='output_data/doencas_dados_v2.rda')
