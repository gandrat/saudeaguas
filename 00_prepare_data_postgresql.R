# script_pre_processamento.R

library(DBI)
library(RPostgres)
library(dplyr)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305,
  user = "guardia",
  password = "gda_2025"
)

# 1. Dados de Casos por UF (Estados)
# Tabela original no banco: casos_uf
casos_uf <- dbGetQuery(con,"select * from casos_uf;")
casos_uf <- casos_uf %>%
  mutate(
    ano = format(data,'%Y'),
    mes = format(data,'%m'),
    # Calcula a Prevalência 
    prev = (total_casos / pop_total) * 100000 
  )

casos_rgi <- dbGetQuery(con,"select * from casos_rgi;")
casos_rgi <- casos_rgi %>%
  mutate(
    # Adiciona colunas 'ano' e 'mes'
    ano = format(data, '%Y'),
    mes = format(data, '%m'),
    # Calcula a Prevalência
    prev = (total_casos / pop_total) * 100000
  )

uf <- dbGetQuery(con,"select * from uf;")


save(casos_uf, casos_rgi, uf, file='output_data/doencas_dados_v2.rda') 

print("Arquivos de dados (doencas_dados_v2.rda) criados e salvos com sucesso!")