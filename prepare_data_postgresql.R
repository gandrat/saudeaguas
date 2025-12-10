# processamento_dados.R

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(sf) 

# Conexão----------
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305,
  user = "guardia",
  password = "gda_2025"
)

# Tabela principal---------
casos <- " SELECT t1.nm_mun, t1.nm_rgi, t1.nm_rgint, t1.nm_uf, t1.nm_regia,
t1.doenca, t1.data, t1.total_casos, t1.prev, t2.lat, t2.lon
FROM casos_municipios_mes AS t1
LEFT JOIN regic_2018 AS t2
ON t1.geo_cod = t2.geo_cod;
"
casos_municipios_mes <- dbGetQuery(con, casos)

casos_municipios_mes <- casos_municipios_mes %>%
  filter(prev>0,
         !is.na(lat),
         !is.na(lon))

casos_municipios_mes <- casos_municipios_mes %>%
  mutate(mes=format(data,'%m'),
         ano=format(data,'%y'))

casos_municipios_mes <- casos_municipios_mes%>%
  mutate(estacao = case_when(
    mes %in% c('10', '11', '12') ~ "Primavera",
    mes %in% c('01', '02', '03')~ "Verão",
    mes %in% c('04', '05', '06') ~ "Outono",
    mes %in% c('07', '08', '09') ~ "Inverno"
  ))

casos_municipios_mes <- casos_municipios_mes %>%
  mutate(
    estacao = factor(estacao, levels = c("Verão", "Outono", "Inverno", "Primavera")))

regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

save(casos_municipios_mes, regioes,
     file = 'output_data/doencas_dados_v4.rda')
