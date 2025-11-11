# processamento_dados.R

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(sf) 

# Estação------------
def_estacao <- function(df) {
  df <- df %>%
    mutate(data = as.Date(data),
           mes = month(data),
           ano = lubridate::year(data))
  
  df_base <- df %>%
    mutate(estacao = case_when(
      mes %in% c(9, 10, 11) ~ "Primavera",
      mes %in% c(12, 1, 2)~ "Verão",
      mes %in% c(3, 4, 5) ~ "Outono",
      mes %in% c(6, 7, 8) ~ "Inverno",
      TRUE ~ "Não Classificado"
    ))
  
  meses_transicao <- df_base %>%
    filter(mes %in% c(3, 6, 9, 12))
  
  replicas <- meses_transicao %>%
    mutate(
      estacao = case_when(
        mes == 3 ~ "Verão",
        mes == 6 ~ "Outono",
        mes == 9 ~ "Inverno",
        mes == 12 ~ "Primavera",
        TRUE ~ estacao
      ),
      ano = ifelse(mes == 12 & estacao == "Primavera", ano - 1, ano)
    ) %>%
    anti_join(df_base, by = names(df_base))
  
  df_final <- bind_rows(df_base, replicas) %>%
    mutate(
      ano_estacao = ifelse(mes %in% c(1, 2, 3) & estacao %in% c("Verão", "Outono"), ano - 1, ano)
    ) %>%
    select(-mes, -ano)
  
  return(df_final)
}

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
dbDisconnect(con)

print(paste("Linhas iniciais:", nrow(casos_municipios_mes)))

casos_municipios_mes <- casos_municipios_mes %>%
  filter(prev>0,
         !is.na(lat),
         !is.na(lon))

casos_municipios_mes <- def_estacao(casos_municipios_mes)

casos_municipios_mes <- casos_municipios_mes %>%
  mutate(
    estacao = factor(estacao, levels = c("Verão", "Outono", "Inverno", "Primavera")),
    ano_estacao = as.integer(ano_estacao))

regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

save(casos_municipios_mes, regioes,
     file = 'output_data/doencas_dados_v4.rda')

print("Arquivo de dados (doencas_dados_v4.rda) criado e salvo com sucesso!")
