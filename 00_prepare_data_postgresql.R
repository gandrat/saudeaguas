# script_pre_processamento

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)

# ESTAÇÕES -----
define_estacao <- function(df) {
  df %>%
    mutate(
      estacao = case_when(
        # Verão (Dezembro, Janeiro, Fevereiro)
        mes %in% c("12", "01", "02") ~ "Verão",
        # Outono (Março, Abril, Maio)
        mes %in% c("03", "04", "05") ~ "Outono",
        # Inverno (Junho, Julho, Agosto)
        mes %in% c("06", "07", "08") ~ "Inverno",
        # Primavera (Setembro, Outubro, Novembro)
        mes %in% c("09", "10", "11") ~ "Primavera",
        TRUE ~ "Não Classificado"
      ),
      ano = lubridate::year(data) # Garante que o ano seja numérico para gráficos
    )
}

# CONEXÃO ----

# Detalhes de Conexão
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305,
  user = "guardia",
  password = "gda_2025"
)

# 1. CASOS MUNICIPIOS
casos <- " SELECT t1.geo_cod, t1.nm_mun, t1.cd_rgi, t1.nm_rgi, t1.cd_rgint, t1.nm_rgint,
t1.cd_uf, t1.nm_uf, t1.cd_regia, t1.nm_regia, t1.id_doenca, t1.doenca,
t1.data, t1.mes, t1.total_casos, t1.pop_total, t1.prev,t2.lat, t2.lon
FROM casos_municipios_mes AS t1

LEFT JOIN regic_2018 AS t2
ON t1.geo_cod = t2.geo_cod;
"

casos_municipios_mes <- dbGetQuery(con, casos)

casos_municipios_mes <- casos_municipios_mes %>%
  mutate(
    ano = format(data, '%Y'),
    mes = format(data, '%m')
  )
casos_municipios_mes <- define_estacao(casos_municipios_mes)


# 2. TABELAS -----

# Casos por UF
casos_uf <- casos_municipios_mes %>%
  group_by(nm_uf, nm_regia, doenca, data, estacao, ano) %>% 
  summarise(
    total_casos = sum(total_casos, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    prev = (total_casos / pop_total) * 100000
  )

# Casos por Região Imediata
casos_rgi <- casos_municipios_mes %>%
  group_by(nm_rgi, nm_rgint, doenca, data, estacao, ano) %>% 
  summarise(
    total_casos = sum(total_casos, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    prev = (total_casos / pop_total) * 100000
  )

# 3. LOOKUPS -----

# filtros de Regiões Imediatas
imediatas_regiao <- casos_municipios_mes %>%
  select(nm_rgi, nm_rgint, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_rgint, nm_rgi)

# Aba 1- RG. Intermediárias agrupadas por Região Geográfica
intermed_por_regiao <- imediatas_regiao %>%
  select(nm_rgint, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_rgint) %>%
  split(.$nm_rgint, .$nm_regia)

# filtro de Regiões Imediatas
imediatas_por_intermed <- imediatas_regiao %>%
  select(nm_rgi, nm_rgint) %>%
  distinct() %>%
  arrange(nm_rgint, nm_rgi)

# filtros de Estados (Aba 1)
estados_regiao <- casos_uf %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_uf)
estados_por_regiao <- split(estados_regiao$nm_uf, estados_regiao$nm_regia)

# filtros de RGI/UF (Aba 2)
rgi_estados_regiao <- casos_municipios_mes %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_uf)
rgi_estados_por_regiao <- split(rgi_estados_regiao$nm_uf, rgi_estados_regiao$nm_regia)

# Região Intermediária por Estado
intermed_por_estado_df <- casos_municipios_mes %>%
  select(nm_uf, nm_rgint) %>%
  distinct() %>%
  arrange(nm_uf, nm_rgint)

save(casos_uf, casos_rgi, casos_municipios_mes, imediatas_regiao, intermed_por_regiao, imediatas_por_intermed,
     estados_regiao, estados_por_regiao, rgi_estados_regiao, rgi_estados_por_regiao,
     intermed_por_estado_df,
     file = 'output_data/doencas_dados_v2.rda')

save(casos_municipios_mes, imediatas_regiao, intermed_por_regiao, imediatas_por_intermed,
     estados_regiao, estados_por_regiao, rgi_estados_regiao, rgi_estados_por_regiao,
     intermed_por_estado_df,
     file = 'output_data/doencas_dados_v3.rda')

print("Arquivos de dados (doencas_dados_v2.rda) criados e salvos com sucesso!")