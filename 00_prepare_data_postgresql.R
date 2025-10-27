# script_pre_processamento.R 

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)

# --- ESTAÇÕES ---
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

# --- CONEXÃO ---

# Detalhes de Conexão
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305,
  user = "guardia",
  password = "gda_2025"
)

# 1. Casos por UF (Estados)
casos_uf <- dbGetQuery(con,"select * from casos_uf;")

# Adiciona colunas 'ano', 'mes' e calcula a taxa de prevalência
casos_uf <- casos_uf %>%
  mutate(
    ano = format(data,'%Y'),
    mes = format(data,'%m'),
    prev = (total_casos / pop_total) * 100000 
  )
casos_uf <- define_estacao(casos_uf)


# 2. REGIÃO IMEDIATA (RGI)
casos_rgi <- dbGetQuery(con,"select * from casos_rgi;")

casos_rgi <- casos_rgi %>%
  mutate(
    ano = format(data, '%Y'),
    mes = format(data, '%m'),
    prev = (total_casos / pop_total) * 100000
  )
casos_rgi <- define_estacao(casos_rgi)

# 3. (UF)
uf <- dbGetQuery(con,"select * from uf;")

# 4. REGIC

regic<-dbGetQuery(con,"select * from casos_regic;")

# --- LOOKUP ---

# 1.  filtros de Regiões Imediatas
imediatas_regiao <- casos_rgi %>%
  select(nm_rgi, nm_rgint, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_rgint, nm_rgi)

# RG. Intermediárias agrupadas por Região Geográfica (Aba 1)
intermed_por_regiao <- imediatas_regiao %>%
  select(nm_rgint, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_rgint) %>%
  split(.$nm_rgint, .$nm_regia) 

#  filtro de Regiões Imediatas
imediatas_por_intermed <- imediatas_regiao %>%
  select(nm_rgi, nm_rgint) %>%
  distinct() %>%
  arrange(nm_rgint, nm_rgi)

# 2. filtros de Estados (Aba 1)
estados_regiao <- casos_uf %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_uf)
estados_por_regiao <- split(estados_regiao$nm_uf, estados_regiao$nm_regia)

# 3. filtros de RGI/UF (Aba 2)
rgi_estados_regiao <- casos_rgi %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_uf)
rgi_estados_por_regiao <- split(rgi_estados_regiao$nm_uf, rgi_estados_regiao$nm_regia)

# Região Intermediária por Estado 
intermed_por_estado_df <- casos_rgi %>%
  select(nm_uf, nm_rgint) %>%
  distinct() %>%
  arrange(nm_uf, nm_rgint)

save(casos_uf, casos_rgi, uf, imediatas_regiao, intermed_por_regiao, imediatas_por_intermed,
     estados_regiao, estados_por_regiao, rgi_estados_regiao, rgi_estados_por_regiao, regic,
     intermed_por_estado_df,
     file = 'output_data/doencas_dados_v2.rda')

print("Arquivos de dados (doencas_dados_v2.rda) criados e salvos com sucesso!")
