library(DBI)
library(RPostgres)
library(dplyr)

# Detalhes de Conexão (Assumindo que estão corretos)
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
casos_uf<-dbGetQuery(con,"select * from casos_uf;")

# Adiciona colunas 'ano' e 'mes'
casos_uf<-casos_uf %>%
  mutate(
    ano = format(data,'%Y'),
    mes = format(data,'%m')
  )

# 2. DADOS DE CASOS POR REGIÃO IMEDIATA (RGI)
# Tabela original no banco: casos_regiao
# As colunas nm_rgi e nm_rgint JÁ EXISTEM, conforme a imagem, não precisa renomear.
casos_rgi<-dbGetQuery(con,"select * from casos_rgi;")

casos_rgi<-casos_rgi %>%
  mutate(
    # Adiciona colunas 'ano' e 'mes'
    ano = format(data, '%Y'),
    mes = format(data, '%m'),
    # Calcula a Prevalência (prev) para RGI: (total_casos / pop_total) * 100000
    prev = (total_casos / pop_total) * 100000
  )

# 3. Dados Auxiliares (UF)
uf<-dbGetQuery(con,"select * from uf;")

save(casos_uf, casos_rgi, uf, file='output_data/doencas_dados_v3.rda') 

print("Arquivos de dados (doencas_dados_v3.rda) criados e salvos com sucesso!")


# script_pre_processamento.R

library(DBI)
library(RPostgres)
library(dplyr)

# Detalhes de Conexão (AJUSTE CONFORME NECESSÁRIO)
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

# Adiciona colunas 'ano' e 'mes' e calcula a taxa de prevalência (se faltar)
casos_uf <- casos_uf %>%
  mutate(
    ano = format(data,'%Y'),
    mes = format(data,'%m'),
    # Calcula a Prevalência (prev) para UF: (total_casos / pop_total) * 100000
    prev = (total_casos / pop_total) * 100000 
  )

# Executa a consulta
casos_rgi <- dbGetQuery(con,"select * from casos_rgi;")

casos_rgi <- casos_rgi %>%
  mutate(
    # Adiciona colunas 'ano' e 'mes'
    ano = format(data, '%Y'),
    mes = format(data, '%m'),
    # Calcula a Prevalência (prev) para RGI: (total_casos / pop_total) * 100000
    prev = (total_casos / pop_total) * 100000
  )

# 3. Dados Auxiliares (UF)
uf <- dbGetQuery(con,"select * from uf;")

# Salva TODOS os data frames necessários no arquivo .rda
# Garanta que a pasta 'output_data' exista!
save(casos_uf, casos_rgi, uf, file='output_data/doencas_dados_v3.rda') 

print("Arquivos de dados (doencas_dados_v3.rda) criados e salvos com sucesso!")