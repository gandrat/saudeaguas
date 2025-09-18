# scripts/prepara_dados.R
# --- Pré-processamento e padronização dos dados

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(sf)
library(purrr)
library(janitor)
library(here)

# 1. Ler os dados de casos de todas as planilhas
# Certifique-se de que o seu diretório de trabalho está definido para a pasta PIE-Projeto
arquivos <- list.files(here("inputdata_planilhas"), pattern = "^\\d{4}_(esquistossomose|leptospirose)\\.csv$", full.names = TRUE)

ler_planilha_final <- function(arquivo) {
  # Extrai o ano e a doença do nome do arquivo
  nome_base <- basename(arquivo)
  ano <- str_extract(nome_base, "^\\d{4}") %>% as.integer()
  doenca <- str_extract(nome_base, "(esquistossomose|leptospirose)")
  
  # Lê os dados pulando as 3 primeiras linhas e usando a 4ª como cabeçalho
  raw_data <- read_delim(
    arquivo,
    skip = 3,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  # A correção definitiva: renomear a primeira coluna pelo seu índice
  names(raw_data)[1] <- "municipio_notificacao"
  
  data_limpa <- raw_data %>%
    as_tibble() %>%
    filter(!municipio_notificacao == "Total") %>%
    select(-`Total`) %>%
    mutate(
      # Extrai o código do município (os 6 primeiros dígitos) em uma nova coluna
      cod_mun = str_extract(municipio_notificacao, "^\\d{6}"),
      # Extrai o nome do município (o restante) em outra nova coluna
      nome_mun = str_remove(municipio_notificacao, "^\\d{6}\\s")
    ) %>%
    pivot_longer(
      # Agora seleciona apenas as colunas de meses de forma explícita
      cols = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
      names_to = "mes",
      values_to = "casos"
    ) %>%
    mutate(
      casos = as.numeric(gsub("-", "0", casos)),
      ano = ano,
      doenca = doenca
    )
  
  return(data_limpa)
}

# Combina todas as planilhas em um único data frame
dados_municipios <- purrr::map_dfr(arquivos, ler_planilha_final)

# 2. Ler os dados geográficos e de população
mun_shape <- st_read(here("shapes/Municipios_BR.gpkg"), quiet = TRUE) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

est_shape <- st_read(here("shapes/Estados_BR.gpkg"), quiet = TRUE) %>%
  st_set_crs(st_crs(mun_shape)) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# 3. Aggregar dados de casos para o nível de estado e fazer a junção com a população
dados_estados <- dados_municipios %>%
  as_tibble() %>%
  left_join(st_drop_geometry(mun_shape) %>% mutate(CD_MUN = str_sub(CD_MUN, 1, 6)), by = c("cod_mun" = "CD_MUN")) %>%
  group_by(NM_UF, ano, doenca) %>% 
  summarise(
    casos_total = sum(casos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(st_drop_geometry(est_shape), by = "NM_UF") %>%
  mutate(
    casos_por_100k = (casos_total / POPULACAO) * 100000
  )

# 4. Salvar os objetos em um único arquivo .rda
save(dados_municipios, dados_estados, mun_shape, est_shape, file = here("data/doencas_dados.rda"))