# Carregar pacotes necessários
library(dplyr)

# Caminho do arquivo CSV
caminho_arquivo <- 'input_data/sinannet_cnv_leptobr161355200_132_11_251.csv'

# Verificar se o arquivo existe antes de carregar
if(file.exists(caminho_arquivo)) {
  # Tente carregar o arquivo com o delimitador correto (vírgula ou ponto e vírgula)
  dados <- read.csv(caminho_arquivo, fileEncoding = "ISO-8859-1", sep = ";") # Verifique se é ";"
  
  # Visualizar as primeiras linhas do arquivo
  head(dados)
  
  # Exibir os dados no Viewer do RStudio
  View(dados)
  
  # Separar colunas numéricas e de caracteres
  dados_numericos <- dados %>%
    select(where(is.numeric))
  
  dados_caracteres <- dados %>%
    select(where(~ is.character(.) | is.factor(.)))
  
  # Exibir as primeiras linhas dos dados numéricos e de caracteres
  head(dados_numericos)
  head(dados_caracteres)
  
  # Exibir as tabelas no Viewer do RStudio
  View(dados_numericos)
  View(dados_caracteres)
} else {
  cat("O arquivo não foi encontrado. Verifique o caminho.")
}

