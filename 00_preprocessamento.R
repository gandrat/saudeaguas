##Pré processamento de dados
# Carregar pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)

# Caminho do arquivo CSV
arquivosc <- list.files('input_data/datasus_bruto', pattern = ".csv",full.names = T)
arquivos <- list.files('input_data/datasus_bruto', pattern = ".csv")

# Verificar se o arquivo existe antes de carregar
if (file.exists(arquivosc)) {
  df <- read.csv(arquivosc, fileEncoding = "ISO-8859-1", sep = ";") 
}
  # Tente carregar o arquivo com o delimitador correto (vírgula ou ponto e vírgula)
  

# Visualizar as primeiras linhas do arquivo
head(df)

df[, -1] <- lapply(df[, -1], as.numeric)

df$ano<-as.numeric(substring(arquivos,1,4))

dfp<-df%>%select(-Total)%>%
  pivot_longer(
    cols = c(-Município.de.residência,-ano), # Seleciona todas as colunas EXCETO 'municipio' para pivotar
    names_to = "mes", # Nome da nova coluna que conterá os nomes das colunas originais
    values_to = "valor"    # Nome da nova coluna que conterá os valores das colunas originais
  )

ggplot(dfp,aes(x=mes,y=valor))+geom_col()

#Próximos passos:
# 1. transformar nome do mês em data.
# 2. Adicionar nome da doença na coluna de valor
# 3. separar geocódigo de nome do município

