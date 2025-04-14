#explorando base de dados

#Pacotes--------------
##Instalando pacotes--------
install.packages('ggplot2')


##Carregando pacotes---------
library(datasets)
library(ggplot2)
library(dplyr)
library(tidyr)

df<-WorldPhones
df<-as.data.frame(df)
df$year<-row.names(df)

plot(df$N.Amer,df$S.Amer)

#fazendo um gráfico de barras com os dados do df
ggplot(df,aes(x=year,y=N.Amer))+geom_col()


ggplot(df)+
  geom_col(aes(x=year,y=N.Amer))+
  geom_col(aes(x=year,y=Europe),fill='green')+
  geom_col(aes(x=year,y=S.Amer),fill='pink')

ggplot(df)+
  # geom_col(aes(x=year,y=N.Amer))+
  geom_col(aes(x=year,y=S.Amer),fill='pink')


#pivotando a tabela-------------
df2<-df%>%
  pivot_longer(
    cols = 1:7,
    names_to = "continente",
    values_to = "n_telefones"
  )

ggplot(df2,aes(x=year,y=n_telefones,fill=continente))+geom_col()

ggplot(df2,aes(x=year,y=n_telefones,fill=continente))+geom_path()

ggplot(df2,aes(x=year,y=n_telefones))+geom_col()+
  facet_wrap(~continente)




#Exemplo chat gpt--------------
# Criar um dataframe de exemplo (tabela larga)
df_largo <- data.frame(
  ID = 1:3,
  Nome = c("Ana", "Bruno", "Carla"),
  Matemática = c(8, 7.5, 9),
  História = c(7, 6.5, 8.5),
  Geografia = c(9, 8, 9.5)
)

# Exibir a tabela original
print("Tabela larga:")
print(df_largo)

# Usar pivot_longer para transformar a tabela larga em longa
df_longo <- df_largo %>%
  pivot_longer(
    cols = Matemática:Geografia, # ou cols = -c(ID, Nome)
    names_to = "Disciplina",
    values_to = "Nota"
  )

# Exibir a tabela transformada
print("Tabela longa:")
print(df_longo)