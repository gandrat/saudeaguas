# Desenvolvimento de uma Plataforma de Informações Espaciais (PIE) para análise espaço-temporal de Doenças de Veiculação Hídrica (DVH) no Brasil.

Este repositório contém o código e a documentação para a Plataforma de Informações Espaciais (PIE) desenvolvida como Trabalho de Conclusão de Curso (TCC) em Geoprocessamento por Isabelli Cruz Costa, no ano de 2025. A plataforma visa facilitar a análise espaço-temporal da prevalência de Doenças de Veiculação Hídrica (DVH) no Brasil, permitindo a eanálise através de mapas de prevalência de calor (Kernel) e por tamanho proporcional de círculos, com a variação do número de casoos das doenças por município ao longo das estações, do ano e dos meses.
A ferramenta é construída em torno de mapas interativos que utilizam técnicas de geovisualização.

# Metodologia 

A PIE emprega duas técnicas primárias de mapeamento para representar diferentes aspectos da distribuição da doença:

# 1. Mapa de Estimativa de Densidade de Kernel - KDE:

O Mapa de Concentração utiliza a técnica de Estimativa de Densidade de Kernel (KDE) para identificar áreas de alta concentração de casos (pontos de calor), suavizando os dados pontuais em uma superfície contínua. 

# 2. Mapa de proporção

Os círculos são dimensionados para representar a prevalência da população por município. Sendo o tamanho dos círculos determinados pela taxa de prevalência populacional (casos por 100.000 habitantes) de cada município no mês selecionado. Sendo crucial para neutralizar o viés da população, onde um município populoso teria um círculo maior apenas por ter mais habitantes, e não necessariamente maior risco.

# Análise sazonal

A análise temporal é feita através da segmentação da base de dados por períodos sazonais, ocorrendo as seguintes etapas:

Extração do mês: O número do mês é extraído da coluna de data de cada registro, logo determinando qual mês irá representar uma estação do ano. Ficando definido como: 
Verão: Dezembro, Janeiro, Fevereiro
Outono: Março, Abril, Maio
Inverno: Junho, Julho, Agosto
Primavera: Setembro, Outubro, Novembro

Esta variável permite filtrar a base de dados de maneira dinamica, revelando como a distribuição espacial dos pontos de calor e o risco de prevalência municipal se alteram a cada estação.

# Como executar o projeto

Pré-requisitos: Realizar o download das seguintes bibliotecas em ambiente RStudio: 
library('shiny')
library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet')
library('ggplot2')
library('plotly')
library('dplyr')
library('sf')
library('leaflet.extras')
library('lubridate')
library('RColorBrewer')

Pré-processamento: [saudeaguas/prepare_data_postgresql.R](saudeaguas/prepare_data_postgresql.R)

Execução: [saudeaguas/app.R](saudeaguas/app.R)
