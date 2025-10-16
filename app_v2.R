# app.R

# Carregue as bibliotecas necessárias
library('shiny')
library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet')
library('ggplot2')
library('plotly')
library('dplyr')
library('sf')
library('here')
library('leaflet.extras')
library('lubridate')

load('output_data/doencas_dados_v2.rda')

casos_uf <- casos_uf %>%
  mutate(
    estacao = case_when(
      mes %in% c("12", "01", "02") ~ "Verão", 
      mes %in% c("03", "04", "05") ~ "Outono",      
      mes %in% c("06", "07", "08") ~ "Inverno",          
      mes %in% c("09", "10", "11") ~ "Primavera",        
      TRUE ~ "Não Classificado" 
    ),
    ano = lubridate::year(data) # Extrai o ano da coluna 'data'
  )

# --- Preparação de dados para o pickerInput ---
estados_regiao <- casos_uf %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  arrange(nm_regia, nm_uf)

estados_por_regiao <- split(estados_regiao$nm_uf, estados_regiao$nm_regia)

##Basemap----------------
basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Casos por UF"), # Nome do layer de visualização
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)

basemap

#(UI)-----------
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Monitoramento de Doenças", id="nav",
                 
                 
                 ## Aba Gráficos por Estados-----
                 
                 tabPanel("Gráficos por Estados",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_estado", "Selecione a Doença:",
                                          choices = unique(casos_uf$doenca),
                                          selected = "Leptospirose"),
                              pickerInput("estado_select", "Estados por Região:", 
                                          choices = estados_por_regiao, 
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_uf$nm_uf)[1:5],
                                          multiple = TRUE),
                              
                              hr(),
                              
                        
                              pickerInput("estacao_select", "Estações:",
                                          choices = unique(casos_uf$estacao)[unique(casos_uf$estacao) != "Não Classificado"],
                                          selected = "Verão",
                                          multiple = FALSE)
                            ),
                            mainPanel(
                              h4("Casos Totais por Data"),
                              plotlyOutput("grafico_estados"),
                              hr(),
                              h4("Casos por 100 mil Habitantes"),
                              plotlyOutput("grafico_taxa_estados"),
                              hr(),
                              h4("Casos por Estações dos anos"),
                              plotlyOutput("grafico_estacao")
                            )
                          )
                 )
)

# backend---------
server <- function(input, output) {
  
  ##Gráficos de Série Temporal------------	
  
  casos_uf_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    casos_uf %>%	
      filter(doenca == input$doenca_estado, nm_uf %in% input$estado_select)	
  })
  
  #Gráfico de Estações
  casos_uf_verao_db <- reactive({
    req(input$doenca_estado, input$estado_select, input$estacao_select)
    casos_uf %>%
      filter(
        doenca == input$doenca_estado, 
        nm_uf %in% input$estado_select,
        estacao == input$estacao_select
      ) %>%
      group_by(nm_uf, ano) %>%
      summarise(total_casos_estacao = sum(total_casos, na.rm = TRUE), .groups = 'drop')
  })
  
  ## Gráficos--------------
  
  # 1. Gráfico de Casos Totais por Data (Série Temporal)
  output$grafico_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = total_casos, color = nm_uf, group = nm_uf)) +
      geom_line() +
      labs(title = "",	
           x = "Data", y = "Nº de Casos Totais",
           color = "Estado") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12)) 
    
    ggplotly(g)
  })
  
  # 2. Gráfico de Casos por 100 mil (Taxa) por Data
  output$grafico_taxa_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = prev, color = nm_uf, group = nm_uf)) +
      geom_line() +
      labs(title = "",	
           x = "Data", y = "Casos por 100 mil hab.",
           color = "Estado") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12)) 
    
    ggplotly(g)
  })
  
  # Gráfico para Análise Sazonal
  output$grafico_estacao <- renderPlotly({
    data_plot <- casos_uf_verao_db()
    
    g <- ggplot(data_plot, aes(x = ano, y = total_casos_estacao, fill = nm_uf, group = nm_uf, text = paste("Ano:", ano, "<br>Casos:", total_casos_estacao))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "",
           x = "Ano",
           y = paste("Casos por estações", unique(data_plot$estacao)),
           fill = "Estado") +
      scale_x_continuous(breaks = unique(data_plot$ano)) + 
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    
    ggplotly(g, tooltip = "text") 
  })
  
  # Mapa Base
  output$mymap <- renderLeaflet({
    basemap
  })
  
}
# Execute o aplicativo
shinyApp(ui = ui, server = server)