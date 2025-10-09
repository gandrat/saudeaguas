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

# --- Carregar e Preparar os Dados ----

# 1. Carregar os data frames de casos e atributos (CASOS_MUN, CASOS_UF, JOIN_MUN, UF)
load('output_data/doencas_dados_v2.rda')

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

# interface do usuário (UI)-----------
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Monitoramento de Doenças", id="nav",
                 
                 
                 ## Aba Gráficos por Estados-----
                 tabPanel("Gráficos por Estados",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_estado", "Selecione a Doença:",
                                          choices = unique(casos_uf$doenca),
                                          selected = "Leptospirose"),
                              pickerInput("estado_select", "Estados:",
                                          choices = unique(casos_uf$nm_uf),
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_uf$nm_uf)[1:5],
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              h4("Casos Totais por Data"),
                              plotlyOutput("grafico_estados"),
                              hr(),
                              h4("Casos por 100 mil Habitantes"),
                              plotlyOutput("grafico_taxa_estados")
                            )
                          )
                 )
)

# backend---------
server <- function(input, output) {
  
  ## Reactive DBs------------	
  
  casos_uf_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    casos_uf %>%	
      filter(doenca == input$doenca_estado, nm_uf %in% input$estado_select)	
  })
  
  ## Gráficos--------------
  
  # 1. Gráfico de Casos Totais por Data (Série Temporal)
  output$grafico_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = casos, color = nm_uf, group = nm_uf)) +
      geom_line() +
      labs(title = "",	
           x = "Data", y = "Nº de Casos Totais") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(g)
  })
  
  # 2. Gráfico de Casos por 100 mil (Taxa) por Data
  output$grafico_taxa_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = prev, color = nm_uf, group = nm_uf)) +
      geom_line() +
      labs(title = "",	
           x = "Data", y = "Casos por 100 mil hab.") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(g)
  })
  
  
  # Mapa Base
  output$mymap <- renderLeaflet({
    basemap
  })
  
}
# Execute o aplicativo
shinyApp(ui = ui, server = server)