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
load('output_data/doencas_dados.rda')

# Define o caminho para o GeoPackage único (corrigido para 'shape')
geopackage_path <- "input_data/shape/PIE.gpkg"

# 2. Ler os objetos geográficos (com geometria) para o Leaflet
# Lendo a camada de Municípios do PIE.gpkg
mun_sf <- st_read(geopackage_path, layer = "BR_Municipios_2024", quiet = TRUE)
# Lendo a camada de Estados do PIE.gpkg
uf_sf <- st_read(geopackage_path, layer = "BR_UF_2024", quiet = TRUE)


# 3. Preparar os objetos geográficos para junção e visualização
mun_sf <- mun_sf %>% 
  # Cria a coluna de junção COD (código de 6 dígitos)
  mutate(COD = substring(CD_MUN, 1, 6)) %>% 
  # Transforma para WGS84 para Leaflet
  st_transform(crs = 'EPSG:4326') 

uf_sf <- uf_sf %>%
  # Transforma para WGS84 para Leaflet
  st_transform(crs = 'EPSG:4326')


basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Casos por Município"), # Nome do layer de visualização
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)


# interface do usuário (UI)-----------
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Monitoramento de Doenças", id="nav",
                 
                 ## Mapa dos Casos-----
                 tabPanel("Mapa dos Casos",
                          div(class="outer",
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 100, left = 10, width = 300, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h4("Controles"),
                                            p("Selecione as opções abaixo para filtrar os dados."),
                                            
                                            selectInput("doenca_selecionada", "Selecione a Doença:",
                                                        choices = unique(CASOS_MUN$DVH),
                                                        selected = unique(CASOS_MUN$DVH)[1]),
                                            
                                            selectInput("ano_selecionado", "Selecione o Ano:",
                                                        choices = unique(CASOS_MUN$ANO),
                                                        selected = unique(CASOS_MUN$ANO)[1]),
                                            
                                            selectInput("mes_selecionado", "Selecione o Mês:",
                                                        choices = unique(CASOS_MUN$MES),
                                                        selected = unique(CASOS_MUN$MES)[1]),
                                            
                                            # Mantemos o actionButton para forçar a atualização do mapa
                                            actionButton("update_view", "Gerar Mapa!")),
                              
                              # Painel para exibir os totais
                              h3(textOutput("total_casos"), align = "right"),
                              h4(textOutput("num_municipios"), align = "right")
                          )
                 ),
                 
                 ## Gráficos por Estados-----
                 tabPanel("Gráficos por Estados",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_estado", "Selecione a Doença:",
                                          choices = unique(CASOS_UF$DVH),
                                          selected = "leptospirose"),
                              pickerInput("estado_select", "Estados:",
                                          choices = unique(CASOS_UF$NM_UF),
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(CASOS_UF$NM_UF)[1:5],
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
  
  # Reactive para os gráficos: Filtra dados de estados (CASOS_UF)
  CASOS_UF_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    CASOS_UF %>%	
      filter(DVH == input$doenca_estado, NM_UF %in% input$estado_select)	
  })
  
  # Reactive para o mapa: Filtra os dados de município (CASOS_MUN) e junta com a geometria (mun_sf)
  CASOS_MUN_db_sf <- eventReactive(input$update_view, {
    req(input$doenca_selecionada, input$ano_selecionado, input$mes_selecionado)
    
    # Filtra os dados de casos
    dados_filtrados <- CASOS_MUN %>%	
      filter(DVH == input$doenca_selecionada,	
             ANO == input$ano_selecionado,	
             MES == input$mes_selecionado,
             CASOS > 0) # Filtra apenas municípios com casos
    
    # Junta com a geometria (mun_sf) usando a chave COD
    dados_filtrados_sf <- dados_filtrados %>%
      # Agrega os casos por município para a junção com o sf
      group_by(COD, NOME) %>%
      summarise(CASOS_TOTAIS = sum(CASOS, na.rm = TRUE), .groups = 'drop') %>%
      # Junção com a geometria, usa left_join para manter a geometria
      left_join(mun_sf, by = 'COD') %>%
      # Converte de volta para sf após a junção
      st_as_sf()
    
    return(dados_filtrados_sf)
  })
  
  # Totais (Continua usando o filtro municipal)
  output$total_casos <- renderText({
    # Usa o mesmo filtro, mas sem a geometria, para calcular o total
    casos_total <- CASOS_MUN %>%
      filter(DVH == input$doenca_selecionada,	
             ANO == input$ano_selecionado,	
             MES == input$mes_selecionado) %>%
      summarise(total = sum(CASOS, na.rm = TRUE)) %>%
      pull(total)
    
    paste0(format(casos_total, big.mark="."), " casos")
  })
  
  output$num_municipios <- renderText({
    n_mun <- CASOS_MUN_db_sf() %>% distinct(NOME) %>% nrow()	
    paste0(n_mun, " municípios afetados")
  })
  
  ## Gráficos--------------
  
  # 1. Gráfico de Casos Totais por Data (Série Temporal)
  output$grafico_estados <- renderPlotly({
    g <- ggplot(CASOS_UF_db(), aes(x = DATA, y = CASOS, color = NM_UF, group = NM_UF)) +
      geom_line() +
      labs(title = "",	
           x = "Data", y = "Nº de Casos Totais") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(g)
  })
  
  # 2. Gráfico de Casos por 100 mil (Taxa) por Data
  output$grafico_taxa_estados <- renderPlotly({
    g <- ggplot(CASOS_UF_db(), aes(x = DATA, y = POP_100K, color = NM_UF, group = NM_UF)) +
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
  
  
  # Lógica de Atualização do Mapa (eventReactive acionado pelo botão)
  observeEvent(input$update_view, {
    dados_mapa <- CASOS_MUN_db_sf()
    
    # 1. Define paleta de cores
    cor_fun <- colorBin(
      palette = "Reds",
      domain = dados_mapa$CASOS_TOTAIS,
      bins = c(0, 1, 5, 10, 20, 50, Inf) # Exemplo de classes, ajuste conforme seus dados
    )
    
    # 2. Define o rótulo (popup)
    labels <- sprintf(
      "<strong>%s</strong><br/>Casos: %s",
      dados_mapa$NOME, dados_mapa$CASOS_TOTAIS
    ) %>% lapply(htmltools::HTML)
    
    # 3. Atualiza o mapa
    leafletProxy("mymap", data = dados_mapa) %>%	
      clearShapes() %>% # Limpa as geometrias antigas
      addPolygons(
        fillColor = ~cor_fun(CASOS_TOTAIS),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = labels,
        group = "Casos por Município"
      )
  })
  
  
}
# Execute o aplicativo
shinyApp(ui = ui, server = server)