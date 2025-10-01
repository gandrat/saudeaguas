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
# arquivo .rda que gerou
load('output_data/doencas_dados.rda')
mun<-st_transform(mun,crs='EPSG:4326')
uf<-st_transform(uf,crs='EPSG:4326')

basemap = leaflet(option=leafletOptions(zoomControl=FALSE)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Mapa1"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("Municípios: Mortes por 100 mil","Estados: Casos por 100 mil","Estados: Mortes por 100 mil","Municípios: Índice de Vulnerabilidade Social (IVS)"))  %>%
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
                                                        choices = unique(casos_mun$doenca),
                                                        selected = unique(casos_mun$doenca)[1]), # Adicionado
                                            
                                            selectInput("ano_selecionado", "Selecione o Ano:",
                                                        choices = unique(casos_mun$ano),
                                                        selected = unique(casos_mun$ano)[1]), # Adicionado
                                            
                                            selectInput("mes_selecionado", "Selecione o Mês:",
                                                        choices = unique(casos_mun$mes),
                                                        selected = unique(casos_mun$mes)[1]),
                                            
                                            actionButton("update_view", "Gerar Mapa!")),
                              
                              # Painel para exibir os totais
                              h3(textOutput("total_casos"), align = "right"),
                              h4(textOutput("num_municipios"), align = "right")
                              
                              # Gráfico simplificado
                              # plotOutput("grafico_casos_total", height="180px", width="100%")
                          )
                 ),
                 
                 
                 
                 ## Gráficos por Estados-----
                 tabPanel("Gráficos por Estados",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_estado", "Selecione a Doença:",
                                          choices = unique(casos_uf$doenca),
                                          selected = "leptospirose"),
                              pickerInput("estado_select", "Estados:",
                                          choices = unique(casos_uf$NM_UF),
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_uf$NM_UF)[1:5],
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              plotlyOutput("grafico_estados")
                            )
                          )
                 )
                 
)

plot(uf)
# backend---------
server <- function(input, output) {
  ##Reactive DBs------------  
  
  # Reactive para os gráficos: Filtra dados de estados
  casos_uf_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    casos_uf %>%
      filter(doenca == input$doenca_estado, NM_UF %in% input$estado_select)
  })
  
  casos_uf_db_sf <- reactive({
    req(input$doenca_selecionada, input$ano_selecionado, input$mes_selecionado)
    casos_uf %>%
      filter(doenca == input$doenca_selecionada,
             ano==input$ano_selecionado)%>%
      left_join(uf,by='CD_UF')
  })
  
  # Reactive para o mapa: Filtra os dados de acordo com a seleção do usuário
  casos_mun_db <- reactive({
    req(input$doenca_selecionada, input$ano_selecionado)
    casos_mun %>%
      filter(doenca == input$doenca_selecionada, 
             ano == input$ano_selecionado,
             mes==input$mes_selecionado)%>%
      left_join(mun,by='cod_mun')
  })
  
  # Totais
  output$total_casos <- renderText({
    casos_total <- sum(casos_mun_db()$casos, na.rm = TRUE)
    paste0(format(casos_total, big.mark=","), " casos")
  })
  
  
  
  output$num_municipios <- renderText({
    n_mun <- casos_mun_db() %>% distinct(nome_mun) %>% nrow()
    paste0(n_mun, " municípios afetados")
  })
  
  ## Gráficos--------------
  ###Casos por estados----------
  
  # Gráfico por Estados
  output$grafico_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = ano, y = casos_total, color = NM_UF, group = NM_UF)) +
      geom_line() +
      labs(title = paste("Casos de", input$doenca_estado, "por Estado"),
           x = "Ano", y = "Nº de Casos Totais") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(g)
  })
  
  output$mymap <- renderLeaflet({
    basemap})
  
  
  # Mapa Leaflet----------
  
  # observeEvent(input$update_view,{
  #   leafletProxy("mymap") %>% 
  #     # clearMarkers() %>%
  #     
  #     addPolygons(data = casos_uf_db_sf(), 
  #                 color=c("#A50026"))
  # })
  
  
}
# Execute o aplicativo
shinyApp(ui = ui, server = server)