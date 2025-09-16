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

# --- Carregar e Preparar os Dados ---
# Carregue os objetos do arquivo .rda que você gerou
load(here("data", "doencas_dados.rda"))

# Combine os dados dos municípios com a geometria do mapa
mapa_dados_municipios <- left_join(mun_shape, dados_municipios, by = c("CD_MUN" = "cod_mun"))

# Defina a interface do usuário (UI)
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
                                                        choices = unique(mapa_dados_municipios$doenca),
                                                        selected = unique(mapa_dados_municipios$doenca)[1]), # Adicionado
                                            
                                            selectInput("ano_selecionado", "Selecione o Ano:",
                                                        choices = unique(mapa_dados_municipios$ano),
                                                        selected = unique(mapa_dados_municipios$ano)[1]), # Adicionado
                                            
                                            # Painel para exibir os totais
                                            h3(textOutput("total_casos"), align = "right"),
                                            h4(textOutput("num_municipios"), align = "right"),
                                            
                                            # Gráfico simplificado
                                            plotOutput("grafico_casos_total", height="180px", width="100%")
                              )
                          )
                 ),
                 
                 ## Gráficos por Estados-----
                 tabPanel("Gráficos por Estados",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_estado", "Selecione a Doença:",
                                          choices = unique(dados_estados$doenca),
                                          selected = "leptospirose"),
                              pickerInput("estado_select", "Estados:",
                                          choices = unique(dados_estados$NM_UF),
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(dados_estados$NM_UF)[1:5],
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              plotlyOutput("grafico_estados")
                            )
                          )
                 )
                 
)

# Defina a lógica do servidor (backend)
server <- function(input, output) {
  
  # Reactive para o mapa: Filtra os dados de acordo com a seleção do usuário
  dados_mapa_filtrados <- reactive({
    req(input$doenca_selecionada, input$ano_selecionado)
    mapa_dados_municipios %>%
      filter(doenca == input$doenca_selecionada, ano == input$ano_selecionado)
  })
  
  # Reactive para os gráficos: Filtra dados de estados
  dados_estados_filtrados <- reactive({
    req(input$doenca_estado, input$estado_select)
    dados_estados %>%
      filter(doenca == input$doenca_estado, NM_UF %in% input$estado_select)
  })
  
  # Totais
  output$total_casos <- renderText({
    casos_total <- sum(dados_mapa_filtrados()$casos, na.rm = TRUE)
    paste0(format(casos_total, big.mark="."), " casos")
  })
  
  output$num_municipios <- renderText({
    n_mun <- dados_mapa_filtrados() %>% distinct(nome_mun) %>% nrow()
    paste0(n_mun, " municípios afetados")
  })
  
  # Gráfico de Casos Totais
  output$grafico_casos_total <- renderPlot({
    dados_mapa_filtrados() %>%
      group_by(mes) %>%
      summarise(casos_mensais = sum(casos, na.rm = TRUE)) %>%
      ggplot(aes(x = mes, y = casos_mensais)) +
      geom_bar(stat = "identity", fill = "salmon") +
      labs(title = paste("Casos de", input$doenca_selecionada, "em", input$ano_selecionado),
           x = "Mês", y = "Nº de Casos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico por Estados
  output$grafico_estados <- renderPlotly({
    g <- ggplot(dados_estados_filtrados(), aes(x = ano, y = casos_total, color = NM_UF, group = NM_UF)) +
      geom_line() +
      labs(title = paste("Casos de", input$doenca_estado, "por Estado"),
           x = "Ano", y = "Nº de Casos Totais") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(g)
  })
  
  # Renderize o mapa Leaflet
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-50, -18, 4.4)
  })
  
  # Atualize o mapa Leaflet com os dados filtrados
  observeEvent(input$doenca_selecionada, {
    
    pal <- colorBin("YlOrRd", domain = dados_mapa_filtrados()$casos)
    
    leafletProxy("mymap", data = dados_mapa_filtrados()) %>%
      clearMarkers() %>%
      addPolygons(
        fillColor = ~pal(casos),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste0(nome_mun, " (", CD_MUN, "): ", casos, " casos")
      ) %>%
      addLegend(
        pal = pal,
        values = ~casos,
        title = "Casos",
        position = "bottomright"
      )
  })
}

# Execute o aplicativo
shinyApp(ui = ui, server = server)