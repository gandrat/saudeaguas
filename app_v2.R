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

##Basemap----------------
basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Casos por Município","Casos por Município - Bolinha"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)

# (UI) -----------
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Plataforma de Informações Espaciais", id="nav",
                 
                 ## 1. Aba Mapa -----
                 tabPanel("Mapa de prevalência",
                          div(class="outer",
                              tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                              leafletOutput("mymap")),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left  = 10, width = 300, fixed=TRUE,
                                        draggable = T, height = "auto",
                                        
                                        
                                        sliderInput("map_date",
                                                    label = h5("Selecione a data"),
                                                    min = as.Date(min(regic$data),"%Y-%m-%d"),
                                                    max = as.Date(max(regic$data),"%Y-%m-%d"),
                                                    value = as.Date(max(regic$data),
                                                                    step=30,
                                                                    timeFormat = "%d-%m-%y",
                                                                    animate=animationOptions(interval = 1000, loop = FALSE))
                                        )
                          )
                 ), 
                 
                 ## 2. Aba Gráficos por Estados-----
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
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              h4("Prevalência por Data"), 
                              plotlyOutput("grafico_estados"),
                              hr(),
                              h4("Prevalência por 100 mil Habitantes"), 
                              plotlyOutput("grafico_taxa_estados"),
                              hr(),
                              h4("Prevalência por Estações dos anos"), 
                              plotlyOutput("grafico_estacao")
                            )
                          )
                 ), 
                 
                 ## 3. Aba Gráficos por Regiões Imediatas-----
                 tabPanel("Gráficos por Regiões",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_rgi", "Selecione a Doença:",
                                          choices = unique(casos_rgi$doenca),
                                          selected = "Leptospirose"),
                              pickerInput("estado_select_rgi", "Estados:",
                                          choices = rgi_estados_por_regiao, 
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_rgi$nm_uf)[1:3],
                                          multiple = TRUE),
                              pickerInput("rgint_select", "Por Região Intermediária:",
                                          choices = NULL, 
                                          options = list(`actions-box` = TRUE),
                                          multiple = FALSE),
                              pickerInput("rgi_select", "Por Região Imediata:",
                                          choices = NULL,
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              h4("Prevalência por Data"), 
                              plotlyOutput("grafico_imediatas"),
                              hr(),
                              h4("Prevalência por 100 mil Habitantes"), 
                              plotlyOutput("grafico_taxa_imediatas"),
                              hr(),
                              h4("Prevalência por Estações dos anos"), 
                              plotlyOutput("grafico_estacao_imediatas")
                            )
                          )
                 ) 
)

# backend (Server)---------
server <- function(input, output, session) {
  
  observeEvent(input$estado_select_rgi, {
    req(input$estado_select_rgi)
    
    
    rgint_filtradas_df <- intermed_por_estado_df %>%
      filter(nm_uf %in% input$estado_select_rgi)
    
    rgint_filtradas_lista <- split(rgint_filtradas_df$nm_rgint, rgint_filtradas_df$nm_uf)
    
    primeira_rgint <- unlist(rgint_filtradas_lista)[1] 
    
    updatePickerInput(
      session,
      "rgint_select",
      choices = rgint_filtradas_lista,
      selected = primeira_rgint
    )
  })
  
  
  observeEvent(input$rgint_select, {
    req(input$rgint_select, input$estado_select_rgi)
    
    
    regioes_filtradas <- imediatas_por_intermed %>%
      filter(nm_rgint == input$rgint_select) %>%
      pull(nm_rgi)
    
    updatePickerInput(
      session,
      "rgi_select",
      choices = regioes_filtradas,
      selected = head(regioes_filtradas, 5)
    )
  })
  
  ##Gráficos de Série Temporal------------	
  
  ###reactive-----
  
  ####ESTADOS----------
  casos_uf_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    casos_uf %>%	
      filter(doenca == input$doenca_estado, nm_uf %in% input$estado_select)	
  })
  
  ####REGIOES IMEDIATAS-----------
  casos_rgi_db <- reactive({
    req(input$doenca_rgi, input$rgint_select, input$rgi_select) 
    casos_rgi %>%	
      filter(doenca == input$doenca_rgi, nm_rgint == input$rgint_select, nm_rgi %in% input$rgi_select)
  })
  
  ####ESTAÇÕES ESTADOS---------------
  casos_uf_estacao_db <- reactive({
    req(input$doenca_estado, input$estado_select)
    casos_uf %>%
      filter(
        doenca == input$doenca_estado,	
        nm_uf %in% input$estado_select,
        estacao != "Não Classificado"
      ) %>%
      mutate(ano = as.factor(ano)) %>%	
      group_by(nm_uf, ano, estacao) %>%	
      summarise(total_casos_estacao = sum(total_casos, na.rm = TRUE), .groups = 'drop')
  })
  
  ####ESTAÇÕES RGI--------------
  casos_rgi_estacao_db <- reactive({
    req(input$doenca_rgi, input$rgint_select, input$rgi_select)
    casos_rgi %>%
      filter(
        doenca == input$doenca_rgi,	
        nm_rgint == input$rgint_select,
        nm_rgi %in% input$rgi_select,
        estacao != "Não Classificado"
      ) %>%
      mutate(ano = as.factor(ano)) %>%	
      group_by(nm_rgi, ano, estacao) %>%	
      summarise(total_casos_estacao = sum(total_casos, na.rm = TRUE), .groups = 'drop')
  })
  
  ####REGIC Mapa------------
  
  reactive_db_regic = reactive({
    regic %>% filter(data == min(data),
                     doenca=='Leptospirose') 
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  # --- GRÁFICOS	 ---
  
  output$grafico_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = total_casos, fill = nm_uf, group = nm_uf)) +	
      geom_col(position = "stack") +	
      labs(title = "", x = "Data", y = "Nº de Casos Totais", fill = "Estado") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))	
    ggplotly(g)
  })
  
  output$grafico_imediatas <- renderPlotly({
    g <- ggplot(casos_rgi_db(), aes(x = data, y = total_casos, fill = nm_rgi, group = nm_rgi)) +	
      geom_col(position = "stack") +	
      labs(title = "", x = "Data", y = "Nº de Casos Totais", fill = "Região Imediata") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))	
    ggplotly(g)
  })
  
  output$grafico_taxa_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = prev, fill = nm_uf, group = nm_uf)) +	
      geom_col(position = "stack") +	
      labs(title = "", x = "Data", y = "Casos por 100 mil hab.", fill = "Estado") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))	
    ggplotly(g)
  })
  
  output$grafico_taxa_imediatas <- renderPlotly({
    g <- ggplot(casos_rgi_db(), aes(x = data, y = prev, fill = nm_rgi, group = nm_rgi)) +	
      geom_col(position = "stack") +	
      labs(title = "", x = "Data", y = "Casos por 100 mil hab.", fill = "Região Imediata") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))	
    ggplotly(g)
  })
  
  # GRÁFICO DE ESTAÇÃO
  output$grafico_estacao <- renderPlotly({
    data_plot <- casos_uf_estacao_db()
    
    g <- ggplot(data_plot, aes(x = ano, y = total_casos_estacao, fill = estacao,	
                               group = estacao,
                               text = paste("Ano:", ano, "<br>Estação:", estacao, "<br>Casos:", total_casos_estacao))) +
      geom_col(position = "stack") +	
      labs(title = "", x = "Ano", y = "Nº de Casos Totais", fill = "Estação") +	
      facet_wrap(~nm_uf, scales = "free_y") +	
      theme_minimal() +
      theme(legend.title = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g, tooltip = "text")	%>%
      layout(hovermode = "x unified")
  })
  
  # GRÁFICO DE ESTAÇÃO - RGI
  output$grafico_estacao_imediatas <- renderPlotly({
    data_plot <- casos_rgi_estacao_db()
    
    g <- ggplot(data_plot, aes(x = ano, y = total_casos_estacao, fill = estacao,	
                               group = estacao,
                               text = paste("Ano:", ano, "<br>Estação:", estacao, "<br>Casos:", total_casos_estacao))) +
      geom_col(position = "stack") +	
      labs(title = "", x = "Ano", y = "Nº de Casos Totais", fill = "Estação") +	
      facet_wrap(~nm_rgi, scales = "free_y") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g, tooltip = "text")	%>%
      layout(hovermode = "x unified")
  })
  
  # MAPA----------------
  # output$mymap <- renderLeaflet({
  #   basemap
  # })
  # 
  
  observeEvent(input$map_date, {
    leafletProxy("mymap") %>% 
      clearMarkers()%>%
      addCircleMarkers(data=reactive_db_regic(), lat=~lat, lng=~lon, radius = ~prev^1/2,
                 group = "Casos por Município - Bolinha")%>%
      addHeatmap(data = reactive_db_regic(), lat = ~ lat, lng = ~ lon, intensity = ~(prev*5000000),
                 radius=8, layerId = 1, 
                 group = "Casos por Município")
  })
}

shinyApp(ui = ui, server = server)

