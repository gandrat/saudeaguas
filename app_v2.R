# app.R

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

#1.Carregue o arquivo rda!!!-----
load('output_data/doencas_dados_v2.rda')

if (!inherits(regic$data, "Date")) {
  regic$data <- as.Date(regic$data)
}

#ordem das estações-----
estacoes_ordem <- factor(
  c("Verão", "Outono", "Inverno", "Primavera"),
  levels = c("Verão", "Outono", "Inverno", "Primavera")
)

#datas para o slider--------
datas_unicas_slider <- regic %>%
  mutate(data_mes = floor_date(data, "month")) %>%
  pull(data_mes) %>%
  unique() %>%
  sort()

##Basemap----------------
basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Densidade de Prevalência","Marcadores de Prevalência"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)

# (UI) -----------
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Plataforma de Informações Espaciais", id="nav",
                 
                 ##1. Aba Mapa-----
                 tabPanel("Mapa principal de prevalência",
                          div(class="outer",
                              tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                              leafletOutput("mymap")),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left = 10, width = 300, fixed=TRUE,
                                        draggable = T, height = "auto",
                                        
                                        # Filtro de seleção de doenças - Slider-------
                                        pickerInput("map_doenca", "Selecione a Doença:",
                                                    choices = unique(regic$doenca),
                                                    selected = "Leptospirose",
                                                    multiple = FALSE),
                                        
                                        sliderInput("map_date",
                                                    label = h5("Selecione o Mês"),
                                                    min = min(datas_unicas_slider),
                                                    max = max(datas_unicas_slider),
                                                    value = max(datas_unicas_slider),
                                                    step=30,
                                                    timeFormat = "%m-%Y",
                                                    animate=animationOptions(interval = 1000, loop = FALSE))
                          )
                 ), # <--- Fechamento correto da aba 1
                 
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
                              h4("Prevalência por Estações (Anos)"),
                              plotlyOutput("grafico_estacao")
                            )
                          )
                 ), # <--- Fechamento correto da aba 2
                 
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
                                          multiple = TRUE),
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
                              h4("Prevalência por Estações (Anos)"),
                              plotlyOutput("grafico_estacao_imediatas")
                            )
                          )
                 ), # <--- Fechamento correto da aba 3
                 
                 #4. Aba: "Sobre esta plataforma"-----
                 tabPanel("Sobre esta plataforma",
                          fluidRow(
                            column(12,
                                   h3("Informações e Autoria"),
                                   
                                   hr(),
                                   
                                   h4("Última Atualização:"),
                                   p("2025-10-28"),
                                   
                                   hr(),
                                   
                                   h4("Detalhes:"),
                                   p("As informações apresentadas neste site baseiam-se em dados oficiais sobre a evolução das doenças de veiculação hídrica no Brasil. Todavia, os resultados aqui divulgados são fruto de modelos científicos e não necessariamente representam a realidade, pois fatores não previstos podem influenciar a expansão do contágio. Embora os autores vislumbrem um bom potencial no uso desses resultados para a tomada de decisões visando a gestão da epidemia, convém reafirmar que incertezas são inerentes ao processo de modelagem preditiva e que as informações aqui apresentadas devem ser consideradas um ensaio acadêmico e sempre interpretadas com a devida cautela."),
                                   
                                   hr(),
                                   
                                   h4("Fontes de Dados:"),
                                   tags$ul(
                                     tags$li(strong("Variáveis do Censo Demográfico:"), " IBGE"),
                                     tags$li(strong("Ocorrências de casos das DVH utilizadas (agregadas por mês, ano e município de notificação):"), " DATASUS")
                                   ),
                                   
                                   hr(),
                                   
                                   h4("Autores:"),
                                   p(strong("Dr. Tiago Gandra")),
                                   tags$ul(
                                     tags$li("Professor do Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul"),
                                     tags$li("Laboratório de Geotecnologias e Meio Ambiente (GEOMA)"),
                                     tags$li(tags$a(href = "mailto:tiago.gandra@riogrande.ifrs.edu.br", "tiago.gandra@riogrande.ifrs.edu.br"))
                                   ),
                                   
                                   p(strong("Isabelli Cruz Costa")),
                                   tags$ul(
                                     tags$li("Discente no Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul"),
                                     tags$li("Bolsista CNPq do Projeto Guardiãs das Águas"),
                                     tags$li(tags$a(href = "mailto:11040438@aluno.riogrande.ifrs.edu.br", "11040438@aluno.riogrande.ifrs.edu.br"))
                                   )
                            )
                          )
                 ) 
) 

# backend (Server)---------
server <- function(input, output, session) {
  
  # 1. OBSERVE EVENT: ESTADOS -> REGIÕES INTERMEDIÁRIAS
  observeEvent(input$estado_select_rgi, {
    req(input$estado_select_rgi)
    
    rgint_filtradas_df <- intermed_por_estado_df %>%
      filter(nm_uf %in% input$estado_select_rgi)
    
    if (nrow(rgint_filtradas_df) == 0) {
      rgint_filtradas_lista <- NULL
      primeiras_rgint <- NULL
    } else {
      rgint_filtradas_lista <- split(rgint_filtradas_df$nm_rgint, rgint_filtradas_df$nm_uf)
      
      primeiras_rgint <- unique(rgint_filtradas_df$nm_rgint)
    }
    
    updatePickerInput(
      session,
      "rgint_select",
      choices = rgint_filtradas_lista,
      selected = primeiras_rgint
    )
  })
  
  
  #2.REGIÕES INTERMEDIÁRIAS (rgint) -> REGIÕES IMEDIATAS (rgi)-----------------
  observeEvent(input$rgint_select, {
    
    rgint_select_debounced <- debounce(reactive(input$rgint_select), 500)
    
    req(rgint_select_debounced())
    
    if (is.null(rgint_select_debounced()) || length(rgint_select_debounced()) == 0) {
      updatePickerInput(
        session,
        "rgi_select",
        choices = c("Selecione uma Região Intermediária" = "Vazio"),
        selected = NULL
      )
      return()
    }
    
    # 2. Filtra as Regiões Imediatas pelas Regiões Intermediárias selecionadas--------
    regioes_filtradas_df <- imediatas_por_intermed %>%
      filter(nm_rgint %in% rgint_select_debounced())
    
    if (nrow(regioes_filtradas_df) == 0) {
      updatePickerInput(
        session,
        "rgi_select",
        choices = c("Nenhuma região imediata encontrada" = "Vazio"),
        selected = NULL
      )
      return()
    }
    
    #lista de regiões imediatas por Região Intermediária-------
    regioes_filtradas_lista <- split(regioes_filtradas_df$nm_rgi, regioes_filtradas_df$nm_rgint)
    
    primeiras_rgi <- regioes_filtradas_df$nm_rgi
    
    updatePickerInput(
      session,
      "rgi_select",
      choices = regioes_filtradas_lista,
      selected = primeiras_rgi
    )
  }, priority = 1)
  
  ##Gráficos de Série Temporal.-----
  
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
      filter(doenca == input$doenca_rgi, nm_rgint %in% input$rgint_select, nm_rgi %in% input$rgi_select)
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
      group_by(nm_uf, doenca, estacao) %>%
      summarise(total_casos_estacao = sum(total_casos, na.rm = TRUE), .groups = 'drop') %>%
      mutate(estacao = factor(estacao, levels = levels(estacoes_ordem)))
  })
  
  ####ESTAÇÕES RGI--------------
  casos_rgi_estacao_db <- reactive({
    req(input$doenca_rgi, input$rgint_select, input$rgi_select)
    casos_rgi %>%
      filter(
        doenca == input$doenca_rgi,
        nm_rgint %in% input$rgint_select,
        nm_rgi %in% input$rgi_select,
        estacao != "Não Classificado"
      ) %>%
      group_by(nm_rgi, doenca, estacao) %>%
      summarise(total_casos_estacao = sum(total_casos, na.rm = TRUE), .groups = 'drop') %>%
      mutate(estacao = factor(estacao, levels = levels(estacoes_ordem)))
  })
  
  
  output$mymap <- renderLeaflet({
    basemap
  })
  
  #GRÁFICOS---------
  
  output$grafico_estados <- renderPlotly({
    g <- ggplot(casos_uf_db(), aes(x = data, y = total_casos, fill = nm_uf, group = nm_uf)) +
      geom_col(position = "stack") +
      labs(title = "", x = "Data", y = "Casos", fill = "Estado") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    ggplotly(g)
  })
  
  output$grafico_imediatas <- renderPlotly({
    g <- ggplot(casos_rgi_db(), aes(x = data, y = total_casos, fill = nm_rgi, group = nm_rgi)) +
      geom_col(position = "stack") +
      labs(title = "", x = "Data", y = "de Casos Totais", fill = "Região Imediata") +
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
  
  #GRÁFICO DE ESTAÇÃO - ESTADOS----------
  output$grafico_estacao <- renderPlotly({
    data_plot <- casos_uf_estacao_db()
    
    g <- ggplot(data_plot, aes(x = estacao, y = total_casos_estacao, fill = estacao,
                               group = estacao,
                               text = paste("Estado:", nm_uf, "<br>Estação:", estacao, "<br>Casos:", total_casos_estacao))) +
      geom_col(position = "dodge") +
      labs(title = "", x = "Estação", y = "Prevalência por Estações (Anos)", fill = "Estação") +
      facet_wrap(~nm_uf, scales = "free_y") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  #GRÁFICO DE ESTAÇÃO - RGI------
  output$grafico_estacao_imediatas <- renderPlotly({
    data_plot <- casos_rgi_estacao_db()
    
    g <- ggplot(data_plot, aes(x = estacao, y = total_casos_estacao, fill = estacao,
                               group = estacao,
                               text = paste("Região:", nm_rgi, "<br>Estação:", estacao, "<br>Casos Totais:", total_casos_estacao))) +
      geom_col(position = "dodge") +
      labs(title = "", x = "Estação", y = "Casos Totais (Anos)", fill = "Estação") +
      facet_wrap(~nm_rgi, scales = "free_y") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # MAPA----------------
  observeEvent(c(input$map_date, input$map_doenca), {
    req(input$map_date, input$map_doenca)
    
    #1.FILTRAGEM----------
    data_filtrada <- floor_date(input$map_date, "month")
    
    dados_filtrados_regic <- regic %>%
      filter(data == data_filtrada,
             doenca == input$map_doenca,
             prev > 0)
    
    leafletProxy("mymap", session) %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      removeImage(layerId = 1) %>%
      removeControl(layerId = "kernel_legend")
    
    # 2. ALERTA--------
    if (nrow(dados_filtrados_regic) == 0) {
      showNotification(
        paste("Neste mês (", format(data_filtrada, "%m/%Y"), ") não houve nenhuma ocorrência desta doença (", input$map_doenca, ") no Brasil."),
        type = "warning",
        duration = 5
      )
      return()
    }
    
    #3.FOCO, GRADIENTE E LEGENDA----------
    max_prev <- max(dados_filtrados_regic$prev, na.rm = TRUE)
    min_prev <- min(dados_filtrados_regic$prev, na.rm = TRUE)
    
    #intensidade do Kernel------
   
    kernel_colors <- rev(brewer.pal(n = 9, name = "Spectral"))
    paleta_legenda <- colorNumeric(
      palette = kernel_colors,
      domain = c(min_prev, max_prev),
      na.color = "transparent"
    )
    
    leafletProxy("mymap", session) %>%
      addCircleMarkers(data=dados_filtrados_regic, lat=~lat, lng=~lon, radius = ~(prev-min(prev))/(max(prev)-min(prev))*20,
                       group = "Marcadores de Prevalência",
                       popup = ~paste0("Município: ", nome_cidad, "<br>Data: ", format(data, "%d/%m/%Y"), "<br>Doença: ", doenca,
                                       "<br>Prevalência (100k): ", round(prev, 2))) %>%
      
      #Mapa de Calor (Kernel)---------
    addHeatmap(data = dados_filtrados_regic, lat = ~ lat, lng = ~ lon, intensity = ~(prev-min(prev))/(max(prev)-min(prev))*5000,
               radius=25,
               blur=20,
               minOpacity=0.01,
               # gradient = kernel_colors,
               layerId = 1,
               group = "Densidade de Prevalência") %>%
      
      #LEGENDA-----------
    addLegend(pal = paleta_legenda,
              values = c(min_prev, max_prev),
              title = "Prevalência (Casos/100K hab.)",
              position = "bottomleft",
              opacity = 0.9,
              layerId = "kernel_legend",
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))
    )
  })
}

shinyApp(ui = ui, server = server)