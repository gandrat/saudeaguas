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

load('output_data/doencas_dados_v4.rda') 


#agrupamentos----
group_df <- casos_municipios_mes %>%
  select(nm_uf, nm_regia) %>%
  distinct() %>%
  mutate(nm_regia = factor(nm_regia, levels = regioes)) %>%
  arrange(nm_regia, nm_uf)

group_rg <- split(group_df$nm_uf, group_df$nm_regia)

rgint_df <- casos_municipios_mes %>%
  select(nm_uf, nm_rgint) %>%
  distinct() %>%
  arrange(nm_uf, nm_rgint)

group_rgit <- casos_municipios_mes %>%
  select(nm_rgi, nm_rgint) %>%
  distinct() %>%
  arrange(nm_rgint, nm_rgi)

doencas_lookup <- casos_municipios_mes %>%
  select(doenca) %>%
  distinct() %>%
  arrange(doenca)


# data--------
if (!inherits(casos_municipios_mes$data, "Date")) {
  casos_municipios_mes$data <- as.Date(casos_municipios_mes$data)
}

estacoes_ordem <- factor(
  c("Verão", "Outono", "Inverno", "Primavera"),
  levels = c("Verão", "Outono", "Inverno", "Primavera")
)

# datas para o slider--------
data_slider <- casos_municipios_mes %>%
  mutate(data_mes = floor_date(data, "month")) %>%
  pull(data_mes) %>%
  unique() %>%
  sort()

## Basemap----------------
basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)

# (UI)

ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="Plataforma de Informações Espaciais (PIE) de Doenças de Veiculação Hídrica (DVH)", id="nav",
                 
                 tags$head(
                   tags$style(HTML("
                     .outer {
                       position: fixed;
                       top: 41px; 
                       left: 0; 
                       right: 0; 
                       bottom: 0;
                       overflow: hidden; 
                       padding: 0;
                     }
                     #controls {
                       background-color: white;
                       padding: 15px; 
                       border-radius: 10px;
                       box-shadow: 0 4px 6px 0 rgba(0, 0, 0, 0.1);
                     }
                   "))
                 ),
                 
                 ##1. Aba Mapa-----
                 tabPanel("Mapa de prevalência",
                          div(class="outer",
                              tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                              leafletOutput("mymap")),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left = 10, width = 300, fixed=TRUE,
                                        draggable = T, height = "auto",
                                        
                                        pickerInput("map_doenca", "Selecione a Doença:",
                                                    choices = unique(doencas_lookup$doenca),
                                                    selected = "Leptospirose",
                                                    multiple = FALSE),
                                        
                                        sliderInput("map_date",
                                                    label = h5("Selecione o Mês"),
                                                    min = min(data_slider),
                                                    max = max(data_slider),
                                                    value = max(data_slider),
                                                    step=32,
                                                    timeFormat = "%m-%Y",
                                                    animate=animationOptions(interval = 1000, loop = FALSE)),
                                        hr(),
                                        h5(strong("Filtro de Visualização:")),
                                        awesomeCheckboxGroup(
                                          inputId = "map_layers",
                                          label = NULL,
                                          choices = c("Densidade de Prevalência", "Marcadores de Prevalência"),
                                          selected = c("Densidade de Prevalência", "Marcadores de Prevalência"),
                                          inline = TRUE
                                        )
                          )
                 ),
                 
                 ## 2. Aba Série Temporal-----
                 tabPanel("Série Temporal",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_rgi", "Selecione a Doença:",
                                          choices = unique(doencas_lookup$doenca),
                                          selected = "Leptospirose"),
                              pickerInput("df_select", "Estados:",
                                          choices = group_rg,
                                          options = list(`actions-box` = TRUE),
                                          selected = unlist(group_rg),
                                          multiple = TRUE),
                              pickerInput("rgint_select", "Por Região Intermediária:",
                                          choices = NULL,
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_municipios_mes$nm_rgint),
                                          multiple = TRUE),
                              pickerInput("rgi_select", "Por Região Imediata:",
                                          choices = NULL,
                                          options = list(`actions-box` = TRUE),
                                          selected = unique(casos_municipios_mes$nm_rgi),
                                          multiple = TRUE),
                              pickerInput("mun_select", "Por Município:",
                                          choices = NULL,
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              h4("Casos"),
                              plotlyOutput("grafico_mun"),
                              hr(),
                              h4("Prevalência"),
                              plotlyOutput("graph_mun"),
                              hr(),
                              h4("Gráficos por Estação"),
                              plotlyOutput("grafico_estacao"),
                              hr())
                            
                          )
                 ),
                 
                 # 3. Aba: "Sobre esta plataforma"-----
                 tabPanel("Sobre esta plataforma",
                          fluidRow(
                            column(12,
                                   h3("Informações e Autoria"),
                                   hr(),
                                   h4("Última Atualização:"),
                                   p("2025-11-12"),
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
                                   ),
                                   hr(),
                                   h4("Apoio e Financiamento:"),
                                   tags$div(
                                     style = "text-align: center; margin-top: 40px;",
                                     lapply(c("ifrs_logo.png", "ga_logo.png"), function(logo) {
                                       tags$img(
                                         src = logo,
                                         height = "120px",
                                         style = "margin: 0 30px; display: inline-block;"
                                       )
                                     })
                                   )
                            )
                          )
                 )
)

# backend (Server)
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    basemap
  })
  
  # REGIÕES INTERMEDIÁRIAS-------
  observeEvent(input$df_select, {
    req(input$df_select)
    filter_df <- rgint_df %>%
      filter(nm_uf %in% input$df_select)
    
    if (nrow(filter_df) == 0) {
      filter_rgint <- NULL
      choice_rgint <- NULL
    } else {
      filter_df <- filter_df %>%
        mutate(nm_uf = factor(nm_uf, levels = input$df_select)) %>%
        arrange(nm_uf, nm_rgint)
      
      filter_rgint <- split(filter_df$nm_rgint, filter_df$nm_uf)
      choice_rgint <- unique(filter_df$nm_rgint)
    }
    
    updatePickerInput(
      session,
      "rgint_select",
      choices = filter_rgint,
      selected = choice_rgint
    )
  }, priority = 2)
  
  
  #REGIÕES IMEDIATAS-----
  observeEvent(input$rgint_select, {
    
    req(input$rgint_select)
    
    if (is.null(input$rgint_select) || length(input$rgint_select) == 0) {
      updatePickerInput(
        session,
        "rgi_select",
        choices = c("Selecione uma Região Intermediária" = "Vazio"),
        selected = NULL
      )
      return()
    }
    
    filter_df <- group_rgit %>%
      filter(nm_rgint %in% input$rgint_select)
    
    if (nrow(filter_df) == 0) {
      updatePickerInput(
        session,
        "rgi_select",
        choices = c("Nenhuma região imediata encontrada" = "Vazio"),
        selected = NULL
      )
      return()
    }
    
    filter_rg <- split(filter_df$nm_rgi, filter_df$nm_rgint)
    
    choice_rgi <- filter_df$nm_rgi
    
    updatePickerInput(
      session,
      "rgi_select",
      choices = filter_rg,
      selected = choice_rgi
    )
  }, priority = 1)
  
  
  # Agrupamento por Estado------
  observeEvent(c(input$df_select, input$rgi_select), {
    
    req(input$df_select, input$rgi_select)
    
    filter_mun <- casos_municipios_mes %>%
      filter(nm_uf %in% input$df_select,
             nm_rgi %in% input$rgi_select) %>%
      select(nm_uf, nm_mun) %>%
      distinct() %>%
      mutate(nm_uf = factor(nm_uf, levels = input$df_select)) %>%
      arrange(nm_uf, nm_mun)
    
    if (nrow(filter_mun) > 0) {
      mun_list <- split(filter_mun$nm_mun, filter_mun$nm_uf)
      choice_mun <- unique(filter_mun$nm_mun)
    } else {
      mun_list <- NULL
      choice_mun <- NULL
    }
    
    updatePickerInput(
      session,
      "mun_select",
      choices = mun_list,
      selected = choice_mun
    )
    
  }, priority = 0)
  
  
  ## BDs reativo----------------
  ### Gráficos--------
  casos_mun_db <- reactive({
    req(input$doenca_rgi, input$rgint_select, input$rgi_select, input$mun_select)
    casos_municipios_mes %>%
      filter(doenca == input$doenca_rgi,
             nm_rgint %in% input$rgint_select,
             nm_rgi %in% input$rgi_select,
             nm_mun %in% input$mun_select) %>%
      group_by(data, estacao) %>%
      summarise(
        total_casos = sum(total_casos, na.rm = TRUE),
        prev = mean(prev, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  ###Kernel-----------
  observeEvent(c(input$map_date, input$map_doenca, input$map_layers), {
    req(input$map_date, input$map_doenca)
    data_filtrada <- floor_date(input$map_date, "month")
    
    casos_mun_db_map <- casos_municipios_mes %>%
      filter(data == data_filtrada,
             doenca == input$map_doenca,
             prev > 0) %>%
      filter(!is.na(lat) & !is.na(lon))
    
    fator_kernel <- 50000
    
    proxy <- leafletProxy("mymap", session) %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      removeImage(layerId = 1) %>%
      removeControl(layerId = "kernel_legend")
    
    if (nrow(casos_mun_db_map) == 0) {
      showNotification(
        paste("Neste mês (", format(data_filtrada, "%m/%Y"), ") não houve nenhuma ocorrência desta doença (", input$map_doenca, ") ou as coordenadas não estão disponíveis no Brasil."),
        type = "warning",
        duration = 5
      )
      return()
    }
    
    max_prev <- max(casos_mun_db_map$prev, na.rm = TRUE)
    min_prev <- min(casos_mun_db_map$prev, na.rm = TRUE)
    
    kernel_colors <- rev(brewer.pal(n = 9, name = "Spectral"))
    paleta_legenda <- colorNumeric(
      palette = kernel_colors,
      domain = c(min_prev, max_prev),
      na.color = "transparent"
    )
    
    if ("Marcadores de Prevalência" %in% input$map_layers) {
      proxy %>%
        addCircleMarkers(data=casos_mun_db_map, lat=~lat, lng=~lon, radius = ~(prev-min(prev))/(max(prev)-min(prev))*20,
                         group = "Marcadores de Prevalência",
                         popup = ~paste0("Município: ", nm_mun, "<br>Data: ", format(data, "%d/%m/%Y"), "<br>Doença: ", doenca,
                                         "<br>Prevalência (100k): ", round(prev, 2)))
    }
    
    if ("Densidade de Prevalência" %in% input$map_layers) {
      proxy %>%
        addHeatmap(data = casos_mun_db_map, lat = ~ lat, lng = ~ lon,
                   intensity = ~(prev * fator_kernel),
                   radius=20,
                   blur=25,
                   minOpacity=0.01,
                   gradient = kernel_colors,
                   layerId = 1,
                   group = "Densidade de Prevalência")
      proxy %>%
        addLegend(pal = paleta_legenda,
                  values = c(min_prev, max_prev),
                  title = "Prevalência (Casos/100K hab.)",
                  position = "bottomleft",
                  opacity = 0.9,
                  layerId = "kernel_legend",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))
        )
    }
  }, ignoreNULL = FALSE)
  
  # GRÁFICOS-----
  output$grafico_mun <- renderPlotly({
    g <- ggplot(casos_mun_db(), aes(x = data, y = total_casos)) +
      geom_col(position = "stack") +
      labs(title = "", x = "Data", y = "Nº de Casos") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    ggplotly(g)
  })
  
  # GRÁFICO DE PREVALÊNCIA---------
  output$graph_mun <- renderPlotly({
    g <- ggplot(casos_mun_db(), aes(x = data, y = prev)) +
      geom_col(position = "dodge") +
      labs(title = "", x = "Data", y = "Casos por 100 mil hab.") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    ggplotly(g)
  })
  
  ##Gráfico por estações------
  output$grafico_estacao <- renderPlotly({
    
    data_plot <- casos_mun_db() %>%
      mutate(ano = lubridate::year(data)) %>%
      group_by(ano, estacao) %>%
      summarise(casos_est = sum(total_casos, na.rm = TRUE), .groups = "drop") %>%
      
      mutate(
        estacao_ordem = factor(estacao, levels = c("Verão", "Outono", "Inverno", "Primavera")),
        ano_fator = factor(ano)
      ) %>%
      arrange(ano)
    g <- ggplot(data_plot, aes(x = ano_fator, y = casos_est, fill = estacao_ordem,
                               text = paste("Ano:", ano,
                                            "<br>Estação:", estacao,
                                            "<br>Casos:", casos_est))) +
      geom_col(position = "dodge") +
      labs(title = "Casos Agregados por Ano e Estação", x = "Ano", y = "Casos Totais Agregados", fill = "Estação") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
}
shinyApp(ui = ui, server = server)