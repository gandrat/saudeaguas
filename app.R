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

# agrupamentos----
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

group_rgint <- casos_municipios_mes %>%
  select(nm_rgi, nm_rgint) %>%
  distinct() %>%
  arrange(nm_rgint, nm_rgi)

doencas <- casos_municipios_mes %>%
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
dt_sld <- casos_municipios_mes %>%
  mutate(data_mes = floor_date(data, "month")) %>%
  pull(data_mes) %>%
  unique() %>%
  sort()

## Basemap----------------
bm = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)

legend_fun <- function(pal_cor, min_val, max_val) {
  
  vals_pretty <- pretty(c(min_val, max_val), n = 4)
  vals_leg <- vals_pretty[vals_pretty <= (max_val * 1.05)]
  
  if (length(vals_leg) < 3) {
    vals_leg <- unique(c(round(min_val, 1),
                         round(min_val + (max_val - min_val) / 2, 1),
                         round(max_val, 1)))
  } else if (length(vals_leg) > 4) {
    if (length(vals_leg) >= 4) {
      vals_leg <- vals_leg[c(1, seq(2, length(vals_leg)-1, length.out=2), length(vals_leg))]
    }
    vals_leg <- unique(round(vals_leg, 1))
  }
  
  vals_leg <- vals_leg[order(vals_leg)]
  vals_leg <- unique(round(vals_leg, 1))
  
  rad_leg <- scales::rescale(sqrt(vals_leg), to = c(6, 48))
  
  itens <- sapply(1:length(vals_leg), function(i) {
    val <- vals_leg[i]
    rad <- rad_leg[i]
    col <- pal_cor(val)
    
    # HTML para os círculos
    paste0(
      "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
      " <div style='width: ", rad, "px; height: ", rad, "px; background-color: transparent; border: 2.8px solid ", col, "; border-radius: 50%; flex-shrink: 0;'></div>",
      " <span style='margin-left: 8px; vertical-align: middle;'>", val, "</span>",
      "</div>"
    )
  })
  
  # HTML pois o addControl() depende disso
  legend_html <- HTML(paste0(
    "<div style='padding: 6px 8px; font: 14px/16px Arial, Helvetica, sans-serif; background: white; color: #333; box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px;'>",
    " <strong>Prevalência (Casos/100K hab.)</strong><br>",
    paste(itens, collapse = ""),
    "</div>"
  ))
  
  return(legend_html)
}


# 3. UI------------------------------------------------------
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
}"))
                 ),
                 
                 #1. Aba Mapa-----
                 tabPanel("Mapa de prevalência",
                          div(class="outer",
                              tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                              leafletOutput("mymap")),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left = 10, width = 300, fixed=TRUE,
                                        draggable = T, height = "auto",
                                        
                                        pickerInput("map_doenca", "Selecione a Doença:",
                                                    choices = unique(doencas$doenca),
                                                    selected = "Leptospirose",
                                                    multiple = FALSE),
                                        
                                        sliderInput("map_date",
                                                    label = h5("Selecione o Mês"),
                                                    min = min(dt_sld),
                                                    max = max(dt_sld),
                                                    value = max(dt_sld),
                                                    step=32,
                                                    timeFormat = "%m-%Y",
                                                    animate=animationOptions(interval = 1000, loop = FALSE)),
                                        hr(),
                                        h5(strong("Resumo de Casos:")),
                                        uiOutput("total_cases"),
                                        hr(),
                                        h5(strong("Filtro de Visualização:")),
                                        awesomeCheckboxGroup(
                                          inputId = "map_layers",
                                          label = NULL,
                                          choices = c("Mapa de densidade", "Mapa de casos"),
                                          selected = c("Mapa de densidade", "Mapa de casos"),
                                          inline = FALSE
                                        )
                          )
                 ),
                 
                 # 2. Aba Série Temporal-----
                 tabPanel("Série Temporal",
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("doenca_rgi", "Selecione a Doença:",
                                          choices = unique(doencas$doenca),
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
                              h4("Gráficos por Estação"),
                              plotlyOutput("grafico_estacao"),
                              hr())
                            
                          )
                 ),
                 
                 # 3. Aba: "Sobre esta plataforma"-----
                 tabPanel("Sobre esta plataforma",
                          tags$div(
                            style = "text-align: center; margin-top: 40px;",
                            lapply(c("ifrs_logo.png"), function(logo) {
                              tags$img(
                                src = logo,
                                height = "120px",
                                style = "margin: 0 30px; display: inline-block;"
                              )
                            })
                          ),
                          fluidRow(
                            column(12,
                                   h3("Informações:"),
                                   hr(),
                                   h4("Última Atualização:"),
                                   p("2025-11-17"),
                                   hr(),
                                   h4("Detalhes:"),
                                   p("A plataforma está sendo desenvolvida como Trabalho de Conclusão de Curso do Curso Técnico Integrado de Geoprocessamento no Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul - Campus Rio Grande, com foco na integração de dados sobre as DVH, obtidos do DataSUS entre os anos de 2014 a 2024, sendo tratada por meio de ferramentas de geoprocessamento. Utilizando a linguagem R e o pacote Shiny, foram implementados mapas interativos e gráficos que permitem uma visualização acessível e detalhada da distribuição dessas doenças em escala municipal, estadual, região intermediária e região imediata. Além de permitir análises espaciais e temporais, a PIE se propõe como instrumento de democratização do acesso à informação, contribuindo para a conscientização dos impactos causados pela ausência de saneamento adequado. Os dados apresentados possibilitam identificar padrões, cruzar informações e fomentar discussões interdisciplinares entre saúde, geografia e tecnologia. Os testes preliminares com a estrutura da plataforma demonstram que ela oferece navegação funcional, com filtros, visualizações dinâmicas e potencial de uso tanto por pesquisadores quanto por gestores públicos. A iniciativa visa, assim, ampliar a transparência das informações em saúde e estimular o uso social dos dados públicos, abrindo caminhos para futuras melhorias, pesquisas e políticas públicas voltadas à redução das desigualdades socioespaciais no Brasil."),
                                   hr(),
                                   h4("Fontes de Dados:"),
                                   tags$ul(
                                     tags$li(strong("Variáveis do Censo Demográfico:"), " IBGE"),
                                     tags$li(strong("Ocorrências de casos das DVH utilizadas (agregadas por mês, ano e município de notificação):"), " DATASUS")
                                   ),
                                   hr(),
                                   h4("Autoria:"),
                                   p(strong("Isabelli Cruz Costa")),
                                   tags$ul(
                                     tags$li("Discente no Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul"),
                                     tags$li("Bolsista CNPq do Projeto Guardiãs das Águas"),
                                     tags$li(tags$a(href = "mailto:11040438@aluno.riogrande.ifrs.edu.br", "11040438@aluno.riogrande.ifrs.edu.br"))
                                   ),
                                   hr(),
                                   h4("Colaboradores:"),
                                   p(strong("Tiago Borges Ribeiro Gandra")),
                                   tags$ul(
                                     tags$li("Professor do Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul"),
                                     tags$li("Laboratório de Geotecnologias e Meio Ambiente (GEOMA)"),
                                     tags$li(tags$a(href = "mailto:tiago.gandra@riogrande.ifrs.edu.br", "tiago.gandra@riogrande.ifrs.edu.br"))
                                   ),
                                   hr(),
                                   p(strong("Carolina Larrosa de Oliveira Claro")),
                                   tags$ul(
                                     tags$li("Professora do Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul"),
                                     tags$li("Laboratório de Geotecnologias na Gestão Municipal"),
                                     tags$li(tags$a(href = "mailto:carol.larrosa@riogrande.ifrs.edu.br", "carol.larrosa@riogrande.ifrs.edu.br"))
                                   ),
                                   hr(),
                                   h4("Apoio :"),
                                   tags$div(
                                     style = "text-align: center; margin-top: 40px;",
                                     lapply(c("apoio.png", "ga_logo.png"), function(logo) {
                                       tags$img(
                                         src = logo,
                                         height = "130px",
                                         style = "margin: 0 30px; display: inline-block;"
                                       )
                                     })
                                   )
                            )
                          )
                 )
)

# backend (Server)---------------------
server <- function(input, output, session) {
  map_legend <- reactiveValues(total = 0, data_atual = Sys.Date())
  output$mymap <- renderLeaflet({
    bm
  })
  
  # Menu de filtros---------
  ## Rgint-------
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
  
  ##Rgi-----
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
    
    filter_df <- group_rgint %>%
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
  
  ## BD Reativo (Gráficos)------------
  observeEvent(c(input$df_select, input$rgi_select), {
    
    req(input$df_select, input$rgi_select)
    
    filt_mun <- casos_municipios_mes %>%
      filter(nm_uf %in% input$df_select,
             nm_rgi %in% input$rgi_select) %>%
      select(nm_uf, nm_mun) %>%
      distinct() %>%
      mutate(nm_uf = factor(nm_uf, levels = input$df_select)) %>%
      arrange(nm_uf, nm_mun)
    
    if (nrow(filt_mun) > 0) {
      mun_list <- split(filt_mun$nm_mun, filt_mun$nm_uf)
      ch_mun <- unique(filt_mun$nm_mun)
    } else {
      mun_list <- NULL
      ch_mun <- NULL
    }
    
    updatePickerInput(
      session,
      "mun_select",
      choices = mun_list,
      selected = ch_mun
    )
    
  }, priority = 0)
  
  
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
        .groups = "drop"
      )
  })
  
  # Mapa-----------
  observeEvent(c(input$map_date, input$map_doenca, input$map_layers), {
    req(input$map_date, input$map_doenca)
    dt_filt <- floor_date(input$map_date, "month")
    
    cases_date <- casos_municipios_mes %>%
      filter(data == dt_filt, doenca == input$map_doenca)
    
    cases_leg <- cases_date %>%
      summarise(total = sum(total_casos, na.rm = TRUE)) %>%
      pull(total)
    
    map_legend$total <- cases_leg
    map_legend$data_atual <- dt_filt
    
    df_map <- cases_date %>%
      filter(prev > 0) %>%
      filter(!is.na(lat) & !is.na(lon))
    
    fat_ker <- 50000
    
    proxy <- leafletProxy("mymap", session) %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      removeImage(layerId = 1) %>%
      removeControl(layerId = "kernel_legend") %>%
      removeControl(layerId = "casos_legend")
    
    if (nrow(df_map) == 0) {
      showNotification(
        paste("Neste mês (", format(dt_filt, "%m/%Y"),
              ") não houve nenhuma ocorrência desta doença (", input$map_doenca,
              ") ou as coordenadas não estão disponíveis no Brasil."),
        type = "warning",
        duration = 5
      )
      return()
    }
    
    ## Escala e paleta-----------
    max_prev <- max(df_map$prev, na.rm = TRUE)
    min_prev <- min(df_map$prev, na.rm = TRUE)
    
    pal_cor <- colorNumeric(
      palette = c("#FF9898FF", "#D9636CFF", "#A91E45FF", "#691238FF", "#251714FF"),
      domain = c(min_prev, max_prev),
      na.color = "transparent"
    )
    
    ## Mapa de casos-----
    if ("Mapa de casos" %in% input$map_layers) {
      
      df_map <- df_map %>%
        mutate(rad_esc = scales::rescale(sqrt(prev),
                                         to = c(6, 48)))
      
      proxy %>%
        addCircleMarkers(
          data = df_map,
          lat = ~lat, lng = ~lon,
          radius = ~rad_esc,
          color = ~pal_cor(prev),
          weight = 2.8,
          fillColor = ~pal_cor(prev),
          fillOpacity = 0.25,
          stroke = TRUE,
          group = "Mapa de casos",
          popup = ~paste0(
            "<b>Município:</b> ", nm_mun,
            "<br><b>Estado:</b> ", nm_uf,
            "<br><b>Data:</b> ", format(data, "%d/%m/%Y"),
            "<br><b>Prevalência (Casos/100k hab.):</b> ", round(prev, 2),
            "<br><b>Nº de Casos:</b> ", total_casos
          )
        )
      
      if (nrow(df_map) > 0) {
        legenda_html <- legend_fun(
          pal_cor = pal_cor,
          min_val = min_prev,
          max_val = max_prev
        )
        
        proxy %>%
          addControl(
            html = legenda_html,
            position = "bottomright",
            layerId = "casos_legend"
          )
      }
    }
    
    ## Mapa de densidade-----------
    if ("Mapa de densidade" %in% input$map_layers) {
      cores_ker <- rev(brewer.pal(n = 9, name = "Spectral"))
      pal_leg_prev <- colorNumeric(
        palette = cores_ker,
        domain = c(min_prev, max_prev),
        na.color = "transparent"
      )
      
      proxy %>%
        addHeatmap(
          data = df_map,
          lat = ~lat, lng = ~lon,
          intensity = ~(prev * fat_ker),
          radius = 20,
          blur = 25,
          minOpacity = 0.01,
          gradient = cores_ker,
          layerId = 1,
          group = "Mapa de densidade"
        ) %>%
        addLegend(
          pal = pal_leg_prev,
          values = c(min_prev, max_prev),
          title = "Prevalência (Casos/100K hab.)",
          position = "bottomleft",
          opacity = 0.9,
          layerId = "kernel_legend",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))
        )
    }
  }, ignoreNULL = FALSE)
  
  output$total_cases <- renderUI({
    req(map_legend$total, map_legend$data_atual)
    
    dt_formatada <- format(map_legend$data_atual, "(%m/%Y)")
    tot_formatado <- prettyNum(map_legend$total, big.mark = ".", decimal.mark = ",")
    
    tags$div(
      tags$b(dt_formatada),
      " Houveram ",
      tags$b(tot_formatado),
      " casos neste mês."
    )
  })
  
  # Gráficos-----------
  ## Gráfico da série temporal-------
  output$grafico_mun <- renderPlotly({
    req(casos_mun_db())
    g <- ggplot(casos_mun_db(), aes(x = data, y = total_casos)) +
      geom_col(position = "stack") +
      labs(title = "", x = "Data", y = "Nº de Casos") +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    ggplotly(g)
  })
  
  ## Gráfico por estações------
  output$grafico_estacao <- renderPlotly({
    
    req(casos_mun_db())
    
    df_plot <- casos_mun_db() %>%
      mutate(ano = lubridate::year(data)) %>%
      group_by(ano, estacao) %>%
      summarise(casos = sum(total_casos, na.rm = TRUE), .groups = "drop")
    
    cores_est <- c(
      "Verão" = "#F1BB7BFF",
      "Outono" = "#FD6467FF",
      "Primavera" = "#9A5155FF",
      "Inverno" = "#A497B2FF"
    )
    
    g <- ggplot(df_plot, aes(x = factor(ano), y = casos, fill = estacao,
                             text = paste("Ano:", ano,
                                          "<br>Estação:", estacao,
                                          "<br>Casos:", casos))) +
      geom_col(position = "dodge") +
      labs(y = "Nº de Casos", fill = "Estação") +
      labs(x = "Ano") +
      scale_fill_manual(values = cores_est) +
      theme_minimal() +
      theme(legend.title = element_text(size = 12))
    
    ggplotly(g, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
}

shinyApp(ui = ui, server = server)
