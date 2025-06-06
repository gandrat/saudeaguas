library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(leaflet)
library(geobr)
library(sf)

#teste para modificações
ui <- fluidPage(
  titlePanel("Quadro de incidências de Leptospirose e instalações sanitárias por municípios brasileiros no ano de 2010."),
  
  tags$div(
    style = "background-color: #f9f9f9; border-left: 5px solid #8e44ad; padding: 15px 20px; margin-bottom: 20px; border-radius: 8px;",
    HTML('<p style="font-size:16px; color:#333; font-style:italic; margin: 0;">
      📊 Este quadro vem a partir do artigo escrito pelos autores <b>Isabelli Cruz</b> e <b>Tiago Gandra</b>, onde há a análise de casos de leptospirose em todos os municípios brasileiros no ano de 2010. 
      A influência das doenças em locais que não possuem essas instalações pode ser o foco, e observa-se como essa incidência diminui ao longo dos anos.
    </p>')
  ),
  
  checkboxGroupInput(
    inputId = "filtro_especies",
    label = "Filtrar por categoria:",
    choices = c("Instalações sanitárias", "Leptospirose"),
    selected = c("Instalações sanitárias", "Leptospirose"),
    inline = TRUE
  ),
  
  tabsetPanel(
    tabPanel("Gráfico Principal",
             plotOutput(
               outputId = "meu_grafico",
               brush = brushOpts(
                 id = "seleciona_area",
                 direction = "xy",
                 resetOnNew = FALSE
               )
             ),
             br(),
             radioButtons("formato_grafico", "Formato do gráfico:",
                          choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg"),
                          inline = TRUE),
             downloadButton("baixar_grafico", "Baixar gráfico principal")
    ),
    
    tabPanel("Dados Selecionados",
             tableOutput("pontos_selecionados"),
             downloadButton("baixar_csv", "Baixar dados em CSV"),
             br(),
             radioButtons("formato_tabela", "Formato da tabela:",
                          choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg"),
                          inline = TRUE),
             downloadButton("baixar_tabela_grafica", "Baixar tabela como imagem")
    ),
    
    tabPanel("Segundo Gráfico",
             plotOutput("segundo_grafico"),
             br(),
             radioButtons("formato_segundo", "Formato do gráfico:",
                          choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg"),
                          inline = TRUE),
             downloadButton("baixar_segundo", "Baixar segundo gráfico")
    ),
    
    tabPanel("Mapa com Leaflet",
             leafletOutput("mapa_exemplo", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  iris_modificada <- iris %>%
    filter(Species != "setosa") %>%
    mutate(Species = recode(Species,
                            "versicolor" = "Instalações sanitárias",
                            "virginica" = "Leptospirose"))
  
  dados_filtrados_especie <- reactive({
    req(input$filtro_especies)
    subset(iris_modificada, Species %in% input$filtro_especies)
  })
  
  dados_filtrados <- reactive({
    req(input$seleciona_area)
    dados <- dados_filtrados_especie()
    
    subset(dados,
           Sepal.Length >= input$seleciona_area$xmin & Sepal.Length <= input$seleciona_area$xmax &
             Petal.Length >= input$seleciona_area$ymin & Petal.Length <= input$seleciona_area$ymax)
  })
  
  output$meu_grafico <- renderPlot({
    dados <- dados_filtrados_especie()
    
    ggplot(dados, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
      geom_point(size = 3, position = position_jitter(width = 20, height = 0)) +
      scale_x_continuous(name = "Número de Casos", limits = c(0, 100)) +
      scale_y_continuous(name = "Ano (2010)",
                         breaks = 1:12,
                         labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
      theme_minimal() +
      labs(
        title = "Distribuição das Amostras por Categoria",
        subtitle = "Visualização da dispersão de casos por mês e por categoria em 2010.",
        color = "Categoria"
      )
  })
  
  output$pontos_selecionados <- renderTable({
    dados_filtrados()
  })
  
  output$baixar_csv <- downloadHandler(
    filename = function() {
      paste("dados_selecionados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados_filtrados(), file, row.names = FALSE)
    }
  )
  
  output$baixar_tabela_grafica <- downloadHandler(
    filename = function() {
      paste0("tabela_dados_", Sys.Date(), ".", input$formato_tabela)
    },
    content = function(file) {
      dados <- dados_filtrados()
      if (nrow(dados) == 0) return(NULL)
      tabela <- tableGrob(dados)
      ggsave(file, tabela, device = input$formato_tabela, width = 10, height = 6)
    }
  )
  
  output$baixar_grafico <- downloadHandler(
    filename = function() {
      paste0("grafico_principal_", Sys.Date(), ".", input$formato_grafico)
    },
    content = function(file) {
      dados <- dados_filtrados_especie()
      grafico <- ggplot(dados, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
        geom_point(size = 3, position = position_jitter(width = 20, height = 0)) +
        scale_x_continuous(name = "Número de Casos", limits = c(0, 100)) +
        scale_y_continuous(name = "Ano (2010)",
                           breaks = 1:12,
                           labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
        theme_minimal() +
        labs(
          title = "Distribuição das Amostras por Categoria",
          subtitle = "Visualização da dispersão de casos por mês e por categoria em 2010.",
          color = "Categoria"
        )
      ggsave(filename = file, plot = grafico, device = input$formato_grafico, width = 8, height = 6, dpi = 300)
    }
  )
  
  output$segundo_grafico <- renderPlot({
    dados <- dados_filtrados()
    if (nrow(dados) == 0) return(NULL)
    
    ggplot(dados, aes(x = Sepal.Width, y = Petal.Width, color = Species)) +
      geom_point(size = 3, position = position_jitter(width = 20, height = 0)) +
      scale_x_continuous(name = "Número de Casos", limits = c(0, 100)) +
      scale_y_continuous(name = "Ano (2010)",
                         breaks = 1:12,
                         labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
      theme_minimal() +
      labs(
        title = "Análise Comparativa de Larguras",
        subtitle = "Casos distribuídos por largura das variáveis entre as categorias.",
        color = "Categoria"
      )
  })
  
  output$baixar_segundo <- downloadHandler(
    filename = function() {
      paste0("segundo_grafico_", Sys.Date(), ".", input$formato_segundo)
    },
    content = function(file) {
      dados <- dados_filtrados()
      grafico2 <- ggplot(dados, aes(x = Sepal.Width, y = Petal.Width, color = Species)) +
        geom_point(size = 3, position = position_jitter(width = 20, height = 0)) +
        scale_x_continuous(name = "Número de Casos", limits = c(0, 100)) +
        scale_y_continuous(name = "Ano (2010)",
                           breaks = 1:12,
                           labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
        theme_minimal() +
        labs(
          title = "Análise Comparativa de Larguras",
          subtitle = "Casos distribuídos por largura das variáveis entre as categorias.",
          color = "Categoria"
        )
      ggsave(filename = file, plot = grafico2, device = input$formato_segundo, width = 8, height = 6, dpi = 300)
    }
  )
  
  # --- MAPA USANDO SHAPEFILE DOS ESTADOS + PONTOS ALEATÓRIOS ---
  estados <- read_state(year = 2010, simplified = TRUE)
  
  set.seed(123)
  pontos_ficticios <- estados %>%
    st_centroid(of_largest_polygon = TRUE) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(
      estado = estados$name_state,
      casos_1 = sample(10:100, n(), replace = TRUE),
      casos_2 = sample(10:100, n(), replace = TRUE),
      lat2 = Y + runif(n(), -0.5, 0.5),
      lon2 = X + runif(n(), -0.5, 0.5)
    )
  
  pontos_plot <- rbind(
    data.frame(lat = pontos_ficticios$Y, lon = pontos_ficticios$X,
               estado = pontos_ficticios$estado, casos = pontos_ficticios$casos_1),
    data.frame(lat = pontos_ficticios$lat2, lon = pontos_ficticios$lon2,
               estado = pontos_ficticios$estado, casos = pontos_ficticios$casos_2)
  )
  
  output$mapa_exemplo <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = estados, fillColor = "transparent", color = "#666", weight = 1) %>%
      addCircleMarkers(
        lng = pontos_plot$lon,
        lat = pontos_plot$lat,
        radius = 6,
        color = "#d73027",
        stroke = TRUE,
        fillOpacity = 0.8,
        popup = paste0("<strong>Estado: </strong>", pontos_plot$estado,
                       "<br><em>Casos simulados: </em>", pontos_plot$casos)
      )
  })
}

shinyApp(ui = ui, server = server)

#-------------------
#NOVO LAYOUT 22/05/2025
#-------------------
# Tentando arrumar as proporções do gráfico 

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(shinyWidgets)
library(shinythemes)

# --- Dados Provisórios Expandidos ---
# Gerando dados provisórios mais abrangentes para simular um cenário real
# Inclui múltiplos anos e municípios fictícios com coordenadas aproximadas
set.seed(123) # Para reprodutibilidade

# Lista de municípios e suas coordenadas aproximadas (apenas para demonstração)
municipios_coords <- data.frame(
  Municipio = c("Rio de Janeiro", "São Paulo", "Belo Horizonte", "Porto Alegre", "Salvador", "Fortaleza"),
  Lat = c(-22.9068, -23.5505, -19.9167, -30.0346, -12.9714, -3.7319),
  Lon = c(-43.1729, -46.6333, -43.9345, -51.2177, -38.5108, -38.5267)
)

# Gerar dados para múltiplos anos (2013 a 2023) e municípios
anos <- 2013:2023
meses <- month.name
doencas <- c("Leptospirose", "Esquistossomose")

dados_provisorios <- expand.grid(
  Mes = meses,
  Doenca = doencas,
  Ano = anos,
  Municipio = municipios_coords$Municipio
) %>%
  as_tibble() %>%
  mutate(
    Casos = round(runif(n(), min = 10, max = 200)) # Casos aleatórios
  )

# UI (User Interface)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML(
      "
      body {
        background-color: #FFFFFF;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        padding-top: 70px; /* Adiciona padding ao body para o conteúdo não ficar por baixo do título fixo */
      }

      /* Removendo estilos específicos da navbar para focar no título */
      .navbar {
        display: none; /* Esconde completamente a navbar gerada pelo navbarPage */
      }

      /* Estilo para o título principal da aplicação */
      .main-app-title {
        color: #000000;
        font-size: 22px;
        font-weight: bold;
        text-align: center;
        margin: auto;
        max-width: 80%;
        border-radius: 12px;
        padding: 10px;
        display: block;
        background-color: rgba(240, 255, 255, 0.6); /* Sua cor azul com opacidade */
        pointer-events: none;
        cursor: default;
        /* --- Propriedades para fixar o título --- */
        position: fixed;
        top: 0;
        left: 0; /* Garante que comece no canto esquerdo */
        right: 0; /* Garante que termine no canto direito */
        width: 100%; /* Ocupa a largura total */
        z-index: 1030; /* Garante que fique acima de outros elementos */
        box-shadow: 0px 2px 5px rgba(0,0,0,0.1); /* Opcional: Adiciona uma leve sombra para destacar */
        padding-bottom: 20px; /* Ajuste o padding para espaçamento interno */
      }

      .container-fluid {
        display: flex;
        flex-direction: column;
        align-items: center;
      }

      .well {
        background-color: transparent;
        border: none;
        box-shadow: none;
      }

      #graficoPrincipal, #graficoSelecionado, #mapa {
        border: 1px solid #B0E0E6;
        border-radius: 10px;
        padding: 10px;
        background-color: white;
        margin-bottom: 30px;
        box-shadow: 0px 0px 10px rgba(176,224,230, 0.4);
      }

      /* Ajuste para o contêiner do gráfico principal e do mapa */
      #graficoPrincipal-container, #mapa-container {
        width: 95%; /* Quase a largura total da tela */
        max-width: 1400px; /* Um valor alto para que ele se estique bem em monitores maiores */
      }

      /* Ajuste para o contêiner do segundo gráfico */
      .chart-container {
        width: 90%;
        max-width: 1000px;
      }

      #graficoPrincipal-title, #graficoSelecionado-title, #mapa-title {
        font-weight: bold;
        font-size: 18px;
        text-align: center;
        margin-bottom: 10px;
      }

      .legend {
        font-size: 16px;
      }

      .plotly .legend {
        font-size: 16px !important;
      }
      "
    ))
  ),
  
  fluidPage(
    div(class = "main-app-title",
        "Plataforma de Informações Espaciais (PIE) de análise temporal de doenças hídricas relacionadas ao saneamento no Brasil: estudo de casos de leptospirose e esquistossomose de 2013 a 2023."
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(12, align = "center",
             selectInput("ano", "Selecione o Ano:", choices = anos, selected = 2013)
      )
    ),
    fluidRow(
      column(12, id = "graficoPrincipal-container", # Adicionado um ID para controle de largura específico
             div(id = "graficoPrincipal-title", textOutput("graficoPrincipalTitle")),
             plotlyOutput("graficoPrincipal", height = "650px", width = "auto"), # Use "auto" para que plotly respeite o container
             br(),
             div("Downloads do gráfico principal:", style = "font-weight: bold; margin-bottom: 5px;"),
             downloadButton("baixarGrafico", "Baixar PNG"),
             downloadButton("baixarGraficoPDF", "Baixar PDF")
      )
    ),
    fluidRow(
      column(6,
             selectInput("municipio", "Selecione o município:", choices = unique(dados_provisorios$Municipio)),
             selectInput("mes", "Selecione o mês:", choices = month.name),
             br(),
             div("Downloads do gráfico por município e mês:", style = "font-weight: bold; margin-bottom: 5px;"),
             downloadButton("baixarSegundoGrafico", "Baixar PNG"),
             downloadButton("baixarSegundoGraficoPDF", "Baixar PDF")
      ),
      column(6, class = "chart-container",
             div(id = "graficoSelecionado-title", textOutput("graficoSelecionadoTitle")),
             plotlyOutput("graficoSelecionado", height = "500px")
      )
    ),
    br(),
    fluidRow(
      column(12, id = "mapa-container", # Adicionado um ID para controle de largura específico
             div(id = "mapa-title", textOutput("mapaTitle")),
             leafletOutput("mapa", height = "750px", width = "auto") # Use "auto" para que leaflet respeite o container
      )
    ),
    br(),
    br(),
    div("Download geral dos dados:", style = "font-weight: bold; margin-bottom: 5px;"),
    downloadButton("baixarCSV", "Baixar CSV Geral")
  )
)

# Server (O código do server permanece o mesmo, pois as mudanças são apenas na UI/CSS)
server <- function(input, output, session) {
  
  # Título dinâmico para o gráfico principal
  output$graficoPrincipalTitle <- renderText({
    paste("Casos de Leptospirose e Esquistossomose em", input$ano)
  })
  
  # Título dinâmico para o gráfico selecionado
  output$graficoSelecionadoTitle <- renderText({
    paste("Casos em", input$municipio, "em", input$mes)
  })
  
  # Título dinâmico para o mapa
  output$mapaTitle <- renderText({
    paste("Distribuição de Casos por Município em", input$ano)
  })
  
  # Dados filtrados reativos com base no ano selecionado
  dados_filtrados_ano <- reactive({
    req(input$ano)
    dados_provisorios %>%
      filter(Ano == input$ano)
  })
  
  # Gráfico Principal (tendência mensal para o ano selecionado) - AGORA BARRAS VERTICAIS
  output$graficoPrincipal <- renderPlotly({
    dados <- dados_filtrados_ano()
    
    # Agrega os dados por Mês e Doenca para o gráfico
    dados_agregados_principal <- dados %>%
      group_by(Mes, Doenca) %>%
      summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Mes = factor(Mes, levels = month.name)) # Garante a ordem correta dos meses
    
    p <- ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) + # Barras verticais, lado a lado
      scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
      labs(y = "Total de Casos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Aumenta o tamanho do texto do eixo X
            axis.title.y = element_text(size = 12), # Aumenta o tamanho do texto do título do eixo Y
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p) %>%
      layout(autosize = TRUE) # Adiciona layout autosize para plotly
  })
  
  # Gráfico Selecionado (barras para município e mês específicos)
  output$graficoSelecionado <- renderPlotly({
    req(input$municipio, input$mes, input$ano)
    dados <- dados_provisorios %>% # Usa dados_provisorios para filtrar por ano, mes e municipio
      filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
    
    p <- ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
      labs(y = "Casos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p) %>%
      layout(autosize = TRUE) # Adiciona layout autosize para plotly
  })
  
  # Mapa Interativo
  output$mapa <- renderLeaflet({
    req(input$ano)
    
    # Agrega os dados por município para o ano selecionado
    dados_mapa <- dados_provisorios %>%
      filter(Ano == input$ano) %>%
      group_by(Municipio) %>%
      summarise(Total_Casos = sum(Casos, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(municipios_coords, by = "Municipio") # Junta com as coordenadas
    
    # Cria a paleta de cores para os círculos
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = dados_mapa$Total_Casos
    )
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -47.9292, lat = -15.7801, zoom = 4) %>% # Centraliza no Brasil
      addCircles(
        data = dados_mapa,
        lng = ~Lon,
        lat = ~Lat,
        radius = ~sqrt(Total_Casos) * 1000, # Raio proporcional à raiz quadrada dos casos para melhor visualização
        weight = 1,
        color = "#777777",
        fillColor = ~pal(Total_Casos),
        fillOpacity = 0.7,
        popup = ~paste0("<b>", Municipio, "</b><br/>",
                        "Total de Casos (", input$ano, "): ", Total_Casos)
      ) %>%
      addLegend(
        pal = pal,
        values = dados_mapa$Total_Casos,
        title = "Total de Casos",
        position = "bottomright"
      )
  })
  
  # Download CSV Geral
  output$baixarCSV <- downloadHandler(
    filename = function() paste0("dados_completos_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(dados_provisorios, file, row.names = FALSE) # Baixa todos os dados provisórios
    }
  )
  
  # Download Gráfico Principal PNG
  output$baixarGrafico <- downloadHandler(
    filename = function() paste0("grafico_principal_", input$ano, "_", Sys.Date(), ".png"),
    content = function(file) {
      # Aumenta a largura e altura para o download PNG
      png(file, width = 1600, height = 900, units = "px", res = 96) # Dimensões maiores para download
      dados <- dados_filtrados_ano()
      dados_agregados_principal <- dados %>%
        group_by(Mes, Doenca) %>%
        summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Mes = factor(Mes, levels = month.name))
      
      print(ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
              geom_bar(stat = "identity", position = "dodge", width = 0.7) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos de Leptospirose e Esquistossomose em", input$ano),
                   y = "Total de Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  # Download Gráfico Principal PDF
  output$baixarGraficoPDF <- downloadHandler(
    filename = function() paste0("grafico_principal_", input$ano, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Aumenta a largura e altura para o download PDF
      pdf(file, width = 18, height = 11) # Dimensões maiores para download
      dados <- dados_filtrados_ano()
      dados_agregados_principal <- dados %>%
        group_by(Mes, Doenca) %>%
        summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Mes = factor(Mes, levels = month.name))
      
      print(ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
              geom_bar(stat = "identity", position = "dodge", width = 0.7) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos de Leptospirose e Esquistossomose em", input$ano),
                   y = "Total de Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  # Download Segundo Gráfico PNG
  output$baixarSegundoGrafico <- downloadHandler(
    filename = function() paste0("grafico_municipio_mes_", input$municipio, "_", input$mes, "_", input$ano, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 800, height = 500, units = "px", res = 96)
      req(input$municipio, input$mes, input$ano)
      dados <- dados_provisorios %>%
        filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
      print(ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
              geom_bar(stat = "identity", width = 0.6) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos em", input$municipio, "em", input$mes, "de", input$ano),
                   y = "Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  # Download Segundo Gráfico PDF
  output$baixarSegundoGraficoPDF <- downloadHandler(
    filename = function() paste0("grafico_municipio_mes_", input$municipio, "_", input$mes, "_", input$ano, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 10, height = 8)
      req(input$municipio, input$mes, input$ano)
      dados <- dados_provisorios %>%
        filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
      print(ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
              geom_bar(stat = "identity", width = 0.6) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos em", input$municipio, "em", input$mes, "de", input$ano),
                   y = "Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
}

shinyApp(ui, server)

#TENTANDO ARRUMAR O PROBLEMA DO GRÁFICO 

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(shinyWidgets)
library(shinythemes)

# --- Dados Provisórios Expandidos ---
set.seed(123)

municipios_coords <- data.frame(
  Municipio = c("Rio de Janeiro", "São Paulo", "Belo Horizonte", "Porto Alegre", "Salvador", "Fortaleza"),
  Lat = c(-22.9068, -23.5505, -19.9167, -30.0346, -12.9714, -3.7319),
  Lon = c(-43.1729, -46.6333, -43.9345, -51.2177, -38.5108, -38.5267)
)

anos <- 2013:2023
meses <- month.name
doencas <- c("Leptospirose", "Esquistossomose")

dados_provisorios <- expand.grid(
  Mes = meses,
  Doenca = doencas,
  Ano = anos,
  Municipio = municipios_coords$Municipio
) %>%
  as_tibble() %>%
  mutate(
    Casos = round(runif(n(), min = 10, max = 200))
  )

# UI (User Interface)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML(
      "
      body {
        background-color: #FFFFFF;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        padding-top: 70px;
      }

      .navbar {
        display: none;
      }

      .main-app-title {
        color: #000000;
        font-size: 22px;
        font-weight: bold;
        text-align: center;
        margin: auto;
        max-width: 80%;
        border-radius: 12px;
        padding: 10px;
        display: block;
        background-color: rgba(240, 255, 255, 0.6);
        pointer-events: none;
        cursor: default;
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        width: 100%;
        z-index: 1030;
        box-shadow: 0px 2px 5px rgba(0,0,0,0.1);
        padding-bottom: 20px;
      }

      /* Centraliza o conteúdo da página */
      .container-fluid {
        display: flex;
        flex-direction: column;
        align-items: center; /* Centraliza horizontalmente as fluidRows */
      }

      /* Estilo dos contêineres dos gráficos (os 'quadrados azuis') */
      /* Agora eles terão a largura controlada aqui */
      #graficoPrincipal, #mapa {
        border: 1px solid #B0E0E6;
        border-radius: 10px;
        padding: 10px;
        background-color: white;
        margin-bottom: 30px;
        box-shadow: 0px 0px 10px rgba(176,224,230, 0.4);
        width: 100%; /* Ocupa 100% da largura da coluna pai */
        max-width: 1400px; /* AUMENTADO NOVAMENTE */
        margin-left: auto; /* Centraliza dentro da coluna */
        margin-right: auto; /* Centraliza dentro da coluna */
      }

      #graficoSelecionado {
        border: 1px solid #B0E0E6;
        border-radius: 10px;
        padding: 10px;
        background-color: white;
        margin-bottom: 30px;
        box-shadow: 0px 0px 10px rgba(176,224,230, 0.4);
        width: 100%; /* Ocupa 100% da largura da coluna pai */
        max-width: 800px; /* AUMENTADO NOVAMENTE */
        margin-left: auto; /* Centraliza dentro da coluna */
        margin-right: auto; /* Centraliza dentro da coluna */
      }

      /* Garante que os próprios gráficos Plotly e Leaflet preencham 100% de seus contêineres */
      .plotly.html-widget.plotly-graph-div,
      .leaflet.html-widget {
        width: 100% !important; /* Preenche a largura do contêiner (#graficoPrincipal, #mapa, #graficoSelecionado) */
        /* Removendo o max-width daqui, pois ele já está nos elementos pais */
      }

      #graficoPrincipal-title, #graficoSelecionado-title, #mapa-title {
        font-weight: bold;
        font-size: 18px;
        text-align: center;
        margin-bottom: 10px;
      }

      .legend {
        font-size: 16px;
      }

      .plotly .legend {
        font-size: 16px !important;
      }
      "
    ))
  ),
  
  fluidPage(
    div(class = "main-app-title",
        "Plataforma de Informações Espaciais (PIE) de análise temporal de doenças hídricas relacionadas ao saneamento no Brasil: estudo de casos de leptospirose e esquistossomose de 2013 a 2023."
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(12, align = "center",
             selectInput("ano", "Selecione o Ano:", choices = anos, selected = 2013)
      )
    ),
    # --- Gráfico Principal: Casos Anuais por Doença ---
    fluidRow(
      column(12,
             div(id = "graficoPrincipal-title", textOutput("graficoPrincipalTitle")),
             plotlyOutput("graficoPrincipal", height = "600px", width = "auto"),
             br(),
             div("Downloads do gráfico principal:", style = "font-weight: bold; margin-bottom: 5px;"),
             downloadButton("baixarGrafico", "Baixar PNG"),
             downloadButton("baixarGraficoPDF", "Baixar PDF")
      )
    ),
    # --- Gráfico por Município e Mês ---
    fluidRow(
      column(6,
             selectInput("municipio", "Selecione o município:", choices = unique(dados_provisorios$Municipio)),
             selectInput("mes", "Selecione o mês:", choices = month.name),
             br(),
             div("Downloads do gráfico por município e mês:", style = "font-weight: bold; margin-bottom: 5px;"),
             downloadButton("baixarSegundoGrafico", "Baixar PNG"),
             downloadButton("baixarSegundoGraficoPDF", "Baixar PDF")
      ),
      column(6,
             div(id = "graficoSelecionado-title", textOutput("graficoSelecionadoTitle")),
             plotlyOutput("graficoSelecionado", height = "450px", width = "auto")
      )
    ),
    br(),
    # --- Mapa de Distribuição de Casos ---
    fluidRow(
      column(12,
             div(id = "mapa-title", textOutput("mapaTitle")),
             leafletOutput("mapa", height = "700px", width = "auto")
      )
    ),
    br(),
    br(),
    div("Download geral dos dados:", style = "font-weight: bold; margin-bottom: 5px;"),
    downloadButton("baixarCSV", "Baixar CSV Geral")
  )
)

# Server (Mantido o mesmo)
server <- function(input, output, session) {
  
  output$graficoPrincipalTitle <- renderText({
    paste("Casos de Leptospirose e Esquistossomose em", input$ano)
  })
  
  output$graficoSelecionadoTitle <- renderText({
    paste("Casos em", input$municipio, "em", input$mes)
  })
  
  output$mapaTitle <- renderText({
    paste("Distribuição de Casos por Município em", input$ano)
  })
  
  dados_filtrados_ano <- reactive({
    req(input$ano)
    dados_provisorios %>%
      filter(Ano == input$ano)
  })
  
  output$graficoPrincipal <- renderPlotly({
    dados <- dados_filtrados_ano()
    
    dados_agregados_principal <- dados %>%
      group_by(Mes, Doenca) %>%
      summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Mes = factor(Mes, levels = month.name))
    
    p <- ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
      # Ajuste para barras grossas com bom espaçamento no novo tamanho horizontal
      geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.85) +
      scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
      labs(y = "Total de Casos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p) %>%
      layout(autosize = TRUE)
  })
  
  output$graficoSelecionado <- renderPlotly({
    req(input$municipio, input$mes, input$ano)
    dados <- dados_provisorios %>%
      filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
    
    p <- ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
      # Ajuste para barras grossas
      geom_bar(stat = "identity", width = 0.85) +
      scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
      labs(y = "Casos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p) %>%
      layout(autosize = TRUE)
  })
  
  output$mapa <- renderLeaflet({
    req(input$ano)
    
    dados_mapa <- dados_provisorios %>%
      filter(Ano == input$ano) %>%
      group_by(Municipio) %>%
      summarise(Total_Casos = sum(Casos, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(municipios_coords, by = "Municipio")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = dados_mapa$Total_Casos
    )
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -47.9292, lat = -15.7801, zoom = 4) %>%
      addCircles(
        data = dados_mapa,
        lng = ~Lon,
        lat = ~Lat,
        radius = ~sqrt(Total_Casos) * 1000,
        weight = 1,
        color = "#777777",
        fillColor = ~pal(Total_Casos),
        fillOpacity = 0.7,
        popup = ~paste0("<b>", Municipio, "</b><br/>",
                        "Total de Casos (", input$ano, "): ", Total_Casos)
      ) %>%
      addLegend(
        pal = pal,
        values = dados_mapa$Total_Casos,
        title = "Total de Casos",
        position = "bottomright"
      )
  })
  
  output$baixarCSV <- downloadHandler(
    filename = function() paste0("dados_completos_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(dados_provisorios, file, row.names = FALSE)
    }
  )
  
  output$baixarGrafico <- downloadHandler(
    filename = function() paste0("grafico_principal_", input$ano, "_", Sys.Date(), ".png"),
    content = function(file) {
      # Aumentando a largura para o download PNG
      png(file, width = 1600, height = 700, units = "px", res = 96)
      dados <- dados_filtrados_ano()
      dados_agregados_principal <- dados %>%
        group_by(Mes, Doenca) %>%
        summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Mes = factor(Mes, levels = month.name))
      
      print(ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
              geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.85) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos de Leptospirose e Esquistossomose em", input$ano),
                   y = "Total de Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  output$baixarGraficoPDF <- downloadHandler(
    filename = function() paste0("grafico_principal_", input$ano, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Aumentando a largura para o download PDF
      pdf(file, width = 18, height = 9)
      dados <- dados_filtrados_ano()
      dados_agregados_principal <- dados %>%
        group_by(Mes, Doenca) %>%
        summarise(Casos_Total = sum(Casos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Mes = factor(Mes, levels = month.name))
      
      print(ggplot(dados_agregados_principal, aes(x = Mes, y = Casos_Total, fill = Doenca)) +
              geom_bar(stat = "identity", position = position_dodge(width = 0.95), width = 0.85) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos de Leptospirose e Esquistossomose em", input$ano),
                   y = "Total de Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  output$baixarSegundoGrafico <- downloadHandler(
    filename = function() paste0("grafico_municipio_mes_", input$municipio, "_", input$mes, "_", input$ano, "_", Sys.Date(), ".png"),
    content = function(file) {
      # Aumentando a largura para o download PNG
      png(file, width = 1200, height = 500, units = "px", res = 96)
      req(input$municipio, input$mes, input$ano)
      dados <- dados_provisorios %>%
        filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
      print(ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
              geom_bar(stat = "identity", width = 0.85) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos em", input$municipio, "em", input$mes, "de", input$ano),
                   y = "Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
  
  output$baixarSegundoGraficoPDF <- downloadHandler(
    filename = function() paste0("grafico_municipio_mes_", input$municipio, "_", input$mes, "_", input$ano, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Aumentando a largura para o download PDF
      pdf(file, width = 14, height = 8)
      req(input$municipio, input$mes, input$ano)
      dados <- dados_provisorios %>%
        filter(Ano == input$ano, Mes == input$mes, Municipio == input$municipio)
      print(ggplot(dados, aes(x = Doenca, y = Casos, fill = Doenca)) +
              geom_bar(stat = "identity", width = 0.85) +
              scale_fill_manual(values = c("Leptospirose" = "#808000", "Esquistossomose" = "#F0E68C")) +
              labs(title = paste("Casos em", input$municipio, "em", input$mes, "de", input$ano),
                   y = "Casos") +
              theme_minimal() +
              theme(legend.position = "bottom",
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5, face = "bold")))
      dev.off()
    }
  )
}

shinyApp(ui, server)