library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(leaflet)
library(geobr)
library(sf)

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



