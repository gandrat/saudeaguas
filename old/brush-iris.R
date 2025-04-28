library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Selecione os pontos no gráfico e exporte para CSV"),
  
  # Gráfico com seleção por brush
  plotOutput(
    outputId = "meu_grafico",
    brush = brushOpts(
      id = "seleciona_area",
      direction = "xy",
 # Eu, Isabelli, arrumei essa parte que estava TRUE para que pudesse continuar 
# mostrando enquanto clicasse :D, de toda forma fiz com a ajuda do OpenAI
      resetOnNew = FALSE
    )
  ),
  
  tableOutput("pontos_selecionados"),
  
  downloadButton("baixar_csv", "Baixar dados selecionados")
)

server <- function(input, output) {
  
  # Reactive com dados filtrados pela seleção
  dados_filtrados <- reactive({
    req(input$seleciona_area)
    
    xmin <- input$seleciona_area$xmin
    xmax <- input$seleciona_area$xmax
    ymin <- input$seleciona_area$ymin
    ymax <- input$seleciona_area$ymax
    
    subset(iris,
           Sepal.Length >= xmin & Sepal.Length <= xmax &
             Petal.Length >= ymin & Petal.Length <= ymax)
  })
  
  # Gráfico com destaque dos pontos selecionados
  output$meu_grafico <- renderPlot({
    dados <- iris
    dados$selecionado <- FALSE
    
    if (!is.null(input$seleciona_area)) {
      area <- input$seleciona_area
      dados$selecionado <- with(dados,
                                Sepal.Length >= area$xmin & Sepal.Length <= area$xmax &
                                  Petal.Length >= area$ymin & Petal.Length <= area$ymax)
    }
    
    ggplot(dados, aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point(aes(color = selecionado), size = 3) +
      scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "gray80")) +
      theme_minimal() +
      labs(color = "Selecionado")
  })
  
  # Mostra os dados selecionados
  output$pontos_selecionados <- renderTable({
    dados_filtrados()
  })
  
  # Download em CSV
  output$baixar_csv <- downloadHandler(
    filename = function() {
      paste("flores_selecionadas_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados_filtrados(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

