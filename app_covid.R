## COVID-2019 interactive mapping tool

## Made for represent COVID cases evolution for each city in Brazil
## Tiago Gandra (tiago.gandra@riogrande.ifrs.edu.br) and Christian Gobel (cfgobel@gmail.com)

## Adapted from
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

# load required packages
library('ggplot2')
library('rvest')
library('magrittr')
library('dplyr')
library('reshape2')
library('RColorBrewer')
library('ggiraph')
library('leaflet')
library('plotly')
library('maps')
# library('geojsonio')
library('shiny')
library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet.extras')
library('sf')
# library('jsonlite')
library('tidyr')
# library('sp')
library('classInt')





rm(list=ls()) #removin previous objects

options(scipen=999)

load('input_data/cv.rda')
# max(cv_cases$week)
cv_cases_state_week$geometry<-NULL

cv_min_date<-min(cv_cases$date)
cv_max_date<-max(cv_cases$date)
cv_min_week<-min(cv_cases$week)
cv_max_week<-max(cv_cases$week)
current_date<-Sys.Date()


# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"


# create plotting parameters for map

pal_deaths100k <- colorBin("Oranges", domain = cv_cases_state$deaths100k)
pal_cases100k <- colorBin("Oranges", domain = cv_cases_state$cases100k)
pal_svi <- colorQuantile("Reds", cv_today$svi, n = 5)
# cv_pal_per100 <- colorBin("Oranges", domain = cv_today_state$deaths100k)
# cv_pal <- colorBin("Oranges", domain = cv_today_state$death_rate)

#basemap ----
basemap = leaflet(option=leafletOptions(zoomControl=FALSE)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Mapa de Densidade de Mortes","Municípios: Mortes por 100 mil","Municípios: Índice de Vulnerabilidade Social (IVS)","Estados: Casos por 100 mil","Estados: Mortes por 100 mil"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Municípios: Mortes por 100 mil","Estados: Casos por 100 mil","Estados: Mortes por 100 mil","Municípios: Índice de Vulnerabilidade Social (IVS)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-50, -18, 4.4)
# basemap

# Plots----------
plot_df = cv_aggregated


# function to plot new cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  g1 = ggplot(plot_df, aes(x = date, y = cases7)) + 
    geom_line(color=covid_col) + 
    geom_point(data=plot_df%>%filter(date==plot_date),aes(x=date, y=cases7),size=3,position='stack',stat='identity',color=covid_col)+
    ylab("Novos casos") + xlab("") + theme_bw() + 
    # xlim(c(cv_min_date-1,cv_max_date+1)) + 
    # ylim(c(0,max(cv_aggregated$cases7)))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

deaths_cases_plot = function(cv_aggregated, plot_date) {
  g1 = ggplot(plot_df, aes(x = date, y = deaths7)) + 
    geom_line(color=covid_other_col) +
    geom_point(data=cv_aggregated%>%filter(date==plot_date),aes(x=date,y=deaths7),size=3,color=covid_other_col)+
    ylab("Novos óbitos") + xlab("") + theme_bw() + 
    # xlim(c(cv_min_date-1,cv_max_date+1)) + 
    # ylim(c(0,max(cv_aggregated$new_deaths)))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# #Plot by states-----------
cls = rep(c(brewer.pal(12, "Set3"), brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"),  brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
nome_state_cols = cls[1:length(unique(cv_cases_state_week$state))]
names(nome_state_cols) = unique(cv_cases_state_week$state)

# states_cases_plot = function(cv_cases_state_week) {
#   g1 = ggplot(cv_cases_state_week, aes(x = date, y = new_outcome, 
#                                        fill = with(cv_cases_state_week, reorder(state, -outcome)),
#                                        text = paste0(format(date, "%d %B %Y"), "\n", state, ": ",new_outcome))) +
#     geom_area(alpha=.5) +
#     ylab("Nº")+xlab(NULL) + theme_bw() +
#     scale_fill_manual(values=nome_state_cols) +
#     scale_x_date(labels = function(x) format(x, "%d-%b"))+
#     # scale_y_continuous(limits=c(0,max(new_outcome)+10),expand=c(0,0))+
#     theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))
#   ggplotly(g1, tooltip = c("text"))
# }

states_cases_plot_week = function(cv_cases_state_week) {
  g1 = ggplot(cv_cases_state_week, aes(x = date, y = new_outcome, group=state,
                                       fill=with(cv_cases_state_week, reorder(state, -outcome)),
                                       text = paste0(format(date, "%d %B %Y"), "\n", state, ": ",new_outcome))) +
    # geom_bar(position="stack", stat="identity") +
    geom_area(alpha=.5)+
    ylab("Nº")+ theme_bw() +
    scale_fill_manual(values=nome_state_cols) +
    scale_color_manual(values=nome_state_cols) +
    xlab("Data")+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10),
          axis.text.x = element_text(angle = 90))
  ggplotly(g1, tooltip = c("text"))
}

states_cases_cumulative = function(cv_cases_state_week) {
  g1 = ggplot(cv_cases_state_week, aes(x = date, y = outcome, color=with(cv_cases_state_week, reorder(state, -outcome)), group=state,
                               text = paste0(format(date, "%d %B %Y"), "\n", state, ": ",outcome))) +
    geom_line(alpha=0.5)+
    ylab("Cumulativo")+xlab(NULL) + theme_bw() +
    scale_colour_manual(values=nome_state_cols) +
    scale_x_date(labels = function(x) format(x, "%d-%b"))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text"))
}

# #Plots by cities-----------
cls = rep(c(brewer.pal(12, "Set3"), brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"),  brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
city_cols = cls[1:length(unique(cv_cases$city))]
names(city_cols) = unique(cv_cases$city)

mun_cases_plot = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = new_cases)) +
    # geom_bar(position="stack", stat="identity", fill=covid_col,alpha=.5) +
    geom_area(data=cv_cases,aes(x=date,y=cases7),fill=covid_col,alpha=.5)+
    ylab("Nº de Novos Casos")+xlab(NULL) + theme_bw() +
    scale_x_date(labels = function(x) format(x, "%d-%b"))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=12,hjust=0.5))
  g2 = ggplot(cv_cases, aes(x = date, y = new_deaths)) +
    # geom_bar(position="stack", stat="identity", fill=covid_other_col,alpha=.5) +
    geom_area(data=cv_cases,aes(x=date,y=deaths7),fill=covid_other_col,alpha=.5)+
    ylab("Nº de Novos Óbitos")+xlab(NULL) + theme_bw() +
    scale_x_date(labels = function(x) format(x, "%d-%b"))+
    ggtitle(paste(min(cv_cases$city),' - ',min(cv_cases$state),' (',max(cv_cases$cases), ' casos - ',max(cv_cases$deaths), ' óbitos).', sep=''))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=12,hjust=0.5))
  subplot(g1, g2, titleY=T,shareY=F,nrows=1, margin = .03)
}

# mun_cases_plot_week = function(cv_cases_week) {
#   g1 = ggplot(cv_cases_week, aes(x = date, y = new_cases)) +
#     geom_bar(position="stack", stat="identity",fill=covid_col) +
#     ylab("Nº de Casos")+xlab(NULL) + theme_bw() +
#     theme(legend.title = element_blank(), legend.position = "none", 
#           plot.title = element_text(size=12,hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1))
# 
#   g2 = ggplot(cv_cases_week, aes(x = date, y = new_deaths)) +
#     geom_bar(position="stack", stat="identity",fill=covid_other_col) +
#     ylab("Nº de Óbitos")+xlab('Data') + theme_bw() +
#     theme(legend.title = element_blank(), legend.position = "none", 
#           plot.title = element_text(size=12,hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1))
# 
#   subplot(g1, g2, titleY=T,titleX=T,shareY=F,nrows=1, margin = .03)
# }

mun_cases_cumulative = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = cases)) +
    geom_area(fill=covid_col,alpha=.5)+
    ylab("Nº de Casos (Cumulativo)")+xlab(NULL) + theme_bw() +
    scale_x_date(labels = function(x) format(x, "%d-%b"))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=12,hjust=0.5))
  g2 = ggplot(cv_cases, aes(x = date, y = deaths)) +
    geom_area(fill=covid_other_col,alpha=.5)+
    ylab("Nº de Óbitos (Cumulativo)")+xlab(NULL) + theme_bw() +
    scale_x_date(labels = function(x) format(x, "%d-%b"))+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=12,hjust=0.5))

  subplot(g1, g2, titleY=T,shareY=F,nrows=1, margin = .03)
}


#UI-----
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 title="PIE-COVID", id="nav",
                 
                 ##Maps Panel-----
                 tabPanel("Mapa dos Casos",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 100, left  = 10, width = 300, fixed=TRUE,
                                            draggable = T, height = "auto",
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_city_count"), align = "right"),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            plotOutput("deaths_plot", height="130px", width="100%"),
                                            span(h6(textOutput("reactive_case_count_row"), align = "right"), style="color:#662506"),
                                            
                                            
                                            sliderInput("plot_date",
                                                        label = h5("Selecione a data"),
                                                        min = as.Date(min(cv_cases$date),"%Y-%m-%d"),
                                                        max = as.Date(max(cv_cases$date),"%Y-%m-%d"),
                                                        value = as.Date(cv_max_date),
                                                        step=7,
                                                        timeFormat = "%d-%m-%y",
                                                        animate=animationOptions(interval = 1000, loop = FALSE))
                              )
                              
                          )
                 ),
                 ##States Panel-------------------
                 tabPanel("Gráficos por Estados",

                          sidebarLayout(
                            sidebarPanel(

                              pickerInput("outcome_select", "Tipo:",
                                          choices = c("Casos",'Mortes'),
                                          selected = c("Casos"),
                                          multiple = FALSE),

                              pickerInput("state_select", "Estados:",
                                          choices = as.character(cv_today_state[order(-cv_today_state$cases),]$state),
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_cases_state_week$state,
                                          multiple = TRUE),
                              "Selecione os Estados para atualizar os gráficos."
                            ),


                            mainPanel(
                              tabsetPanel(
                                tabPanel("Por semana", plotlyOutput("state_plot_week")),
                                # tabPanel("Por dia", plotlyOutput("state_plot")),
                                tabPanel("Cumulativo", plotlyOutput("state_plot_cumulative"))
                                # tabPanel("Cumulativo (Dia 0)", plotlyOutput("state_plot_cumulative_day0")),
                                # tabPanel("Escala Log", plotlyOutput("state_plot_cumulative_log"), div(br(),'As retas representam casos (ou óbitos) dobrando a cada dia (tracejado), a cada dois dias (pontilhado) e a cada semana (traço e ponto).'))
                              )
                            )
                          )

                 ),
                 ##Cities Panel----------

                 tabPanel("Gráficos por Municípios",

                          sidebarLayout(
                            sidebarPanel(
                              # pickerInput("mun_outcome_select", "Tipo:",
                              #             choices = c("Casos",'Mortes'),
                              #             selected = c("Casos"),
                              #             multiple = FALSE),
                                  htmlOutput("state_selector"),
                                  htmlOutput("muni_selector"),
                              "Selecione o Município para atualizar os gráficos."
                            ),


                            mainPanel(
                              plotlyOutput("mun_plot"),
                              # plotlyOutput("mun_plot_week"),
                              plotlyOutput("mun_plot_cumulative")
                              
                            )
                          )

                 ),
                 # #Downloads Panel-----------
                 tabPanel("Download",
                          numericInput("maxrows", "Rows to show", 10),
                          verbatimTextOutput("rawtable"),
                          downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                          "Adaptado da linha do tempo publicada por ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series",
                                                                             "Johns Hopkins Center for Systems Science and Engineering.")
                 ),
                 
                 tabPanel("Sobre este site",
                          tags$div(
                            tags$h4("Última Atualização"), 
                            h6(paste0(max(cv_cases$date))),
                            tags$h3("Detalhes"), 
                            "As informações apresentadas neste site baseiam-se em dados oficiais sobre a evolução da epidemia do COVID-19 no Brasil. Todavia, os resultados aqui divulgados são fruto de modelos científicos e não necessariamente representam a realidade, pois fatores não previstos podem influenciar a expansão do contágio. Embora os autores vislumbrem um bom potencial no uso desses resultados para a tomada de decisões visando a gestão da epidemia, convém reafirmar que incertezas são inerentes ao processo de modelagem preditiva e que as informações aqui apresentadas devem ser consideradas um ensaio acadêmico e sempre interpretadas com a devida cautela.",tags$br(),tags$br(),
                            "O cálculo do Índice de Vulnerabilidade Social frente ao COVID-19 levou em consideração os seguintes critérios:",tags$br(),
                            "1. Percentual de pessoas com mais de 60 anos;",tags$br(),
                            "2. Percentual de população urbana;",tags$br(), 
                            "3. Número de moradores por domicílio;",tags$br(), 
                            "4. Renda média;",tags$br(),
                            "5. Nº de leitos hospitalares por habitante (fevereiro de 2020).",tags$br(),
                            
                            tags$h4("Fontes de Dados:"),
                            "Nº de casos de COVID por municípios: ",tags$a(href="https://brasil.io/dataset/covid19/caso", " Brasil.io.", target="_blank"),tags$br(),
                            "Variáveis do Censo Demográfico: ",tags$a(href="https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html", "IBGE",target="_blank"),tags$br(),
                            "Nº de leitos: ",tags$a(href="http://www2.datasus.gov.br/DATASUS/index.php?area=0204&id=1479586&VObj=http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/leiuti", "DATASUS", target="_blank"),tags$br(),tags$br(),
                            "O modelo da plataforma foi desenvolvido por ",tags$a(href="https://www.lshtm.ac.uk/aboutus/people/parker.edward", "Edward Parker", target="_blank"), ", disponível neste", 
                            tags$a(href="http://vac-lshtm.shinyapps.io/ncov_tracker/", "website.", target="_blank"),tags$br(),
                            
                            tags$h3("Autor"),
                            "Dr. Tiago Gandra",tags$br(),
                            "Professor do Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul",tags$br(),
                            "Laboratório de Geotecnologias e Meio Ambiente (GEOMA)",tags$br(),
                            "<tiago.gandra@riogrande.ifrs.edu.br>",tags$br(),
                            tags$a(href="http://lattes.cnpq.br/8370478243309846", "Curriculum Vitae", target="_blank"),tags$br(),
                            tags$h3("Colaboradores"),
                            "Dr. Christian Göbel",tags$br(),
                            "Pesquisador da Universidade Federal do Rio Grande (FURG)",tags$br(),
                            tags$a(href="http://lattes.cnpq.br/0033281844355855", "Curriculum Vitae", target="_blank"),tags$br(),tags$br(),
                            tags$h4("Índice de Vulnerabilidade Social ao COVID-19 (IVS)"),
                            "Dr. Jarbas Bonetti",tags$br(),
                            "Professor da Universidade Federal de Santa Catarina (stateSC)",tags$br(),
                            "Laboratório de Oceanografia Costeira (LOC)",tags$br(),
                            tags$a(href="http://lattes.cnpq.br/0024793279904352", "Curriculum Vitae", target="_blank"),tags$br(),tags$br(),
                            "Dra. Carla Bonetti",tags$br(),
                            "Professora da Universidade Federal de Santa Catarina (stateSC)",tags$br(),
                            "Laboratório de Oceanografia Costeira (LOC)",tags$br(),
                            tags$a(href="http://lattes.cnpq.br/1678574965365327", "Curriculum Vitae", target="_blank"),tags$br(),tags$br(),
                            "Msc. Cibele Lima",tags$br(),
                            "Pós-Graduanda da Universidade Federal de Santa Catarina (stateSC)",tags$br(),
                            "Laboratório de Oceanografia Costeira (LOC)",tags$br(),
                            tags$a(href="http://lattes.cnpq.br/9973999081212467", "Curriculum Vitae", target="_blank"),tags$br(),tags$br(),tags$br()
                            
                          )
                 )
                 
                 
)



#Server-----
server = function(input, output) {
  ##Selecao de municipios-----------
  
  output$state_selector = renderUI({ #creates State select box object called in ui
    pickerInput(inputId="state_select2",label = "Estado:", #label displayed in ui
                choices = unique(cv_today_state$state)[order(-cv_today_state$pop)],
                selected = 'RS'
    ) 
  })

  output$muni_selector = renderUI({
    
    if (is.null(input$state_select2)) {
      return(NULL)
    }
    
    muni_filter <- cv_today %>% filter(state == input$state_select2)
    # muni_filter <- cv_today %>% filter(state == 'RS')
    #creates a reactive list of available counties based on the State selection made
    
    pickerInput(inputId = "mun_select", #name of input
                label = "Município:", #label displayed in ui
                choices = unique(muni_filter$city)[order(-muni_filter$pop)],
                selected = 'RIO GRANDE'
    )
  })
  
  # Map tab ----------
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%Y-%m-%d")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date) 
  })
  
  reactive_db_state = reactive({
    cv_cases_state %>% filter(date == input$plot_date) 
  })
  
  reactive_db_ind = reactive({
    cv_aggregated %>% filter(date == input$plot_date)
  })

  output$reactive_case_count <- renderText({
    paste0(format(sum(reactive_db_ind()$cases), big.mark=".", decimal.mark=",",1), " casos")
  })
  
  output$reactive_death_count <- renderText({
    paste0(formatC(sum(reactive_db_ind()$deaths), big.mark=".", decimal.mark=","), " mortes")
  })
  
  output$reactive_city_count <- renderText({
    paste0(nrow(subset(reactive_db())), " municípios afetados.")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })

  #Leaflet Map-------------
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearGroup("Mapa de Densidade de Mortes")%>%
      
      addPolygons(data =reactive_db_state(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.5,
                  # label = sprintf("<strong>%s </strong><br/>Casos confirmado: %g<br/>Casos por 100 mil: %g<br/>Nº de mortes: %g<br/>População: %g milhões<br/>Nº de Leitos UTI (Total): %g<br/>Nº de Leitos UTI (SUS): %g<br/> ",
                                  # cv_today_state$state, cv_today_state$cases, cv_today_state$cases100k,cv_today_state$deaths,round(cv_today_state$pop/1000000,2),round(cv_today_state$leitosh,0),round(cv_today_state$leitos_sus,0)) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                    textsize = "15px", direction = "auto"),
                  fillColor = ~pal_deaths100k(deaths100k),
                  group="Estados: Mortes por 100 mil")%>%


      addPolygons(data =reactive_db_state(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.5,
                  label = sprintf("<strong>%s </strong><br/>Casos confirmado:
                                  %g<br/>Casos por 100 mil: %g<br/>
                                  Nº de mortes: %g<br/>
                                  População: %g milhões<br/> ",
                                  reactive_db_state()$state,
                                  reactive_db_state()$cases,
                                  reactive_db_state()$cases100k,
                                  reactive_db_state()$deaths,
                                  round(reactive_db_state()$pop/1000000,2)) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                    textsize = "15px", direction = "auto"),
                  fillColor = ~pal_cases100k(cases100k),
                  group="Estados: Casos por 100 mil")%>%

      addCircleMarkers(data = reactive_db(), lat = ~ lat, lng = ~ lon, weight = 1, radius = ~deaths100k^(1/3),
                       fillOpacity = 0.3, color = covid_col, group = "Municípios: Mortes por 100 mil",
                       label = sprintf("<strong>%s </strong><br/>Casos confirmado: %g<br/>
                                 Casos por 100 mil: %g<br/>
                                 Nº de mortes: %g<br/>
                                 Mortes por 100 mil: %g<br/>
                                 População: %s<br/>",
                                       reactive_db()$city, 
                                       reactive_db()$cases, 
                                       reactive_db()$cases100k,
                                       reactive_db()$deaths,
                                       reactive_db()$deaths100k,
                                       format(reactive_db()$pop,big.mark=".", decimal.mark=",",1))%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")
      )%>%
      
      addCircleMarkers(data = cv_today, lat = ~ lat, lng = ~ lon, weight = 1, radius=1, 
                       fillOpacity = 0.3, color=~pal_svi(svi),
                       group = "Municípios: Índice de Vulnerabilidade Social (IVS)",
                       label = sprintf("<strong>%s </strong><br/>Casos confirmado: %g<br/>
                                 Casos por 100 mil: %g<br/>
                                 Nº de mortes: %g<br/>
                                 Mortes por 100 mil: %g<br/>
                                 População: %s<br/>
                                       IVS: %s<br/>",
                                       cv_today$city,
                                       cv_today$cases,
                                       cv_today$cases100k,
                                       cv_today$deaths,
                                       cv_today$deaths100k,
                                       format(cv_today$pop,big.mark=".", decimal.mark=",",1),
                                       cv_today$svi)%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")
      )%>%
      
      addHeatmap(data = reactive_db(), lat = ~ lat, lng = ~ lon, intensity = ~(log(deaths100k)),
                 radius=8, layerId = 1, max=log(max(reactive_db()$deaths100k)),
                 group = "Mapa de Densidade de Mortes"
      )
  

  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  output$deaths_plot <- renderPlot({
    deaths_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # create dataframe states-----------
  state_reactive_db = reactive({
    if (input$outcome_select=="Casos") {
      cv_cases_state_week$outcome = cv_cases_state_week$cases
      cv_cases_state_week$new_outcome = cv_cases_state_week$new_cases
    }
    if (input$outcome_select=="Mortes") {
      cv_cases_state_week$outcome = cv_cases_state_week$deaths
      cv_cases_state_week$new_outcome = cv_cases_state_week$new_deaths
    }
    cv_cases_state_week %>% filter(state %in% input$state_select)
  })
  # create dataframe Cities-----------
  mun_reactive_db = reactive({
      cv_cases %>% filter(city == input$mun_select & state==input$state_select2 
                          & new_deaths>=0 & deaths7>=0)
  })

  # State plots--------------
  output$state_plot <- renderPlotly({
    states_cases_plot(state_reactive_db())
  })

  output$state_plot_week <- renderPlotly({
    states_cases_plot_week(state_reactive_db()%>%
                             group_by(state,week)%>%
                             summarise(outcome=sum(outcome),
                                       new_outcome=sum(new_outcome),
                                       date=max(date)))
  })

  output$state_plot_cumulative <- renderPlotly({
    states_cases_cumulative(state_reactive_db())
  })

  output$state_plot_cumulative_log <- renderPlotly({
    states_cases_cumulative_log(state_reactive_db())
  })
  output$state_plot_cumulative_day0 <- renderPlotly({
    state_cases_cumulative_day0(state_reactive_db())
  })

  # cities plots----------
  output$mun_plot <- renderPlotly({
    mun_cases_plot(mun_reactive_db())
  })

  # output$mun_plot_week <- renderPlotly({
  #   mun_cases_plot_week(mun_reactive_db()%>%
  #                         group_by(week, city)%>%
  #                         summarise(new_cases=sum(new_cases),
  #                                   new_deaths=sum(new_deaths),
  #                                   date=max(date)))
  # })

  output$mun_plot_cumulative <- renderPlotly({
    mun_cases_cumulative(mun_reactive_db())
  })
  output$mun_plot_cumulative_log <- renderPlotly({
    mun_cases_cumulative_log(mun_reactive_db())
  })
  output$mun_plot_cumulative_day0 <- renderPlotly({
    mun_cases_cumulative_day0(mun_reactive_db())
  })





  # output to download data------
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(city_code, city, state_code, state, date, cases, new_cases, deaths, new_deaths,
                                      cases100k, pop)), file)
    }
  )

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(cv_cases %>% select(c(city_code, city, state_code, state, date, cases, new_cases, deaths, new_deaths,
                                                     cases100k, pop)), input$maxrows), row.names = FALSE)
    options(orig)
  })
}

shinyApp(ui, server)

