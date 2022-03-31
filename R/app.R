library(shiny)
library(shinydashboard)
library(tidyverse) 
library(shinythemes) 
library(highcharter)
library(zoo) 
library(readxl) 
library(shinyWidgets)
library(leaflet)
library(sf)
library(viridis) 
library(RColorBrewer)
library(dplyr)
library(htmlwidgets)
library(leaflet.providers) 
library(haven) 
library(leaflet) 
library(leaflet.extras)
library(rworldxtra) 
library(raster) 
library(paletteer)
library(rcartocolor)

inhabilitacion_shiny <- read_excel("www/inhabilitacion_shiny.xlsx")
multas_shiny <- read_excel("www/multas_shiny.xlsx")
cluster_fshiny <- read_excel("www/cluster_fshiny.xlsx")
c_mean_income <- read_excel("www/c_mean_income.xlsx")


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Datatón Anticorrupción 2021", id = "intro",
                 
                 tabPanel("Introducción",
                          box(
                            background = "purple",width = 12, solidHeader = TRUE,
                            h1("Datatón Anticorrupción 2021", align = "center"),
                          ),
                          br(""),
                          h2("Terdashianos", align = "center"),
                          h3("Este trabajo es desarrollado como una propesta para el Datatón,
                              nos concentramos en analizar las sanciones impuestas a servidores públicos,
                             dividimos el traajo en tres partes principales: ", align = "center"),
                          br(""),
                          h3("-En primer lugar se observan las inhabilitaciones promedio según la causa de sanción"),
                          h3("-Después se analiza la dispersión entre el ingreso declarado y las multas impuestas por puestos ejercidos"),
                          h3("-Por último se desarrolla un modelo de ML para crear clusters en los datos y observar patrones")
                          
                          
                          ),
                 tabPanel("Inhabilitaciones",
                          column(4, 
                            fluidRow(
                              pickerInput("picker_dep_i", "Área de la Dependencia", choices = c(unique(inhabilitacion_shiny$dependencia)),
                                          options = list(`actions-box` = FALSE),multiple = FALSE)
                              )
                          ),
                          fluidRow(
                            highchartOutput("bar_inhabi", height = "80vh", width = "80vh"), width=10, 
                          )
                          ),
                 tabPanel("Multas",
                          column(4, 
                                 fluidRow(
                                   pickerInput("picker_dep_m", "Área de la Dependencia", choices = c(unique(multas_shiny$dependencia)),
                                               options = list(`actions-box` = FALSE),multiple = FALSE)
                                 )
                          ),
                          column(4, 
                                 fluidRow(
                                   pickerInput("picker_cau_m", "Causa de la Sanción", choices = c(unique(multas_shiny$causa)),
                                               options = list(`actions-box` = FALSE),multiple = FALSE)
                                 )
                          ),
                          fluidRow(
                            highchartOutput("scatter_multas", height = "80vh", width = "80vh"), width=8, 
                          )
                          ),
                 tabPanel("Clusters Método",
                          HTML('
                           <section class="hero">
  <div class="container-fluid">
    <div class="row">
      <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
        <h1>Modelo de Clusters</h1>
        <h2>Evaluación y Resultados</h2>
        <h3>Como última parte desarrollamos un modelo de K-means que ayuda a determinar clusters en los datos basándose en centroides, Para determinar los clusters se usó la dependencia, el tipo de sanción, la causa de la sanción, género e ingreso</h3>
                           </div>
                             </div>
                             </div>
                             </section>
                           '),
                          fluidRow(
                              column(6,
                              HTML('<p><img src="elbone_score.jpg"/></p>')
                            ),
                            box(title = "Elbone Score", status = "primary", width = 4,  solidHeader = TRUE,
                                p("Para determinar la cantidad óptima de clusters se pueden realizar dos
                                  dos pruebas principales, la primera llamada Elbone Score indica el
                                  óptimo en donde la recta sea curva, para este caso se presenta entre 2 y 5"),
                            )
                          ),
                          fluidRow(
                            column(6,
                                   HTML('<p><img src="silhouette_score.jpg"/></p>')
                                   ),
                            box(title = "Silhouette Score", status = "primary", width = 4,  solidHeader = TRUE,
                                p("una forma más eficazpara determinar la cantidad de clusters es el método
                                  Silhouette Score, en conde el valor más bajo en este score determina el óptimo
                                  de clusters del modelo, para este caso la mejor opción parecen ser 4 Clusters
                                  y es la opción elegida"),
                            )
                          ),
                          fluidRow(
                            column(6,
                                   HTML('<p><img src="clusters.jpg"/></p>')
                            ),
                            box(title = "Clusters con K-means", status = "primary", width = 4,  solidHeader = TRUE,
                                p("Por último se aplica el modelo con 4 centroides que generen el mismo número
                                  de clusters, para simplificar este proceso primero se normalizaron los datos
                                  en un rango cerrrado entre 1 y -1, posteriormente Se descompusieron los datos 
                                  reduciendo su dimensionalidad, esto facilita el cálculo y hace más amena la visualización,
                                  A grandes rasgoz los datos parecen estar ya divididos en dos grupos y a partir de estos
                                  el modelo los fragmenta."),
                            )
                          )
                          
                          
                          ),
                 tabPanel("Clusters Resultado",
                          column(4, 
                                 fluidRow(
                                   pickerInput("picker_clus", "Cluster", choices = c(unique(cluster_fshiny$cluster)),
                                               options = list(`actions-box` = FALSE),multiple = FALSE)
                                 )
                          ),
                          column(4, 
                                 fluidRow(
                                   pickerInput("picker_clus_dep", "Dependencia", choices = c(unique(cluster_fshiny$dependencia)),
                                               selected = "Agricultura", options = list(`actions-box` = TRUE),multiple = TRUE)
                                 )
                          ),
                          fluidRow(
                            highchartOutput("bar_c_val", height = "90vh", width = "90vh"), width=10
                          ),
                          column(4, 
                                 fluidRow(
                                   pickerInput("picker_clus_income", "Cluster", choices = c(unique(c_mean_income$cluster)),
                                               options = list(`actions-box` = FALSE),multiple = FALSE)
                                 )
                          ),
                          column(4, 
                                 fluidRow(
                                     pickerInput("picker_clus_sanc", "Sanción", choices = c(unique(c_mean_income$sancion)),
                                               options = list(`actions-box` = FALSE),multiple = FALSE)
                                 )
                          ),
                          fluidRow(
                            highchartOutput("bar_c_inc", height = "90vh", width = "90vh"), width=10
                          )
                 )
)

server <- function(input, output, session) {
  
  observeEvent(c(input$picker_dep_i), {
    inahilitados <- inhabilitacion_shiny %>% filter(dependencia==input$picker_dep_i)
    
    updatePickerInput(session = session, inputId = "picker_cau_i",
                      choices=c(unique(inahilitados$causa)))
  })
  
  observeEvent(c(input$picker_clus), {
    c_1 <- cluster_fshiny %>% filter(cluster==input$picker_clus)
    
    updatePickerInput(session = session, inputId = "picker_clus_dep", selected = "Agricultura",
                      choices=c(unique(c_1$dependencia)))
  })
  
  observeEvent(c(input$picker_dep_m), {
    multas_1 <- multas_shiny %>% filter(dependencia==input$picker_dep_m)
    
    updatePickerInput(session = session, inputId = "picker_cau_m",
                      choices=c(unique(multas_1$causa)))
  })
  
  output$bar_inhabi<-renderHighchart({
    inhabi_out <- inhabilitacion_shiny %>% filter(dependencia==input$picker_dep_i)
    
    highchart() %>% 
      hc_title(
        text = "<b>Duración Promedio de la Inhabilitación por Causa</b>",
        style = list(color = "#17202A", useHTML = TRUE)
      ) %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = inhabi_out$causa) %>%
      hc_add_series(data = inhabi_out$san_hombres, name = "Hombres", color = "#138D75")%>%
      hc_add_series(data = inhabi_out$san_mujeres, name = "Mujeres", color = "#CD5C5C")%>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_bloom()
      )
  })
  
  output$scatter_multas<-renderHighchart({
    scat_multa1 <- multas_shiny %>% filter(dependencia==input$picker_dep_m) %>% filter(causa==input$picker_cau_m)
    
    hc <- multas_shiny %>% 
      hchart('scatter', hcaes(x = ingreso, y = multa, group = puesto)) %>%
      hc_colors(c("#196F3D", "#17A589", "#7D3C98", "#D35400", "#939693", "#27AE60", "#C0392B", "#34495E", "#FA8072"))%>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_bloom()
      )
  })
  
  output$bar_c_val<-renderHighchart({
    cluster_bar1 <- cluster_fshiny %>% filter(cluster==input$picker_clus )%>% filter(dependencia==input$picker_clus_dep)
    
    highchart() %>% 
      hc_title(
        text = "<b>Número de Personas que Comietieron Faltas</b>",
        style = list(color = "#17202A", useHTML = TRUE)
      ) %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = cluster_bar1$falta) %>%
      hc_add_series(data = cluster_bar1$Hombre, name = "Hombres", color = "#138D75")%>%
      hc_add_series(data = cluster_bar1$Mujer, name = "Mujeres", color = "#CD5C5C")%>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_bloom()
      )
    
  })
  
  output$bar_c_inc <- renderHighchart({
    cluster_bar2 <- c_mean_income %>% filter(cluster==input$picker_clus_income )%>% filter(sancion==input$picker_clus_sanc)
    
    highchart() %>% 
      hc_title(
        text = "<b>Ingresos Promedio por Puesto</b>",
        style = list(color = "#17202A", useHTML = TRUE)
      ) %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = cluster_bar2$puesto) %>%
      hc_add_series(data = cluster_bar2$Hombre, name = "Hombres", color = "#138D75")%>%
      hc_add_series(data = cluster_bar2$Mujer, name = "Mujeres", color = "#CD5C5C")%>%
      hc_exporting(
        enabled=TRUE
      )%>%
      hc_add_theme(
        hc_theme_bloom()
      )
  })


}


shinyApp(ui = ui, server = server)
