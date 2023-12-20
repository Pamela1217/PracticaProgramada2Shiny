library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(ggplot2)
library(gapminder)
library(readr)
library(plotly)

datos_lib <- read_xlsx("C:/Users/Admin/OneDrive/Documentos/Nueva carpeta/Programación/Nivel III/PracticaProgramada2Shiny/datos3/datos_libertad.xlsx")
ui <- dashboardPage(
  dashboardHeader(title = "Libertades Mundiales", titleWidth = 300),
  
  dashboardSidebar(
    selectInput("pais", "Selecciona un País", choices = unique(datos_lib$pais)),
    sliderInput("ano", "Selecciona el año:", 
                min = min(datos_lib$anio), 
                max = max(datos_lib$anio),
                value = c(min(datos_lib$anio), max(datos_lib$anio)),
                step = 1),
    radioButtons("data_type", "Selecciona tipo de datos:", choices = c("Ranking", "Puntaje")),
    downloadButton("download_data", "Descargar datos")
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Libertad Humana", plotOutput("plotHumana")),
      tabPanel("Libertad Personal", plotOutput("plotPersonal")),
      tabPanel("Libertad Económica", plotOutput("plotEconomica"))
    )
  )
)


server <- function(input, output) {
  
  
  datos_filtrados <- reactive({
    filter(datos_lib, pais == input$pais, anio >= input$ano[1] & anio <= input$ano[2])
  })
  
  render_graph <- function(metrica, titulo) {
    ggplot(datos_filtrados(), aes(x = anio, y = get(metrica), group = pais)) +
      geom_line(color = "blue") +  
      ggtitle(paste(titulo, "-", input$data_type)) +
      ylab("Puntaje") +
      xlab("Año") +
      theme(legend.position = "none") 
  }
  
  output$plotHumana <- renderPlot({
    metrica <- ifelse(input$data_type == "Puntaje", "libertad_humana_puntaje", "libertad_humana_ranking")
    render_graph(metrica, "Libertad Humana")
  })
  
  output$plotPersonal <- renderPlot({
    metrica <- ifelse(input$data_type == "Puntaje", "libertad_personal_puntaje", "libertad_personal_ranking")
    render_graph(metrica, "Libertad Personal")
  })
  
  output$plotEconomica <- renderPlot({
    metrica <- ifelse(input$data_type == "Puntaje", "libertad_economica_puntaje", "libertad_economica_ranking")
    render_graph(metrica, "Libertad Económica")
  })
  
  output$download_data <- downloadHandler(
    filename = function() {paste("datos_", input$pais, ".pdf", sep = "")},
    content = function(file) {
      metrica <- ifelse(input$data_type == "Puntaje", paste("libertad", input$data_type, sep = ""), paste("libertad", input$data_type, sep = ""))
      pdf(file)
      print(render_graph(metrica, "Libertad"))
      dev.off()
    }
  )
}

shinyApp(ui, server)
