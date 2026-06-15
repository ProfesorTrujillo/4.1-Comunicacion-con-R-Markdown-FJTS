library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)
library(janitor)
library(tidyr)
library(shinybusy)

options(shiny.maxRequestSize = 200*1024^2)

# =========================
# UI
# =========================
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "911 CDMX Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "home", icon = icon("chart-line")),
      menuItem("Análisis Detallado", tabName = "analisis", icon = icon("table")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map"))
    ),
    
    fileInput("archivo", "Cargar CSV"),
    
    sliderInput("filtro_hora", "Hora", 0, 23, c(0,23)),
    
    uiOutput("filtro_alcaldia")
  ),
  
  dashboardBody(
    add_busy_spinner(),
    
    tabItems(
      
      # ======================
      # DASHBOARD PRINCIPAL
      # ======================
      tabItem(tabName = "home",
              
              fluidRow(
                valueBoxOutput("total"),
                valueBoxOutput("tiempo"),
                valueBoxOutput("falsa")
              ),
              
              fluidRow(
                box(plotlyOutput("tendencia"), width=12, title="Tendencia por hora")
              ),
              
              fluidRow(
                box(plotlyOutput("categorias"), width=6),
                box(plotlyOutput("top_alcaldias"), width=6)
              ),
              
              fluidRow(
                box(verbatimTextOutput("insights"), width=12, title="Insights automáticos")
              )
      ),
      
      # ======================
      # ANALISIS
      # ======================
      tabItem(tabName = "analisis",
              fluidRow(
                box(DTOutput("tabla_resumen"), width=12)
              )
      ),
      
      # ======================
      # MAPA
      # ======================
      tabItem(tabName = "mapa",
              box(leafletOutput("mapa", height=600), width=12)
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  detectar <- function(posibles, columnas){
    x <- intersect(posibles, columnas)
    if(length(x)>0) return(x[1]) else return(NA)
  }
  
  datos <- reactive({
    
    req(input$archivo)
    
    df <- read_csv(input$archivo$datapath) %>%
      clean_names()
    
    col_fecha <- detectar(c("fecha_creacion"), names(df))
    col_hora  <- detectar(c("hora_creacion"), names(df))
    col_fecha_cierre <- detectar(c("fecha_cierre"), names(df))
    col_hora_cierre  <- detectar(c("hora_cierre"), names(df))
    col_cat <- detectar(c("clas_con_f_alarma"), names(df))
    col_alcaldia <- detectar(c("alcaldia_cierre"), names(df))
    col_lat <- detectar(c("latitud"), names(df))
    col_lon <- detectar(c("longitud"), names(df))
    
    df %>%
      mutate(
        fecha_inicio = ymd_hms(paste(.data[[col_fecha]], .data[[col_hora]])),
        fecha_fin = ymd_hms(paste(.data[[col_fecha_cierre]], .data[[col_hora_cierre]])),
        hora = hour(fecha_inicio),
        categoria = .data[[col_cat]],
        alcaldia = .data[[col_alcaldia]],
        lat = as.numeric(.data[[col_lat]]),
        lon = as.numeric(.data[[col_lon]]),
        tiempo = as.numeric(difftime(fecha_fin, fecha_inicio, units="mins"))
      ) %>%
      filter(hora >= input$filtro_hora[1] & hora <= input$filtro_hora[2])
  })
  
  # =========================
  # FILTRO DINÁMICO
  # =========================
  output$filtro_alcaldia <- renderUI({
    req(datos())
    selectInput("alcaldia", "Alcaldía",
                choices = unique(datos()$alcaldia),
                selected = unique(datos()$alcaldia),
                multiple = TRUE)
  })
  
  datos_filtrados <- reactive({
    req(input$alcaldia)
    datos() %>% filter(alcaldia %in% input$alcaldia)
  })
  
  # =========================
  # KPIs
  # =========================
  output$total <- renderValueBox({
    valueBox(nrow(datos_filtrados()), "Total llamadas", icon=icon("phone"), color="blue")
  })
  
  output$tiempo <- renderValueBox({
    avg <- round(mean(datos_filtrados()$tiempo, na.rm=TRUE),2)
    valueBox(paste(avg,"min"), "Tiempo promedio", icon=icon("clock"), color="green")
  })
  
  output$falsa <- renderValueBox({
    pct <- round(mean(datos_filtrados()$categoria=="FALSA ALARMA", na.rm=TRUE)*100,2)
    valueBox(paste(pct,"%"), "Falsas alarmas", icon=icon("exclamation"), color="red")
  })
  
  # =========================
  # GRAFICAS PRO
  # =========================
  output$tendencia <- renderPlotly({
    df <- datos_filtrados() %>% count(hora)
    
    plot_ly(df, x=~hora, y=~n, type='scatter', mode='lines+markers') %>%
      layout(title="Llamadas por hora")
  })
  
  output$categorias <- renderPlotly({
    df <- datos_filtrados() %>% count(categoria)
    
    plot_ly(df, labels=~categoria, values=~n, type='pie')
  })
  
  output$top_alcaldias <- renderPlotly({
    df <- datos_filtrados() %>% count(alcaldia) %>% top_n(10)
    
    plot_ly(df, x=~n, y=~reorder(alcaldia,n), type='bar', orientation='h')
  })
  
  # =========================
  # MAPA
  # =========================
  output$mapa <- renderLeaflet({
    df <- datos_filtrados() %>% filter(!is.na(lat))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(lng=~lon, lat=~lat)
  })
  
  # =========================
  # TABLA
  # =========================
  output$tabla_resumen <- renderDT({
    datos_filtrados() %>%
      group_by(alcaldia, categoria) %>%
      summarise(total=n(), .groups="drop")
  })
  
  # =========================
  # INSIGHTS AUTOMATICOS
  # =========================
  output$insights <- renderText({
    
    df <- datos_filtrados()
    
    top_alcaldia <- df %>% count(alcaldia) %>% arrange(desc(n)) %>% slice(1)
    hora_pico <- df %>% count(hora) %>% arrange(desc(n)) %>% slice(1)
    
    paste(
      "Alcaldía con más incidentes:", top_alcaldia$alcaldia,
      "\nHora pico:", hora_pico$hora,
      "\nTotal de eventos:", nrow(df)
    )
  })
}

# =========================
# RUN
# =========================
shinyApp(ui, server)