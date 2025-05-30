# -------------------- LIBRERÍAS --------------------



library(shiny)
library(dplyr)
library(WDI)
library(ggplot2)
library(plotly)
library(rsconnect)


# -------------------- CARGA Y PREPARACIÓN DE DATOS --------------------

# Indicadores base + adicionales
indicadores <- c(
  "NY.GDP.PCAP.CD",      # pib per cápita
  "NY.GNP.PCAP.CD",      # ingreso per cápita
  "BAR.SCHL.15UP",       # escolaridad
  "SL.UEM.TOTL.ZS",      # desempleo
  "SP.DYN.LE00.IN",      # esperanza de vida
  "SH.XPD.CHEX.GD.ZS",   # gasto en salud
  "SP.DYN.TFRT.IN",      # fertilidad
  "SP.POP.TOTL",         # población total
  "SE.XPD.TOTL.GD.ZS",   # gasto en educación
  "NY.GDP.MKTP.CD",      # pib total
  "SI.POV.DDAY"          # pobreza (% población con menos de $1.90)
  
)

# Descargar datos
datos <- WDI(country = "all", indicator = indicadores, start = 2000, end = 2020)

# Asignar nombres correctos
colnames(datos) <- c(
  "country", "iso2c", "iso3c", "year",
  "pib", "ingreso", "escolaridad", "desempleo",
  "esperanza_vida", "gasto_salud", "fertilidad",
  "poblacion", "gasto_educacion", "pib_total",
  "pobreza"
)

# Limpiar
datos$year <- as.integer(datos$year)
datos <- datos %>% filter(!is.na(year))



# -------------------- UI --------------------
ui <- fluidPage(
  titlePanel("📊 Dinámicas Económicas y Educativas - Dashboard Interactivo"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pais", "Selecciona un país:", choices = sort(unique(datos$country))),
      sliderInput("anio", "Selecciona un año:",
                  min = min(datos$year), max = max(datos$year),
                  value = max(datos$year), step = 1)
    ),
    
    mainPanel(
      
      tags$h4(strong("Relación entre PIB y crecimiento poblacional")),
      fluidRow(
        column(6, plotlyOutput("plotEconomia")),
        column(6, plotlyOutput("plotPoblacionSeleccion"))
      ),
      br(),
      
      tags$h4(strong("Inversión en servicios públicos clave")),
      fluidRow(
        column(6, plotlyOutput("plotGastoEducacion")),
        column(6, plotlyOutput("plotGastoSalud"))
      ),
      br(),
      
      tags$h4(strong("Educación, empleo y bienestar")),
      fluidRow(
        column(6, plotlyOutput("plotEducacionTrabajo")),
        column(6, plotlyOutput("plotEsperanzaVida"))
      ),
      br(),
      
      tags$h4(strong("Comparación entre países y evolución educativa")),
      fluidRow(
        column(6, plotlyOutput("plotComparativo")),
        column(6, plotlyOutput("plotEscolaridadSeleccion"))
      ),
      br(),
      
      tags$h4(strong("Desigualdad y salud pública")),
      fluidRow(
        column(6, plotlyOutput("plotPobreza")),
        column(6, plotlyOutput("plotSalud"))
      )
    )
    
  ) # <- cierre correcto de sidebarLayout
) # <- cierre correcto de fluidPage




# -------------------- SERVER --------------------
server <- function(input, output) {
  
  data_filtrada <- reactive({
    datos %>% filter(country == input$pais)
  })
  
  # Evolución del PIB per cápita
  output$plotEconomia <- renderPlotly({
    df <- data_filtrada()
    validate(need(nrow(df) > 0, "No hay datos disponibles para este país."))
    
    plot_ly(df, x = ~year) %>%
      add_lines(y = ~pib, name = "PIB per cápita", line = list(color = "blue")) %>%
      layout(
        title = paste("Evolución del PIB per cápita en", input$pais),
        yaxis = list(title = "PIB per cápita (USD)"),
        xaxis = list(title = "Año")
      )
  })
  
  # Educación y Trabajo
  output$plotEducacionTrabajo <- renderPlotly({
    df <- data_filtrada()
    validate(need(nrow(df) > 0, "No hay datos disponibles para este país."))
    
    plot_ly(df, x = ~year) %>%
      add_lines(y = ~escolaridad, name = "Escolaridad", line = list(color = "orange")) %>%
      add_lines(y = ~desempleo, name = "Desempleo (%)", line = list(color = "red")) %>%
      layout(
        title = paste("Educación y Trabajo en", input$pais),
        yaxis = list(title = "Porcentaje / Años"),
        xaxis = list(title = "Año")
      )
  })
  
  # Tasa de Fertilidad vs Gasto en Salud
  output$plotSalud <- renderPlotly({
    df <- data_filtrada()
    validate(need(nrow(df) > 0, "No hay datos disponibles para este país."))
    
    plot_ly(df, x = ~year) %>%
      add_lines(y = ~gasto_salud, name = "Gasto en salud (% PIB)", line = list(color = "brown")) %>%
      add_lines(y = ~fertilidad, name = "Tasa de fertilidad", line = list(color = "pink")) %>%
      layout(
        title = paste("Tasa de Fertilidad vs Gasto en Salud en", input$pais),
        yaxis = list(title = "Valores"),
        xaxis = list(title = "Año")
      )
  })
  
  # Esperanza de vida
  output$plotEsperanzaVida <- renderPlotly({
    df <- data_filtrada()
    validate(need(nrow(df) > 0, "No hay datos disponibles para este país."))
    
    plot_ly(df, x = ~year) %>%
      add_lines(y = ~esperanza_vida, name = "Esperanza de vida", line = list(color = "purple")) %>%
      layout(
        title = paste("Esperanza de Vida en", input$pais),
        yaxis = list(title = "Años"),
        xaxis = list(title = "Año")
      )
  })
  
  # PBI per cápita vs año de escolaridad
  output$plotComparativo <- renderPlotly({
    datos_comparados <- datos %>%
      filter(year == input$anio) %>%
      filter(!is.na(escolaridad), !is.na(pib), !is.na(fertilidad), !is.na(esperanza_vida))
    
    validate(need(nrow(datos_comparados) > 0, "No hay datos comparativos para este año."))
    
    plot_ly(datos_comparados, x = ~escolaridad, y = ~pib,
            size = ~esperanza_vida, color = ~escolaridad, type = 'scatter', mode = 'markers',
            text = ~paste("País:", country,
                          "<br>PIB:", round(pib),
                          "<br>Escolaridad:", round(escolaridad))) %>%
      layout(
        title = paste("PBI per cápita vs Año de Escolaridad en", input$anio),
        xaxis = list(title = "Años de Escolaridad"),
        yaxis = list(title = "PIB per cápita (USD)")
      )
  })
  
  # Gasto en Educación
  output$plotGastoEducacion <- renderPlotly({
    datos_pais <- datos %>%
      filter(country == input$pais, !is.na(gasto_educacion))
    
    validate(need(nrow(datos_pais) > 0, "No hay datos de gasto en educación."))
    
    plot_ly(datos_pais, x = ~year, y = ~gasto_educacion, type = 'bar', name = "Educación",
            marker = list(color = "orange")) %>%
      layout(
        title = paste("Gasto en Educación en", input$pais),
        yaxis = list(title = "% del PIB"),
        xaxis = list(title = "Año")
      )
  })
  
  # Gasto en Salud
  output$plotGastoSalud <- renderPlotly({
    datos_pais <- datos %>%
      filter(country == input$pais, !is.na(gasto_salud))
    
    validate(need(nrow(datos_pais) > 0, "No hay datos de gasto en salud."))
    
    plot_ly(datos_pais, x = ~year, y = ~gasto_salud, type = 'bar', name = "Salud",
            marker = list(color = "purple")) %>%
      layout(
        title = paste("Gasto en Salud en", input$pais),
        yaxis = list(title = "% del PIB"),
        xaxis = list(title = "Año")
      )
  })
  
  # Evolución de la pobreza
  output$plotPobreza <- renderPlotly({
    datos_pais <- datos %>%
      filter(country == input$pais, !is.na(pobreza))
    
    validate(need(nrow(datos_pais) > 0, "No hay datos de pobreza para este país."))
    
    plot_ly(datos_pais, x = ~year, y = ~pobreza,
            type = 'bar',
            name = "Pobreza",
            marker = list(color = "tomato")) %>%
      layout(
        title = paste("Evolución de la pobreza en", input$pais),
        xaxis = list(title = "Año"),
        yaxis = list(title = "% de población con < USD 1.90/día")
      )
  })
  
  # Años de Escolaridad seleccionados
  output$plotEscolaridadSeleccion <- renderPlotly({
    datos_filtrados <- datos %>%
      filter(country == input$pais,
             year %in% c(2000, 2005, 2010),
             !is.na(escolaridad))
    
    validate(need(nrow(datos_filtrados) > 0, "No hay datos de escolaridad para esos años."))
    
    plot_ly(datos_filtrados,
            x = ~factor(year),
            y = ~escolaridad,
            type = "bar",
            marker = list(color = "darkorange")) %>%
      layout(
        title = paste("Años Promedio de Escolaridad en", input$pais),
        xaxis = list(title = "Año"),
        yaxis = list(title = "Años de Escolaridad")
      )
  })
  
  # Población total
  output$plotPoblacionSeleccion <- renderPlotly({
    datos_filtrados <- datos %>%
      filter(country == input$pais, !is.na(poblacion))
    
    validate(need(nrow(datos_filtrados) > 0, "No hay datos de población para este país."))
    
    plot_ly(
      datos_filtrados,
      x = ~year,
      y = ~poblacion,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'steelblue', width = 2)
    ) %>%
      layout(
        title = paste("Evolución de la Población en", input$pais),
        xaxis = list(title = "Año"),
        yaxis = list(title = "Población")
      )
  })
  
}


# -------------------- INICIAR APP --------------------
shinyApp(ui = ui, server = server)



