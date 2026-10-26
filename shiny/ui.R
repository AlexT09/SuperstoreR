# --- Archivo: ui.R ---

library(shinydashboard)

# Define la UI (Interfaz de Usuario)
ui <- dashboardPage(skin = "blue",

  # 1. Cabecera del Dashboard
  dashboardHeader(
    title = tagList(
      # Usamos tags$img para añadir la imagen del logo
      tags$img(src = "logo.svg", height = "38", style = "padding-right:10px;"),
      # Título de la App como texto simple
      "Proyección de ventas Sample Superstore"
    ),
    titleWidth = 450
  ),

  # 2. Barra Lateral de Navegación
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Análisis Exploratorio", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Entrenamiento del Modelo", tabName = "entrenamiento", icon = icon("gears")),
      menuItem("Predicciones", tabName = "predicciones", icon = icon("laptop"))
    )
  ),

  # 3. Cuerpo del Dashboard
  dashboardBody(

    # --- CSS para centrar el contenido de las valueBox ---
    tags$head(
      tags$style(HTML("
        .small-box .inner {
          text-align: center;
        }
        .small-box h3, .small-box p {
          text-align: center;
        }
      "))
    ),

    tabItems(
      # Contenido de la Pestaña 1: Análisis Exploratorio
      tabItem(tabName = "eda",
              h2("Análisis Exploratorio de Datos"),

              # Fila de Cajas con Métricas (Value Boxes)
              fluidRow(
                box(
                  width = 12,
                  title = "Resumen del Dataset",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fluidRow(
                    column(3, valueBox(format(nrow(df), big.mark = ","), "Líneas de pedido", width = 12, color = "navy")),
                    column(3, valueBox(ncol(df), "Variables", width = 12, color = "navy")),
                    column(3, valueBox(paste0("$", round(sum(df$Sales) / 1e6, 2), " M"), "Ventas totales 2014–2017", width = 12, color = "navy")),
                    column(3, valueBox(sum(is.na(df)), "Valores Faltantes", width = 12, color = "navy"))
                  )
                )
              ),

              # Fila con Controles y Gráfico
              fluidRow(
                # Caja de Controles
                box(
                  title = "Configuración de Gráficos", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("variable", "Selecciona una variable:",
                              choices = c(
                                "Categoría" = "Category",
                                "Región" = "Region",
                                "Segmento" = "Segment",
                                "Subcategoría" = "Subcategoria",
                                "Modo de envío" = "Envio",
                                "Ventas (Sales)" = "Sales",
                                "Descuento" = "Discount",
                                "Cantidad" = "Quantity"
                              )
                  ),
                  selectInput(
                    "plot_type",
                    "Selecciona el tipo de gráfico:",
                    choices = c(
                      "Histograma / Gráfico de Barras",
                      "Gráfico de Cajas (Box Plot)"
                    ),
                    selected = "Histograma / Gráfico de Barras"
                  ),
                  sliderInput("rango_anio", "Filtra por Año del Pedido:",
                              min = min(df$Anio), max = max(df$Anio),
                              value = c(min(df$Anio), max(df$Anio)), step = 1, sep = "")
                ),
                # Caja para el Gráfico
                box(
                  title = "Gráfico Interactivo", status = "success", solidHeader = TRUE, width = 8,
                  # Usamos plotlyOutput para un gráfico interactivo
                  plotlyOutput("distPlot")
                )
              ),

              # Fila para la Tabla de Datos
              fluidRow(
                box(
                  title = "Tabla de Datos", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("tablaDatos")
                )
              )
      ),

      # Contenido de la Pestaña 2: Entrenamiento
      tabItem(tabName = "entrenamiento",
              h2("Entrenamiento de un Modelo Predictivo"),
              fluidRow(
                # Caja de control
                box(
                  title = "Control de Entrenamiento", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("modelo_seleccionado", "Selecciona un Modelo:",
                              choices = c(
                                "Regresión Lineal" = "lm",
                                "Bosque Aleatorio (Random Forest)" = "rf"
                              )),
                  sliderInput("train_split_percentage",
                              "Porcentaje de Meses para Entrenamiento:",
                              min = 50, max = 90, value = 67, step = 1, post = "%"),

                  actionButton("entrenarModelo", "Entrenar Modelo", class = "btn-success", icon = icon("play")),
                  helpText("Los meses se dividen en orden: los primeros entrenan el modelo y los últimos lo",
                           "prueban, así se evalúa como si se proyectara el futuro."),
                  hr(),
                  actionButton("guardar_modelo", "Guardar Modelo", class = "btn-default", icon = icon("floppy-disk")),
                  helpText("Guarda el modelo entrenado como archivo .rds en la carpeta de la app.")
                ),
                # Caja de resultados
                box(
                  title = "Resultados del Modelo", status = "info", solidHeader = TRUE, width = 8, collapsible = TRUE,

                  # Fila para las métricas de texto
                  fluidRow(
                    column(width = 12,
                           h4("Métricas Principales"),
                           verbatimTextOutput("metricas_modelo")
                    )
                  ),

                  # Fila para los gráficos, uno al lado del otro
                  fluidRow(
                    column(width = 6,
                           h4("Real vs. Predicho (total por mes)"),
                           plotlyOutput("real_vs_predicho")
                    ),
                    column(width = 6,
                           h4("Importancia de Variables"),
                           plotlyOutput("importancia")
                    )
                  )
                )
              )
      ),

      # Contenido de la Pestaña 3: Predicciones
      tabItem(tabName = "predicciones",
              h2("Realizar una Predicción"),
              fluidRow(
                # Caja de Atributos
                box(
                  title = "Ingresa los datos de la serie:", status = "primary", solidHeader = TRUE, width = 4,

                  h4("Cargar Ejemplos:"),
                  fluidRow(
                    column(6, actionButton("cargar_temporada_alta", "Temporada Alta", class = "btn-success", width = "100%")),
                    column(6, actionButton("cargar_temporada_baja", "Temporada Baja", class = "btn-warning", width = "100%"))
                  ),
                  hr(), # Separador visual

                  # Inputs para los datos de la serie
                  selectInput("pred_Category", "Categoría:", choices = levels(df$Category)),
                  selectInput("pred_Region", "Región:", choices = levels(df$Region)),
                  sliderInput("pred_mes", "Mes a predecir (1 = enero, 12 = diciembre):", min = 1, max = 12, value = 11),
                  sliderInput("pred_prom_12m", "Promedio mensual de los últimos 12 meses (USD):",
                              min = 100, max = max_ventas, value = 8000, step = 100),
                  sliderInput("pred_lag_12", "Ventas del mismo mes del año anterior (USD):",
                              min = 0, max = max_ventas, value = 10000, step = 100),
                  sliderInput("pred_lag_1", "Ventas del mes anterior (USD):",
                              min = 0, max = max_ventas, value = 8000, step = 100),
                  sliderInput("pred_prom_3m", "Promedio de los últimos 3 meses (USD):",
                              min = 0, max = max_ventas, value = 8000, step = 100),

                  actionButton("predecir", "Predecir", class = "btn-primary btn-lg", icon = icon("chart-line"), width = "100%")
                ),

                # Caja de Resultados
                box(
                  title = "Resultado de la Predicción", status = "success", solidHeader = TRUE, width = 8,

                  # Salida para el infoBox dinámico
                  uiOutput("resultado_pred_box"),

                  hr(), # Separador visual

                  # Salida para el gráfico de contexto
                  h4("Contexto de los Datos Ingresados"),
                  plotlyOutput("contexto_plot")
                )
              )
      )
    )
  )
)
