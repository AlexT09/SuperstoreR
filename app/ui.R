# --- Archivo: ui.R ---
# Define la UI (Interfaz de Usuario): cabecera, menú lateral y una pestaña por etapa del proyecto.

ui <- dashboardPage(skin = "blue",

  # 1. Cabecera del Dashboard
  dashboardHeader(
    title = tagList(tags$img(src = "logo.svg", height = "30", style = "padding-right:8px;"),
                    "Ventas Superstore"),
    titleWidth = 270
  ),

  # 2. Barra Lateral de Navegación
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      id = "menu",
      menuItem("Resumen ejecutivo", tabName = "resumen", icon = icon("gauge-high"), selected = TRUE),
      tags$li(class = "header", "PROYECTO"),
      menuItem("Introducción", tabName = "introduccion", icon = icon("house")),
      menuItem("Problema y objetivos", tabName = "problema", icon = icon("bullseye")),
      menuItem("Marco teórico", tabName = "marco", icon = icon("book")),
      menuItem("Metodología", tabName = "metodologia", icon = icon("diagram-project")),
      tags$li(class = "header", "DATOS"),
      menuItem("ETL", tabName = "etl", icon = icon("database")),
      menuItem("Análisis exploratorio", icon = icon("chart-bar"),
               menuSubItem("Univariado", tabName = "eda_uni"),
               menuSubItem("Bivariado", tabName = "eda_bi"),
               menuSubItem("Ventas en el tiempo", tabName = "eda_tiempo"),
               menuSubItem("Atípicos y relación con el modelo", tabName = "eda_atip"),
               menuSubItem("Geografía y otros factores", tabName = "eda_geo"),
               menuSubItem("Explorador interactivo", tabName = "eda_explorador")),
      tags$li(class = "header", "MODELO"),
      menuItem("Modelos y resultados", tabName = "modelos", icon = icon("gears")),
      menuItem("Entrenamiento del modelo", tabName = "entrenamiento", icon = icon("play")),
      menuItem("Proyección y predicción", tabName = "proyeccion", icon = icon("chart-line")),
      tags$li(class = "header", "CIERRE"),
      menuItem("Conclusiones", tabName = "conclusiones", icon = icon("flag-checkered"))
    )
  ),

  # 3. Cuerpo del Dashboard
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", href = "custom.css"), tags$link(rel = "icon", href = "favicon.ico")),

    tabItems(

      # ------------------------------------------------------------ Resumen ejecutivo
      tabItem(tabName = "resumen",
        encabezado("Resumen ejecutivo", "Qué pasó en 2014–2017 y qué esperar en 2018, en una página"),
        if (!listo) fluidRow(aviso_sin_modelo()) else tagList(
          # Filtros: todo el resumen se recalcula según la selección
          fluidRow(box(status = "primary", width = 12, class = "barra-filtros",
            fluidRow(
              column(3, selectInput("res_cat", "Categoría:", c(TODAS, names(CATEGORY_COLORS)))),
              column(3, selectInput("res_reg", "Región:", c(TODAS, "Central", "East", "South", "West"))),
              column(6, helpText("Elige una categoría o una región para ver su resumen. Las cifras de 2018 son",
                                 "proyecciones del modelo, no ventas reales."))
            ))),
          # Cifras clave: valueBoxOutput + renderValueBox
          fluidRow(
            valueBoxOutput("res_kpi_proy", width = 3), valueBoxOutput("res_kpi_var", width = 3),
            valueBoxOutput("res_kpi_pico", width = 3), valueBoxOutput("res_kpi_error", width = 3)
          ),
          # ¿Cuánto y cuándo? + ¿qué significa?
          fluidRow(
            box(title = "¿Cuánto venderemos? Real 2015–2017 y proyección 2018", status = "primary", solidHeader = TRUE,
                width = 8, plotlyOutput("resumen_fig", height = "340px"), div(class = "texto-grafico", textOutput("res_lectura", inline = TRUE))),
            box(title = "Hallazgos clave de la empresa", status = "warning", solidHeader = TRUE, width = 4, lista_html(hallazgos))
          ),
          # ¿Dónde? + ¿cuándo reforzar?
          fluidRow(
            caja_grafico("¿Dónde crece la venta? 2017 vs. 2018", "res_barras",
                         "Barras azules: ventas reales de 2017; ámbar: proyección 2018. Encima, el crecimiento esperado. Si eliges una región, se compara por categoría.",
                         width = 6, alto = "320px", status = "success"),
            caja_grafico("¿Cuándo reforzar inventario? Calendario 2018", "res_calor",
                         "Cada celda es la venta proyectada del mes. Los tonos más intensos marcan cuándo reforzar inventario y personal.",
                         width = 6, alto = "320px", status = "warning")
          ),
          # ¿Qué hago?
          fluidRow(box(title = "Decisiones recomendadas", status = "success", solidHeader = TRUE, width = 12,
                       lista_html(RECOMENDACIONES[1:3], numerada = TRUE)))
        )
      ),

      # ------------------------------------------------------------ Introducción
      tabItem(tabName = "introduccion",
        encabezado("Introducción", "Del análisis de las ventas de Sample Superstore a su proyección con machine learning en R"),
        fluidRow(
          valueBox(num(nrow(df)), "Líneas de pedido (2014–2017)", icon = icon("receipt"), color = "navy", width = 3),
          valueBox(usd(sum(df$Sales) / 1e6, 2) %>% paste("M"), "Ventas totales 2014–2017", icon = icon("dollar-sign"), color = "blue", width = 3),
          valueBox(12, "Series: 3 categorías × 4 regiones", icon = icon("layer-group"), color = "light-blue", width = 3),
          valueBox(if (listo) usd(resumen$proyeccion_2018 / 1e3) %>% paste0("k") else "—", "Ventas proyectadas 2018",
                   icon = icon("chart-line"), color = "yellow", width = 3)
        ),
        fluidRow(
          box(title = "El proyecto", status = "primary", solidHeader = TRUE, width = 7,
              p("Sample Superstore es un conjunto de datos de muestra de una cadena minorista de Estados Unidos",
                "(2014–2017) —una simulación con fines analíticos— con cerca de 10.000 líneas de pedido de muebles,",
                "artículos de oficina y tecnología."),
              p("El proyecto tiene dos partes: un análisis exploratorio (EDA) que identifica qué explica el nivel de",
                "ventas, y un modelo de machine learning que usa esos hallazgos para proyectar cuánto venderá la",
                "empresa cada mes, por categoría y región.")),
          box(title = "Preguntas del proyecto", status = "info", solidHeader = TRUE, width = 5,
              p(tags$b("1. "), tags$b(PREGUNTAS[1])), p(tags$b("2. "), tags$b(PREGUNTAS[2])),
              helpText("Use el menú de la izquierda: sigue el flujo del proyecto, del planteamiento a las conclusiones."))
        ),
        fluidRow(
          box(title = "Contexto", status = "primary", solidHeader = TRUE, width = 6,
              p("Una cadena minorista vende a través de varias categorías de producto, regiones y segmentos de cliente,",
                "y sus ventas cambian a lo largo del año. Saber qué combinación de factores mueve las ventas, y",
                "anticipar cuánto se venderá, es un insumo directo para planificar: cuánto inventario comprar, qué",
                "metas fijar a cada región y cuándo reforzar la operación.")),
          box(title = "Impacto para el negocio", status = "success", solidHeader = TRUE, width = 6, lista_html(IMPACTO))
        )
      ),

      # ------------------------------------------------------------ Problema y objetivos
      tabItem(tabName = "problema",
        encabezado("Problema y objetivos", "Qué buscamos explicar, qué buscamos predecir y para qué"),
        fluidRow(box(title = "Planteamiento del problema", status = "primary", solidHeader = TRUE, width = 12,
          p("Las ventas de Superstore varían según qué se vende (categoría), dónde (región), a quién (segmento) y",
            "cuándo (mes y año). Sin entender esos factores, la empresa planifica metas e inventario con base en",
            "intuición, y no puede anticipar los meses de mayor o menor demanda."),
          p(tags$b("Pregunta 1 (EDA): "), PREGUNTAS[1]), p(tags$b("Pregunta 2 (modelo): "), PREGUNTAS[2]))),
        fluidRow(box(title = "Objetivo general", status = "info", solidHeader = TRUE, width = 12, h4(OBJETIVO_GENERAL))),
        fluidRow(
          box(title = "Objetivos específicos", status = "primary", solidHeader = TRUE, width = 7,
              lista_html(OBJETIVOS_ESPECIFICOS, numerada = TRUE)),
          box(title = "Justificación", status = "warning", solidHeader = TRUE, width = 5,
              p("Las ventas tienen un patrón estacional claro (suben hacia fin de año) y una tendencia de crecimiento",
                "desde 2016, como muestra el EDA. Ese comportamiento repetible es lo que hace posible proyectarlas. Se",
                "proyecta por categoría y región porque es el nivel al que se fijan metas e inventario, y porque a",
                "nivel de producto o de día los datos son demasiado escasos y ruidosos."))
        )
      ),

      # ------------------------------------------------------------ Marco teórico
      tabItem(tabName = "marco",
        encabezado("Marco teórico", "Conceptos clave, modelos y operacionalización de variables"),
        fluidRow(
          box(title = "Análisis exploratorio (EDA)", status = "primary", solidHeader = TRUE, width = 4,
              p("Resume y visualiza los datos antes de modelar: variación de cada variable (univariado) y covariación",
                "entre variables (bivariado). Sus hallazgos orientan qué variables entran al modelo.")),
          box(title = "Regresión supervisada", status = "primary", solidHeader = TRUE, width = 4,
              p("Aprende una función que relaciona variables de entrada (X) con un valor numérico (y) a partir de",
                "ejemplos conocidos. Aquí y son las ventas de un mes y X lo que se sabe antes de que empiece:",
                "categoría, región, mes del año e historial de ventas.")),
          box(title = "Generalización y sobreajuste", status = "primary", solidHeader = TRUE, width = 4,
              p("Un modelo sirve si acierta en datos que no vio. Por eso se ajusta con validación cruzada y se evalúa",
                "una sola vez en un periodo reservado (2017). Con datos en el tiempo, el modelo nunca debe entrenar",
                "con meses posteriores a los que valida."))
        ),
        fluidRow(box(title = "Modelos de regresión utilizados", status = "info", solidHeader = TRUE, width = 12,
                     DTOutput("tabla_modelos_info"))),
        fluidRow(lapply(names(METRICAS_INFO), function(m) {
          box(title = m, status = "success", solidHeader = TRUE, width = 4, p(METRICAS_INFO[[m]]))
        })),
        fluidRow(box(title = "Operacionalización: variables del dataset", status = "primary", solidHeader = TRUE,
                     width = 12, collapsible = TRUE, DTOutput("tabla_variables_dataset"))),
        fluidRow(box(title = "Operacionalización: variables del modelo (una fila = categoría × región × mes)",
                     status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, DTOutput("tabla_variables_modelo")))
      ),

      # ------------------------------------------------------------ Metodología
      tabItem(tabName = "metodologia",
        encabezado("Metodología", "Cómo se pasa de los datos crudos a la proyección"),
        fluidRow(box(title = "Flujo del proyecto", status = "primary", solidHeader = TRUE, width = 12,
                     lista_html(sprintf("%s: %s", PASOS$Paso, PASOS$Detalle), numerada = TRUE))),
        fluidRow(
          box(title = "Datos y unidad de análisis", status = "info", solidHeader = TRUE, width = 6, lista_html(c(
            "Se agregan las 9.994 líneas de pedido en ventas mensuales por categoría × región: 12 series × 48 meses = 576 filas.",
            "2014 solo aporta historia (las variables necesitan 12 meses previos).",
            "Entrenamiento: 2015–2016 (288 filas). Prueba: 2017 (144 filas), reservado y usado una sola vez.",
            "La división es por fecha, no al azar: mezclar meses dejaría al modelo ver el futuro (data leakage)."))),
          box(title = "Variable objetivo", status = "info", solidHeader = TRUE, width = 6,
              p("El modelo predice un índice: cuántas veces el promedio de los últimos 12 meses se venderá ese mes.",
                "Luego se multiplica por ese promedio para volver a dólares."),
              p("Así una misma regla sirve para series grandes (Technology en West) y pequeñas (Furniture en South),",
                "y la proyección sigue el nivel reciente de cada serie."))
        ),
        fluidRow(
          box(title = "Preprocesamiento y validación", status = "success", solidHeader = TRUE, width = 6, lista_html(c(
            "Mismo preprocesamiento para los 7 modelos: one-hot para categoría y región y estandarización de las numéricas, ajustado solo con los datos de entrenamiento de cada pliegue (model/features.R).",
            "Validación cruzada temporal con 4 pliegues: cada uno entrena con los meses anteriores y valida con los 3 meses siguientes.",
            "Semilla fija (42) para que los resultados se puedan reproducir."))),
          box(title = "Búsqueda de hiperparámetros", status = "success", solidHeader = TRUE, width = 6, lista_html(c(
            "Grid Search: prueba todas las combinaciones de una rejilla de hiperparámetros definida para cada modelo (model/modelos.R).",
            "Criterio: menor MAE en validación cruzada. El mejor modelo se elige con ese error, no con el de 2017. El árbol de decisión individual se compara pero no es elegible: es inestable.",
            "Referencia: proyectar lo mismo que el mismo mes del año anterior. Un modelo solo aporta si la supera.")))
        )
      ),

      # ------------------------------------------------------------ ETL
      tabItem(tabName = "etl",
        encabezado("ETL", "Extracción, transformación y carga: de las líneas de pedido a la tabla del modelo"),
        fluidRow(
          valueBox(num(nrow(df)), "Líneas de pedido", icon = icon("receipt"), color = "navy", width = 3),
          valueBox(sum(is.na(df)), "Valores faltantes", icon = icon("circle-question"), color = "green", width = 3),
          valueBox(sum(duplicated(df)), "Filas duplicadas", icon = icon("clone"), color = "green", width = 3),
          valueBox(nrow(mensual), sprintf("Filas de la tabla mensual (%d meses sin ventas)", sum(mensual$Sales == 0)),
                   icon = icon("table"), color = "blue", width = 3)
        ),
        fluidRow(
          box(title = "Código del ETL (data/generate_data.R)", status = "primary", solidHeader = TRUE, width = 7,
              collapsible = TRUE, tags$pre(class = "codigo", paste(readLines(file.path(ROOT, "data", "generate_data.R"),
                                                                              encoding = "UTF-8"), collapse = "\n")),
              div(class = "texto-grafico", "Extracción con readr, transformación con lubridate (mdy) y dplyr (mutate, rename,",
                "arrange, group_by, summarise) y ordenación con tidyr (complete): si en un mes no hubo pedidos, la",
                "venta queda en 0 para que cada serie tenga sus 48 meses.")),
          box(title = "Tabla mensual (resultado del ETL)", status = "info", solidHeader = TRUE, width = 5,
              DTOutput("tabla_mensual"))
        ),
        fluidRow(caja_grafico("Ventas mensuales por categoría", "etl_fig",
          "Las tres categorías repiten el mismo patrón cada año —ventas bajas a comienzos de año y picos de septiembre a diciembre— y crecen desde 2016. Ese patrón repetible es lo que el modelo aprende para proyectar.",
          width = 12, status = "success"))
      ),

      # ------------------------------------------------------------ EDA: univariado
      tabItem(tabName = "eda_uni",
        encabezado("Análisis univariado", "Cada fila del dataset es una línea de pedido: un pedido puede incluir varios productos"),
        fluidRow(box(title = "Resumen estadístico de Sales (USD por línea de pedido)", status = "primary",
                     solidHeader = TRUE, width = 12, DTOutput("tabla_resumen"), div(class = "texto-grafico", resumen_eda$texto))),
        fluidRow(caja_grafico("Histograma de Sales", "eda_hist", eda$hist$texto),
                 caja_grafico("Histograma de densidad de log(Sales)", "eda_log", eda$log$texto)),
        fluidRow(caja_grafico("Barras: líneas de pedido por categoría", "eda_bar_cat", eda$bar_cat$texto, status = "info"),
                 caja_grafico("Barras: líneas de pedido por región", "eda_bar_reg", eda$bar_reg$texto, status = "info"))
      ),

      # ------------------------------------------------------------ EDA: bivariado
      tabItem(tabName = "eda_bi",
        encabezado("Análisis bivariado", "Cómo cambian las ventas según categoría, región y segmento"),
        fluidRow(caja_grafico("Cajas: Sales por categoría", "eda_box_cat", eda$box_cat$texto, width = 4),
                 caja_grafico("Cajas: Sales por región", "eda_box_reg", eda$box_reg$texto, width = 4),
                 caja_grafico("Cajas: Sales por segmento", "eda_box_seg", eda$box_seg$texto, width = 4))
      ),

      # ------------------------------------------------------------ EDA: en el tiempo
      tabItem(tabName = "eda_tiempo",
        encabezado("Ventas en el tiempo", "Tendencia, estacionalidad y comportamiento de cada categoría"),
        fluidRow(caja_grafico("Evolución mensual de ventas", "eda_mensual", eda$mensual$texto),
                 caja_grafico("Tendencia: promedio móvil de 12 meses", "eda_movil", eda$movil$texto)),
        fluidRow(caja_grafico("Estacionalidad: % de las ventas del año por mes", "eda_estac", eda$estac$texto, status = "info"),
                 caja_grafico("Tendencia de ventas por categoría", "eda_tend_cat", eda$tend_cat$texto, status = "info"))
      ),

      # ------------------------------------------------------------ EDA: atípicos
      tabItem(tabName = "eda_atip",
        encabezado("Atípicos y relación con el modelo", "Las compras grandes y la relación que aprovecha el modelo"),
        fluidRow(caja_grafico("Atípicos: su peso en líneas y en ventas", "eda_atip", eda$atip$texto, status = "warning"),
                 caja_grafico("Dispersión con línea de tendencia: el mes contra el mismo mes del año anterior",
                              "eda_disp", eda$disp$texto, status = "warning"))
      ),

      # ------------------------------------------------------------ EDA: geografía y otros factores
      tabItem(tabName = "eda_geo",
        encabezado("Geografía y otros factores de las ventas", "Sales por estado, cantidad, descuento y subcategoría"),
        fluidRow(caja_grafico("Mapa: ventas totales por estado", "eda_mapa", eda$mapa$texto, alto = "380px"),
                 caja_grafico("Mapa: venta mediana por línea de pedido", "eda_mapa_med", eda$mapa_med$texto, alto = "380px")),
        fluidRow(caja_grafico("Correlación de Sales con cantidad y descuento", "eda_corr", eda$corr$texto, width = 5,
                              status = "info", alto = "360px"),
                 caja_grafico("Contenedores 2D: descuento y ventas", "eda_bins", eda$bins$texto, width = 7, status = "info",
                              alto = "360px")),
        fluidRow(caja_grafico("Burbujas: volumen y valor de las ventas por subcategoría", "eda_burbuja", eda$burbuja$texto,
                              width = 12, status = "success", alto = "460px"))
      ),

      # ------------------------------------------------------------ EDA: explorador interactivo
      tabItem(tabName = "eda_explorador",
        encabezado("Explorador interactivo", "Elige una variable, el tipo de gráfico y el rango de años"),
        fluidRow(
          box(title = "Configuración de gráficos", status = "primary", solidHeader = TRUE, width = 4,
              selectInput("variable", "Selecciona una variable:", choices = VARIABLES_EXPLORADOR),
              selectInput("plot_type", "Selecciona el tipo de gráfico:",
                          choices = c("Histograma / gráfico de barras" = "barras", "Gráfico de cajas (box plot)" = "caja")),
              sliderInput("rango_anio", "Filtra por año del pedido:", min = min(df$Anio), max = max(df$Anio),
                          value = c(min(df$Anio), max(df$Anio)), step = 1, sep = ""),
              helpText("Para variables categóricas, las barras muestran las ventas totales por categoría de producto;",
                       "para numéricas, el histograma. Las cajas comparan la distribución de las ventas.")),
          box(title = "Gráfico interactivo", status = "success", solidHeader = TRUE, width = 8,
              plotlyOutput("distPlot", height = "380px"))
        ),
        fluidRow(box(title = "Tabla de datos", status = "info", solidHeader = TRUE, width = 12, DTOutput("tablaDatos")))
      ),

      # ------------------------------------------------------------ Modelos y resultados
      tabItem(tabName = "modelos",
        encabezado("Modelos y resultados", "7 modelos de regresión evaluados en 2017, un año que no vieron al entrenar"),
        if (!listo) fluidRow(aviso_sin_modelo()) else tagList(
          fluidRow(
            valueBox(resumen$mejor_modelo, "Mejor modelo (validación cruzada)", icon = icon("trophy"), color = "green", width = 3),
            valueBox(usd(resumen$prueba$MAE), sprintf("MAE en 2017 (referencia: %s)", usd(resumen$referencia$MAE)),
                     icon = icon("bullseye"), color = "blue", width = 3),
            valueBox(pct(resumen$prueba$total_MAE_pct), sprintf("Error del total mensual (referencia: %s)",
                     pct(resumen$referencia$total_MAE_pct)), icon = icon("percent"), color = "light-blue", width = 3),
            valueBox(pct(resumen$prueba$total_desvio_total_pct, signo = TRUE),
                     sprintf("Desvío del total 2017 (referencia: %s)", pct(resumen$referencia$total_desvio_total_pct, signo = TRUE)),
                     icon = icon("scale-balanced"), color = "yellow", width = 3)
          ),
          fluidRow(box(title = "Real vs. proyectado: ventas totales de la empresa por mes", status = "primary",
                       solidHeader = TRUE, width = 12,
                       fluidRow(column(4, selectInput("modelos_sel", "Selecciona un modelo:",
                                                      c(mejores_por_modelo()$modelo, REFERENCIA), resumen$mejor_modelo))),
                       plotlyOutput("modelos_ajuste", height = "360px"),
                       div(class = "texto-grafico", "Cada punto es la suma de las 12 series del mes. La zona sombreada es 2017:",
                         "el modelo no vio esos meses al entrenar. La referencia (repetir el año anterior) queda por debajo",
                         "porque no capta el crecimiento de 2017."))),
          fluidRow(box(title = "Métricas por categoría × región × mes", status = "info", solidHeader = TRUE, width = 12,
                       DTOutput("tabla_metricas"), div(class = "texto-grafico", textos_modelos()$metricas))),
          fluidRow(caja_grafico("Ranking de modelos (MAE en 2017)", "comp_ranking", texto_ranking(), width = 12,
                                alto = "340px")),
          fluidRow(box(title = "Hiperparámetros probados con Grid Search", status = "info", solidHeader = TRUE,
                       width = 12, collapsible = TRUE, DTOutput("tabla_rejillas"),
                       div(class = "texto-grafico", "Cada combinación de la rejilla se entrenó con validación cruzada temporal",
                         "(4 pliegues) y se eligió la de menor MAE del índice en validación."))),
          fluidRow(caja_grafico("¿Hay sobreajuste? Error en entrenamiento vs. prueba", "modelos_sobreajuste",
                                textos_modelos()$sobreajuste, status = "warning"),
                   caja_grafico(paste0("Importancia de variables (", resumen$mejor_modelo, ")"), "modelos_importancia",
                                textos_modelos()$importancia, status = "warning")),
          h3(class = "titulo-seccion", "¿Cuánto confiar en la proyección de un año completo?"),
          fluidRow(
            valueBox(pct(resumen$prueba_recursiva$total_desvio_total_pct, signo = TRUE), sprintf("Desvío del total anual (referencia: %s)",
                     pct(resumen$referencia$total_desvio_total_pct, signo = TRUE)), icon = icon("scale-balanced"), color = "green", width = 4),
            valueBox(pct(resumen$prueba_recursiva$total_MAE_pct), sprintf("Error del total mensual (%s por mes)",
                     usd(resumen$prueba_recursiva$total_MAE)), icon = icon("percent"), color = "blue", width = 4),
            valueBox(sprintf("%.2f", resumen$prueba_recursiva$total_R2), "R² del total mensual",
                     icon = icon("square-root-variable"), color = "light-blue", width = 4)
          ),
          fluidRow(caja_grafico("Prueba de la proyección a 12 meses: 2017 proyectado solo con datos hasta 2016",
            "comp_recursiva", paste("Es la misma forma en que se proyecta 2018: cada mes proyectado se usa como historia",
            "del siguiente, sin conocer ningún dato real del año. El modelo acierta el total del año y la forma",
            "estacional; los errores están en el detalle de cada mes. La referencia subestima todo el año porque",
            "repite 2016 sin el crecimiento."), width = 12, status = "success"))
        )
      ),

      # ------------------------------------------------------------ Entrenamiento del modelo
      tabItem(tabName = "entrenamiento",
        encabezado("Entrenamiento del modelo", "Entrena cualquiera de los 7 modelos con la misma metodología del proyecto"),
        fluidRow(
          # Caja de control
          box(title = "Control de entrenamiento", status = "primary", solidHeader = TRUE, width = 4,
              selectInput("modelo_seleccionado", "Selecciona un modelo:", choices = names(MODELOS), selected = "Random Forest"),
              actionButton("entrenarModelo", "Entrenar modelo", class = "btn-success", icon = icon("play")),
              helpText("Busca los mejores hiperparámetros de su rejilla con validación cruzada temporal (2015–2016) y",
                       "evalúa una sola vez en 2017, igual que model/train_model.R. Random Forest y XGBoost tardan",
                       "unos segundos."),
              hr(),
              actionButton("guardar_modelo", "Guardar modelo", class = "btn-default", icon = icon("floppy-disk")),
              helpText("Guarda el modelo entrenado como archivo .rds en la carpeta de la app.")),
          # Caja de resultados
          box(title = "Resultados del modelo", status = "info", solidHeader = TRUE, width = 8, collapsible = TRUE,
              uiOutput("metricas_entrenamiento"),
              h4("Real vs. predicho (total de la empresa por mes)"),
              plotlyOutput("entrenamiento_ajuste", height = "320px"),
              h4("Hiperparámetros elegidos"),
              verbatimTextOutput("entrenamiento_params"))
        )
      ),

      # ------------------------------------------------------------ Proyección 2018
      tabItem(tabName = "proyeccion",
        encabezado("Proyección y predicción", "Ventas esperadas en 2018 por mes, categoría y región, y simulador de un mes"),
        if (!listo) fluidRow(aviso_sin_modelo()) else tagList(
          fluidRow(box(title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(column(3, selectInput("proy_cat", "Categoría:", c(TODAS, names(CATEGORY_COLORS)))),
                                column(3, selectInput("proy_reg", "Región:", c(TODAS, "Central", "East", "South", "West"))),
                                column(6, helpText(sprintf("Ventas esperadas por mes con %s, reentrenado con 2015–2017.",
                                                           resumen$mejor_modelo)))))),
          fluidRow(uiOutput("proy_kpis")),
          fluidRow(box(title = "Ventas mensuales: real 2015–2017 y proyección 2018", status = "primary",
                       solidHeader = TRUE, width = 12, plotlyOutput("proy_fig", height = "380px"),
                       div(class = "texto-grafico", "La banda muestra el error típico de una proyección a 12 meses para esta",
                         "selección, medido al proyectar 2017 sin conocerlo. Para el total de la empresa el error es menor",
                         "que para una sola categoría en una región, porque al sumar series los errores se compensan."))),
          fluidRow(
            box(title = "Proyección por categoría y región (2018 frente a 2017)", status = "info", solidHeader = TRUE,
                width = 7, DTOutput("tabla_proyeccion")),
            box(title = "Lectura para el negocio", status = "warning", solidHeader = TRUE, width = 5,
                lista_html(lectura_negocio()))
          ),
          h3(class = "titulo-seccion", "Predicción interactiva de un mes (model.rds)"),
          p(class = "subtitulo-seccion", "Simulador con el modelo entrenado: elige categoría, región y mes, cambia",
            "la historia de la serie y presiona Predecir."),
          fluidRow(
            # Caja de atributos
            box(title = "Ingresa los datos de la serie:", status = "primary", solidHeader = TRUE, width = 4,
                h4("Cargar ejemplos:"),
                fluidRow(column(6, actionButton("cargar_temporada_alta", "Temporada alta", class = "btn-success", width = "100%")),
                         column(6, actionButton("cargar_temporada_baja", "Temporada baja", class = "btn-warning", width = "100%"))),
                hr(),
                selectInput("pred_cat", "Categoría:", names(CATEGORY_COLORS), "Technology"),
                selectInput("pred_reg", "Región:", c("Central", "East", "South", "West"), "West"),
                selectInput("pred_mes", "Mes a predecir:", opciones_mes, as.character(FUTURO)),
                lapply(seq_len(nrow(CAMPOS)), function(i) {
                  numericInput(CAMPOS$id[i], CAMPOS$etiqueta[i], value = round(inicial[[CAMPOS$columna[i]]], 2), min = 0, step = 100)
                }),
                helpText("Los valores se llenan con la historia real de la serie; cámbielos para simular otro escenario."),
                actionButton("predecir", "Predecir", class = "btn-primary btn-lg", icon = icon("chart-line"), width = "100%")),
            # Caja de resultados
            box(title = "Resultado de la predicción", status = "success", solidHeader = TRUE, width = 8,
                uiOutput("resultado_pred_box"),
                hr(),
                h4("Contexto de los datos ingresados"),
                plotlyOutput("contexto_plot", height = "320px"),
                uiOutput("pred_nota"))
          ),
          fluidRow(box(title = "Cómo predice el modelo", status = "info", solidHeader = TRUE, width = 12, lista_html(c(
            "El modelo no predice dólares directamente, sino un índice: cuántas veces el promedio de los últimos 12 meses venderá ese mes. Las ventas = índice × promedio de 12 meses.",
            "Las demás ventas del formulario se dividen por ese promedio, y el mes se codifica como seno y coseno para que diciembre quede junto a enero.",
            sprintf("Usa el modelo final guardado en model/artifacts/model.rds (%s) con el mismo preprocesamiento con el que se entrenó.", resumen$mejor_modelo)))))
        )
      ),

      # ------------------------------------------------------------ Conclusiones
      tabItem(tabName = "conclusiones",
        encabezado("Conclusiones", "Lo que aprendimos, qué recomendamos y qué tener en cuenta"),
        fluidRow(
          box(title = "Análisis exploratorio", status = "primary", solidHeader = TRUE, width = 6,
              lapply(conclusiones_eda(df), p)),
          box(title = "Modelo de proyección", status = "info", solidHeader = TRUE, width = 6,
              if (listo) lista_html(conclusiones_modelo()) else p("Entrene el modelo para ver sus conclusiones."))
        ),
        fluidRow(
          box(title = "Recomendaciones para el negocio", status = "success", solidHeader = TRUE, width = 6,
              lista_html(RECOMENDACIONES, numerada = TRUE)),
          box(title = "Limitaciones", status = "warning", solidHeader = TRUE, width = 6, lista_html(LIMITACIONES))
        )
      )
    ),
    div(class = "pie", "Proyecto: Alex Teran y David Estrada")
  )
)
