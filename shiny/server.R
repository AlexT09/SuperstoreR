# --- Archivo: server.R ---

# Define el servidor (lógica del back-end)
server <- function(input, output, session) {

  # --- Pestaña 1: Lógica del Análisis Exploratorio ---

  # 1. Creamos un set de datos reactivo que se filtra por el rango de años
  datos_filtrados <- reactive({
    df %>%
      filter(Anio >= input$rango_anio[1] & Anio <= input$rango_anio[2])
  })

  # 2. Creamos el gráfico de distribución
  output$distPlot <- renderPlotly({
    variable_seleccionada <- input$variable

    # Lógica para seleccionar el tipo de gráfico
    if (input$plot_type == "Histograma / Gráfico de Barras") {

      # Si la variable es categórica (factor), crea un gráfico de barras con las ventas totales
      if (is.factor(datos_filtrados()[[variable_seleccionada]])) {
        p <- datos_filtrados() %>%
          group_by(across(all_of(unique(c(variable_seleccionada, "Category"))))) %>%
          summarise(Ventas = sum(Sales), .groups = "drop") %>%
          ggplot(aes(x = reorder(.data[[variable_seleccionada]], -Ventas, sum), y = Ventas, fill = Category)) +
          geom_col() +
          scale_y_continuous(labels = scales::dollar) +
          labs(x = variable_seleccionada, y = "Ventas (USD)",
               title = paste("Ventas totales por", variable_seleccionada)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if (variable_seleccionada == "Sales") {
        # Las ventas son muy asimétricas: el histograma se ve mejor en escala logarítmica
        p <- datos_filtrados() %>%
          ggplot(aes(x = Sales, fill = Category)) +
          geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
          scale_x_log10(labels = scales::dollar) +
          labs(x = "Ventas por línea de pedido (escala log)", y = "Frecuencia",
               title = "Distribución de las ventas") +
          theme_minimal()
      } else {
        # Si es numérica, crea un histograma
        p <- datos_filtrados() %>%
          ggplot(aes(x = .data[[variable_seleccionada]], fill = Category)) +
          geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
          labs(y = "Frecuencia", title = paste("Distribución de", variable_seleccionada)) +
          theme_minimal()
      }

    } else { # Lógica para el "Gráfico de Cajas (Box Plot)"

      # Si la variable es categórica, muestra la distribución de las ventas para cada categoría
      if (is.factor(datos_filtrados()[[variable_seleccionada]])) {
        p <- datos_filtrados() %>%
          ggplot(aes(x = .data[[variable_seleccionada]], y = Sales, fill = .data[[variable_seleccionada]])) +
          geom_boxplot(show.legend = FALSE) +
          scale_y_log10(labels = scales::dollar) +
          labs(y = "Ventas (escala log)", title = paste("Distribución de las ventas por", variable_seleccionada)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

      } else {
        # Si la variable es numérica, muestra su distribución por categoría de producto
        p <- datos_filtrados() %>%
          ggplot(aes(x = Category, y = .data[[variable_seleccionada]], fill = Category)) +
          geom_boxplot(show.legend = FALSE) +
          labs(title = paste("Distribución de", variable_seleccionada, "por Categoría")) +
          theme_minimal()
        if (variable_seleccionada == "Sales") p <- p + scale_y_log10(labels = scales::dollar)
      }
    }

    # Convierte el gráfico ggplot a plotly para hacerlo interactivo
    ggplotly(p)
  })

  # 3. Creamos la tabla de datos
  output$tablaDatos <- DT::renderDataTable({
    datos_filtrados() %>%
      select(Order_Date, Category, Subcategoria, Region, Segment, Sales, Profit)
  }, options = list(pageLength = 10))


  # --- Pestaña 2: Lógica de Entrenamiento del Modelo ---

  # Usamos reactiveValues para almacenar el modelo entrenado y usarlo en la otra pestaña
  valores_reactivos <- reactiveValues(modelo = NULL)

  # Almacena el modelo entrenado en valores_reactivos al presionar el botón
  observeEvent(input$entrenarModelo, {
    withProgress(message = "Entrenando modelo...", detail = "Esto puede tomar un momento...", {

      # 1. Preparación de datos: división en el tiempo (primeros meses = entrenamiento)
      set.seed(42)
      meses_modelo <- sort(unique(modelo_datos$Mes))
      corte <- meses_modelo[round(length(meses_modelo) * input$train_split_percentage / 100)]
      datos_entrenamiento <- modelo_datos %>% filter(Mes <= corte)
      datos_prueba <- modelo_datos %>% filter(Mes > corte)

      # 2. Entrenamiento del modelo seleccionado por el usuario
      modelo <- train(formula_modelo,
                      data = datos_entrenamiento,
                      method = input$modelo_seleccionado, # Usa el input del usuario
                      trControl = trainControl(method = "cv", number = 5),
                      metric = "MAE")

      # Almacena el modelo para la pestaña de predicciones
      valores_reactivos$modelo <- modelo

      # 3. Predicciones en el periodo de prueba (el índice se convierte a dólares)
      datos_prueba <- datos_prueba %>%
        mutate(Predicho = pmax(predict(modelo, newdata = datos_prueba) * prom_12m, 0))
      metricas <- postResample(pred = datos_prueba$Predicho, obs = datos_prueba$Sales)
      mae_referencia <- mean(abs(datos_prueba$Sales - datos_prueba$lag_12))

      # --- Creación de las salidas ---

      # Salida 1: Métricas principales en texto
      output$metricas_modelo <- renderPrint({
        cat("Entrenamiento:", format(min(datos_entrenamiento$Mes), "%Y-%m"), "a", format(corte, "%Y-%m"),
            "·", nrow(datos_entrenamiento), "filas\n")
        cat("Prueba:       ", format(min(datos_prueba$Mes), "%Y-%m"), "a", format(max(datos_prueba$Mes), "%Y-%m"),
            "·", nrow(datos_prueba), "filas\n\n")
        cat("Métricas en el periodo de prueba (ventas del mes por categoría y región, USD):\n")
        print(round(metricas, 3))
        cat("\nReferencia (repetir el mismo mes del año anterior): MAE =", round(mae_referencia, 1), "\n")
        cat(ifelse(metricas["MAE"] < mae_referencia,
                   "El modelo se equivoca menos que la referencia.\n",
                   "El modelo no mejora a la referencia con esta división.\n"))
      })

      # Salida 2: Real vs. predicho, total de la empresa por mes
      output$real_vs_predicho <- renderPlotly({
        totales <- datos_prueba %>%
          group_by(Mes) %>%
          summarise(Real = sum(Sales), Predicho = sum(Predicho)) %>%
          pivot_longer(-Mes, names_to = "Serie", values_to = "Ventas")

        p <- ggplot(totales, aes(x = Mes, y = Ventas, color = Serie)) +
          geom_line(linewidth = 1) +
          geom_point() +
          scale_color_manual(values = c("Real" = "#1B4F72", "Predicho" = "#E67E22")) +
          scale_y_continuous(labels = scales::dollar) +
          labs(x = "", y = "Ventas del mes (USD)", color = "") +
          theme_minimal()

        ggplotly(p)
      })

      # Salida 3: Importancia de variables
      output$importancia <- renderPlotly({
        imp <- varImp(modelo)$importance %>%
          rownames_to_column("Variable") %>%
          rename(Importancia = 2)

        p <- ggplot(imp, aes(x = reorder(Variable, Importancia), y = Importancia)) +
          geom_col(fill = "#5DADE2") +
          coord_flip() +
          labs(x = "", y = "Importancia relativa (0–100)") +
          theme_minimal()

        ggplotly(p)
      })

    }) # Fin de withProgress
  }) # Fin de observeEvent

  # Guarda el modelo entrenado como archivo .rds
  observeEvent(input$guardar_modelo, {
    if (is.null(valores_reactivos$modelo)) {
      showNotification("Primero entrena un modelo.", type = "warning")
    } else {
      archivo <- paste0(input$modelo_seleccionado, "_", Sys.Date(), ".rds")
      saveRDS(valores_reactivos$modelo, archivo)
      showNotification(paste("Modelo guardado en", archivo), type = "message")
    }
  })


  # --- Pestaña 3: Predicciones ---

  # Llena el formulario con una fila real del dataset
  cargar_ejemplo <- function(fila) {
    updateSelectInput(session, "pred_Category", selected = as.character(fila$Category))
    updateSelectInput(session, "pred_Region", selected = as.character(fila$Region))
    updateSliderInput(session, "pred_mes", value = as.integer(format(fila$Mes, "%m")))
    updateSliderInput(session, "pred_prom_12m", value = round(fila$prom_12m, -2))
    updateSliderInput(session, "pred_lag_12", value = round(fila$lag_12, -2))
    updateSliderInput(session, "pred_lag_1", value = round(fila$lag_1, -2))
    updateSliderInput(session, "pred_prom_3m", value = round(fila$prom_3m, -2))
  }

  # Ejemplo de temporada alta: Technology en East, noviembre de 2017
  observeEvent(input$cargar_temporada_alta, {
    cargar_ejemplo(modelo_datos %>% filter(Category == "Technology", Region == "East", Mes == as.Date("2017-11-01")))
  })

  # Ejemplo de temporada baja: Furniture en Central, febrero de 2017
  observeEvent(input$cargar_temporada_baja, {
    cargar_ejemplo(modelo_datos %>% filter(Category == "Furniture", Region == "Central", Mes == as.Date("2017-02-01")))
  })

  # Reactivo que se activa al presionar el botón "Predecir"
  prediccion_hecha <- eventReactive(input$predecir, {
    req(valores_reactivos$modelo)

    prom_12m <- as.numeric(input$pred_prom_12m)
    datos_nuevos <- data.frame(
      Category = factor(input$pred_Category, levels = levels(df$Category)),
      Region = factor(input$pred_Region, levels = levels(df$Region)),
      r_lag_12 = as.numeric(input$pred_lag_12) / prom_12m,
      r_lag_1 = as.numeric(input$pred_lag_1) / prom_12m,
      r_prom_3m = as.numeric(input$pred_prom_3m) / prom_12m,
      mes_sin = sin(2 * pi * input$pred_mes / 12),
      mes_cos = cos(2 * pi * input$pred_mes / 12)
    )

    indice <- predict(valores_reactivos$modelo, newdata = datos_nuevos)
    ventas <- max(indice * prom_12m, 0)

    # Devuelve una lista con la predicción, el índice y los datos ingresados
    list(ventas = ventas, indice = indice, prom_12m = prom_12m,
         datos = data.frame(
           Variable = c("Promedio 12 meses", "Mismo mes año anterior", "Mes anterior", "Promedio 3 meses"),
           Valor = c(prom_12m, input$pred_lag_12, input$pred_lag_1, input$pred_prom_3m)
         ))
  })

  # Salida para el infoBox dinámico
  output$resultado_pred_box <- renderUI({
    validate(need(!is.null(valores_reactivos$modelo),
                  "Primero entrena un modelo en la pestaña 'Entrenamiento del Modelo'."))
    # Requiere que se haya hecho una predicción
    req(prediccion_hecha())
    res <- prediccion_hecha()

    texto <- paste0(usd(res$ventas), " en ventas (", round(res$indice, 2), "× el promedio de 12 meses)")

    if (res$ventas >= res$prom_12m) {
      infoBox(
        title = "Resultado: Mes por encima del promedio",
        value = texto,
        icon = icon("arrow-trend-up"),
        color = "green",
        width = 12,
        fill = TRUE
      )
    } else {
      infoBox(
        title = "Resultado: Mes por debajo del promedio",
        value = texto,
        icon = icon("arrow-trend-down"),
        color = "orange",
        width = 12,
        fill = TRUE
      )
    }
  })

  # Salida para el gráfico de contexto
  output$contexto_plot <- renderPlotly({
    req(prediccion_hecha())
    res <- prediccion_hecha()

    # Datos ingresados + la predicción
    datos_plot <- bind_rows(
      res$datos %>% mutate(Grupo = "Historia ingresada"),
      data.frame(Variable = "Predicción", Valor = res$ventas, Grupo = "Predicción")
    ) %>%
      mutate(Variable = factor(Variable, levels = rev(Variable)))

    # Crear gráfico
    p <- ggplot(datos_plot, aes(x = Variable, y = Valor, fill = Grupo)) +
      geom_col() +
      scale_fill_manual(values = c("Historia ingresada" = "#5DADE2", "Predicción" = "#E67E22")) +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = "Predicción frente a la historia de la serie", y = "USD", x = "", fill = "") +
      theme_minimal() +
      coord_flip() # Girar coordenadas para mejor lectura

    ggplotly(p)
  })
}
