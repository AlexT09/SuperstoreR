# --- Archivo: server.R ---
# Define el servidor (lógica del back-end)
server <- function(input, output, session) {

  # --- Tablas fijas (Marco teórico, ETL y EDA) ---
  output$tabla_modelos_info <- renderDT(tabla_dt(MODELOS_INFO))
  output$tabla_variables_dataset <- renderDT(tabla_dt(VARIABLES_DATASET))
  output$tabla_variables_modelo <- renderDT(tabla_dt(VARIABLES_MODELO))
  output$tabla_resumen <- renderDT(tabla_dt(resumen_eda$tabla))
  output$tabla_mensual <- renderDT(tabla_dt(
    mensual %>% transmute(Categoría = Category, Región = Region, Mes = format(Mes, "%Y-%m"), Ventas = round(Sales, 2)),
    paginas = TRUE, filtro = TRUE, descargar = TRUE, archivo = "ventas_mensuales") %>% formatCurrency("Ventas"))

  # --- Gráficas fijas: cada ggplot se vuelve interactivo con ggplotly ---
  output$etl_fig <- renderPlotly(interactivo(fig_etl()))
  graficas_eda <- c(eda_hist = "hist", eda_log = "log", eda_bar_cat = "bar_cat", eda_bar_reg = "bar_reg",
                    eda_mapa = "mapa", eda_mapa_med = "mapa_med", eda_box_cat = "box_cat", eda_box_reg = "box_reg", eda_box_seg = "box_seg",
                    eda_burbuja = "burbuja", eda_bins = "bins", eda_corr = "corr", eda_mensual = "mensual",
                    eda_movil = "movil", eda_estac = "estac", eda_tend_cat = "tend_cat", eda_atip = "atip",
                    eda_disp = "disp")
  for (id in names(graficas_eda)) {
    local({
      clave <- graficas_eda[[id]]
      output[[id]] <- renderPlotly(suppressWarnings(interactivo(eda[[clave]]$fig)))
    })
  }

  # --- EDA: explorador interactivo ---
  # 1. Datos reactivos filtrados por el rango de años
  datos_filtrados <- reactive({
    df %>% filter(Anio >= input$rango_anio[1] & Anio <= input$rango_anio[2])
  })
  # 2. Gráfico según la variable y el tipo elegidos
  output$distPlot <- renderPlotly({
    suppressWarnings(interactivo(grafico_explorador(datos_filtrados(), input$variable, input$plot_type)))
  })
  # 3. Tabla de datos filtrada
  output$tablaDatos <- renderDT({
    tabla_dt(datos_filtrados() %>% select(`Order ID`, Order_Date, Category, `Sub-Category`, Region, State, Segment,
                                          `Ship Mode`, Sales, Quantity, Discount, Profit),
             paginas = TRUE, filtro = TRUE, descargar = TRUE, archivo = "superstore_filtrado")
  })

  if (!listo) return(invisible())

  # --- Resumen ejecutivo: todo reacciona a los filtros de categoría y región ---
  # Cifras de la selección (reactive), de las que salen las cuatro valueBox
  kpis_sel <- reactive(kpis_proyeccion(input$res_cat, input$res_reg))
  caja_kpi <- function(i, icono, color) {
    renderValueBox({
      k <- kpis_sel()[[i]]
      valueBox(k[[2]], paste0(k[[1]], " · ", k[[3]]), icon = icon(icono), color = color)
    })
  }
  output$res_kpi_proy <- caja_kpi(1, "chart-line", "yellow")
  output$res_kpi_var <- caja_kpi(2, "arrow-trend-up", "green")
  output$res_kpi_pico <- caja_kpi(3, "calendar", "blue")
  output$res_kpi_error <- caja_kpi(4, "bullseye", "light-blue")
  output$resumen_fig <- renderPlotly(suppressWarnings(interactivo(fig_proyeccion(input$res_cat, input$res_reg))))
  output$res_lectura <- renderText(texto_resumen(input$res_cat, input$res_reg))
  output$res_barras <- renderPlotly(suppressWarnings(interactivo(fig_barras_2017_2018(input$res_cat, input$res_reg))))
  output$res_calor <- renderPlotly(suppressWarnings(interactivo(fig_calor_proyeccion(input$res_cat, input$res_reg), leyenda = FALSE)))

  # --- Modelos y resultados ---
  output$modelos_ajuste <- renderPlotly({
    req(input$modelos_sel)
    suppressWarnings(interactivo(fig_ajuste(input$modelos_sel)))
  })
  output$tabla_metricas <- renderDT({
    res <- bind_rows(mejores_por_modelo(), load_artifact("resultados_modelos.csv") %>% filter(modelo == REFERENCIA))
    tabla_dt(res %>% transmute(Modelo = modelo, MAE = round(MAE), RMSE = round(RMSE), `R²` = round(R2, 2),
                               `Error total mensual (%)` = round(total_MAE_pct, 1)), descargar = TRUE, archivo = "metricas_modelos") %>%
      formatCurrency(c("MAE", "RMSE"), digits = 0)
  })
  output$tabla_rejillas <- renderDT(tabla_dt(mejores_por_modelo() %>% transmute(
    Modelo = ifelse(elegible, modelo, paste(modelo, "(no elegible)")), Combinaciones = evaluaciones,
    `Rejilla probada` = sapply(rejilla, fmt_rejilla), `Elegido por validación cruzada` = sapply(hiperparametros, fmt_params),
    `Error de validación` = round(cv_MAE_indice, 4))))
  output$modelos_sobreajuste <- renderPlotly(suppressWarnings(interactivo(fig_sobreajuste(errores_por_conjunto()))))
  output$modelos_importancia <- renderPlotly(suppressWarnings(interactivo(fig_importancia(), leyenda = FALSE)))

  # --- Entrenamiento del modelo ---
  modelo_entrenado <- reactiveVal(NULL)

  # Entrenar el modelo al presionar el botón
  observeEvent(input$entrenarModelo, {
    nombre <- input$modelo_seleccionado
    withProgress(message = paste("Entrenando", nombre), detail = "Grid Search con validación cruzada temporal…", value = .3, {
      s <- buscar(nombre, dm$train)
      incProgress(.6)
      pred_test <- a_ventas(predecir_indice(s$final, dm$test), dm$test)
      pred_train <- a_ventas(predecir_indice(s$final, dm$train), dm$train)
    })
    modelo_entrenado(list(nombre = nombre, busqueda = s,
                          metricas = metricas(dm$test$Sales, pred_test),
                          referencia = metricas(dm$test$Sales, dm$test$lag_12),
                          pred = bind_rows(dm$train %>% mutate(conjunto = "Entrenamiento", pred = pred_train),
                                           dm$test %>% mutate(conjunto = "Prueba", pred = pred_test))))
    showNotification(paste(nombre, "entrenado."), type = "message")
  })

  output$metricas_entrenamiento <- renderUI({
    m <- modelo_entrenado()
    if (is.null(m)) return(helpText("Elige un modelo y presiona «Entrenar modelo»."))
    mejora <- (1 - m$metricas$MAE / m$referencia$MAE) * 100
    fluidRow(
      valueBox(usd(m$metricas$MAE), "MAE en 2017", icon = icon("bullseye"), color = "blue", width = 3),
      valueBox(usd(m$metricas$RMSE), "RMSE en 2017", icon = icon("square-root-variable"), color = "light-blue", width = 3),
      valueBox(sprintf("%.2f", m$metricas$R2), "R² en 2017", icon = icon("chart-simple"), color = "navy", width = 3),
      valueBox(pct(mejora, 0, signo = TRUE), sprintf("menos error que la referencia (%s)", usd(m$referencia$MAE)),
               icon = icon("scale-balanced"), color = if (mejora > 0) "green" else "red", width = 3)
    )
  })

  output$entrenamiento_ajuste <- renderPlotly({
    m <- modelo_entrenado()
    req(m)
    t <- m$pred %>% group_by(Mes, conjunto) %>% summarise(Real = sum(Sales), Predicho = sum(pred), .groups = "drop") %>%
      pivot_longer(c(Real, Predicho), names_to = "serie", values_to = "ventas")
    p <- ggplot(t, aes(Mes, ventas, color = serie, group = serie,
                       text = sprintf("%s · %s (%s): %s", format(Mes, "%b %Y"), serie, tolower(conjunto), usd(ventas)))) +
      annotate("rect", xmin = as.Date("2017-01-01"), xmax = as.Date("2017-12-01"), ymin = -Inf, ymax = Inf, fill = REAL, alpha = .07) +
      geom_line(linewidth = 1) + geom_point(size = 1.3) +
      scale_color_manual(values = c(Real = REAL, Predicho = PROY), name = NULL) +
      scale_y_continuous(labels = dolares_k) + labs(x = NULL, y = "Ventas del mes (USD)")
    suppressWarnings(interactivo(p))
  })

  output$entrenamiento_params <- renderPrint({
    m <- modelo_entrenado()
    req(m)
    cat("Modelo:", m$nombre, "\n")
    cat("Combinaciones probadas:", m$busqueda$evaluaciones, "en", round(m$busqueda$tiempo, 1), "segundos\n")
    cat("Mejores hiperparámetros:", fmt_params(jsonlite::toJSON(as.list(m$busqueda$params), auto_unbox = TRUE, na = "null")), "\n")
    cat("Error de validación cruzada (MAE del índice):", round(m$busqueda$cv, 4), "\n")
  })

  # Guardar el modelo entrenado como .rds
  observeEvent(input$guardar_modelo, {
    m <- modelo_entrenado()
    if (is.null(m)) {
      showNotification("Primero entrena un modelo.", type = "warning")
      return()
    }
    archivo <- paste0("modelo_", gsub("[^a-z]+", "_", tolower(iconv(m$nombre, to = "ASCII//TRANSLIT"))), ".rds")
    saveRDS(m$busqueda$final, archivo)
    showNotification(paste("Modelo guardado en app/", archivo, sep = ""), type = "message")
  })

  # --- Comparación (dentro de Modelos y resultados) ---
  output$comp_ranking <- renderPlotly(suppressWarnings(interactivo(fig_ranking())))
  output$comp_recursiva <- renderPlotly(suppressWarnings(interactivo(fig_recursiva())))

  # --- Proyección 2018 ---
  output$proy_kpis <- renderUI({
    k <- kpis_proyeccion(input$proy_cat, input$proy_reg)
    iconos <- c("chart-line", "arrow-trend-up", "calendar", "percent")
    colores <- c("yellow", "green", "blue", "light-blue")
    tagList(lapply(seq_along(k), function(i) {
      valueBox(k[[i]][[2]], paste0(k[[i]][[1]], " · ", k[[i]][[3]]), icon = icon(iconos[i]), color = colores[i], width = 3)
    }))
  })
  output$proy_fig <- renderPlotly(suppressWarnings(interactivo(fig_proyeccion(input$proy_cat, input$proy_reg))))
  output$tabla_proyeccion <- renderDT({
    tabla_dt(tabla_series() %>% transmute(Categoría = Category, Región = Region, `Ventas 2017` = round(v2017),
                                          `Proyección 2018` = round(v2018), `Variación (%)` = round(crec, 1)),
             descargar = TRUE, archivo = "proyeccion_2018") %>%
      formatCurrency(c("Ventas 2017", "Proyección 2018"), digits = 0) %>%
      formatStyle("Variación (%)", color = styleInterval(c(0, 15), c("#d64545", "#3b4252", "#1e8e5a")), fontWeight = "bold")
  })

  # --- Predicción (dentro de Proyección y predicción) ---
  # Llena el formulario con la historia real de la serie elegida
  observeEvent(list(input$pred_cat, input$pred_reg, input$pred_mes), {
    f <- fila_historia(input$pred_cat, input$pred_reg, input$pred_mes)
    req(nrow(f) == 1)
    for (i in seq_len(nrow(CAMPOS))) updateNumericInput(session, CAMPOS$id[i], value = round(f[[CAMPOS$columna[i]]], 2))
  }, ignoreInit = TRUE)

  # Botones de ejemplo: cargan un mes de temporada alta o baja
  cargar_ejemplo <- function(e) {
    updateSelectInput(session, "pred_cat", selected = e$cat)
    updateSelectInput(session, "pred_reg", selected = e$reg)
    updateSelectInput(session, "pred_mes", selected = e$mes)
  }
  observeEvent(input$cargar_temporada_alta, cargar_ejemplo(EJEMPLOS$alta))
  observeEvent(input$cargar_temporada_baja, cargar_ejemplo(EJEMPLOS$baja))

  # La predicción se calcula al presionar «Predecir» (y una vez al abrir la app)
  prediccion <- eventReactive(input$predecir, {
    # Un campo vacío llega como NULL: se convierte en NA para que lo detecte la validación
    v <- vapply(CAMPOS$id, function(id) { x <- input[[id]]; if (is.null(x)) NA_real_ else as.numeric(x) }, numeric(1))
    shiny::validate(shiny::need(all(!is.na(v)) && all(v >= 0) && v[1] > 0,
                                "Completa los cuatro valores (el promedio de 12 meses debe ser mayor que cero)."))
    mes <- as.Date(input$pred_mes)
    X <- tibble(Category = input$pred_cat, Region = input$pred_reg, r_lag_12 = v[[2]] / v[[1]], r_lag_1 = v[[3]] / v[[1]],
                r_prom_3m = v[[4]] / v[[1]], mes_sin = sin(2 * pi * month(mes) / 12), mes_cos = cos(2 * pi * month(mes) / 12))
    indice <- predecir_indice(final, X)
    real <- fila_historia(input$pred_cat, input$pred_reg, mes)$Sales
    list(v = unname(v), indice = indice, pred = max(indice * v[[1]], 0), real = if (length(real)) real else NA, mes = mes,
         serie = paste(input$pred_cat, "en", input$pred_reg))
  }, ignoreNULL = FALSE)

  output$resultado_pred_box <- renderUI({
    p <- prediccion()
    tiene_real <- !is.na(p$real) && p$real > 0
    fluidRow(
      infoBox("Ventas predichas", usd(p$pred), subtitle = paste(p$serie, "·", etiqueta_mes(p$mes)),
              icon = icon("chart-line"), color = "green", fill = FALSE, width = 4),
      infoBox("Índice", sprintf("%.2f×", p$indice), subtitle = "del promedio de 12 meses",
              icon = icon("percent"), color = "blue", fill = FALSE, width = 4),
      if (tiene_real) infoBox("Real", usd(p$real), subtitle = paste("error:", pct((p$pred / p$real - 1) * 100, 0, signo = TRUE)),
                              icon = icon("check"), color = "navy", fill = FALSE, width = 4)
      else infoBox("Real", "—", subtitle = "mes futuro, aún sin ventas", icon = icon("clock"), color = "navy", fill = FALSE, width = 4)
    )
  })

  output$contexto_plot <- renderPlotly({
    p <- prediccion()
    t <- tibble(concepto = factor(c("Promedio 12 meses", "Mismo mes año ant.", "Mes anterior", "Promedio 3 meses", "Predicción"),
                                  levels = c("Promedio 12 meses", "Mismo mes año ant.", "Mes anterior", "Promedio 3 meses", "Predicción")),
                valor = c(p$v, p$pred), tipo = c(rep("Historia", 4), "Predicción"))
    g <- ggplot(t, aes(concepto, valor, fill = tipo, text = sprintf("%s: %s", concepto, usd(valor)))) +
      geom_col(width = .65) +
      geom_text(aes(y = valor + max(valor) * .06, label = usd(valor)), size = 3.5) + # (ggplotly ignora vjust)
      scale_fill_manual(values = c(Historia = "#7fa7c9", Predicción = PROY), guide = "none") +
      scale_y_continuous(labels = dolares_k, expand = expansion(mult = c(0, .12))) +
      labs(x = NULL, y = "USD")
    if (!is.na(p$real)) g <- g + geom_hline(yintercept = p$real, linetype = "dotted", color = "#1f2430") +
      annotate("text", x = 0.6, y = p$real, label = paste("Real:", usd(p$real)), vjust = -0.5, hjust = 0, size = 3.5)
    suppressWarnings(interactivo(g, leyenda = FALSE))
  })

  output$pred_nota <- renderUI({
    if (prediccion()$mes >= FUTURO) helpText("Enero 2018 es un mes futuro: el modelo nunca vio sus ventas, es una predicción real.")
    else helpText("El modelo final se entrenó con 2015–2017, así que ya conoce este mes. Para medir su error sin ventaja,",
                  "vea la prueba de 2017 en la sección Modelos y resultados.")
  })
}
