# model/entrenamiento.R — Funciones del entrenamiento: validación cruzada temporal, búsqueda de
# hiperparámetros (Grid Search) y proyección recursiva. Las usan model/train_model.R y la pestaña
# "Entrenamiento del modelo" de la app Shiny.

ANIO_PRUEBA <- 2017
N_SERIES <- 12 # 3 categorías × 4 regiones: filas por mes
N_PLIEGUES <- 4
# Un árbol de decisión individual es inestable: cambios mínimos en los datos (de la sexta decimal)
# cambian sus cortes y su error de validación. Se entrena y se compara como los demás, pero no puede
# ser el modelo final; para eso están sus versiones estables (Random Forest y XGBoost).
NO_ELEGIBLES <- c("Árbol de decisión")

pliegues <- function(n, n_pliegues = N_PLIEGUES, tam = 3 * N_SERIES) {
  # Validación cruzada temporal (como TimeSeriesSplit): cada pliegue entrena con todos los meses
  # anteriores y valida con los 3 meses siguientes. Las filas ya vienen en orden cronológico.
  lapply(seq_len(n_pliegues), function(i) {
    fin <- n - (n_pliegues - i + 1) * tam
    list(train = seq_len(fin), valid = (fin + 1):(fin + tam))
  })
}

ajustar_modelo <- function(nombre, p, df) {
  # Preprocesamiento + modelo, ajustados solo con df (como un Pipeline)
  pre <- ajustar_pre(df)
  list(modelo = nombre, params = p, pre = pre,
       fit = MODELOS[[nombre]]$ajustar(aplicar_pre(pre, df), df[[TARGET]], p))
}

buscar <- function(nombre, train) {
  # Grid Search: prueba cada combinación de la rejilla y se queda con la de menor MAE en validación
  rejilla <- MODELOS[[nombre]]$rejilla
  t0 <- Sys.time()
  cv <- sapply(seq_len(nrow(rejilla)), function(j) {
    p <- rejilla[j, , drop = FALSE]
    mean(sapply(pliegues(nrow(train)), function(f) {
      m <- ajustar_modelo(nombre, p, train[f$train, ])
      mean(abs(train[[TARGET]][f$valid] - predecir_indice(m, train[f$valid, ])))
    }))
  })
  mejor <- which.min(cv)
  list(final = ajustar_modelo(nombre, rejilla[mejor, , drop = FALSE], train), cv = cv[mejor],
       evaluaciones = nrow(rejilla), tiempo = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       params = rejilla[mejor, , drop = FALSE], rejilla = rejilla)
}

proyectar <- function(final, panel, meses) {
  # Proyección recursiva: cada mes proyectado pasa a ser historia del siguiente
  hist <- panel %>% select(Category, Region, Mes, Sales)
  series <- hist %>% distinct(Category, Region)
  for (i in seq_along(meses)) {
    mes <- meses[i]
    hist <- bind_rows(hist, series %>% mutate(Mes = mes, Sales = NA_real_))
    filas <- agregar_variables(hist) %>% filter(Mes == mes)
    pred <- a_ventas(predecir_indice(final, filas), filas)
    hist <- hist %>% filter(Mes != mes) %>%
      bind_rows(filas %>% select(Category, Region, Mes) %>% mutate(Sales = pred))
  }
  hist %>% filter(Mes %in% meses) %>% rename(Proyeccion = Sales) %>% arrange(Mes, Category, Region)
}

a_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", digits = NA))

datos_modelo <- function(panel = load_mensual()) {
  # Tabla del modelo sin las filas que no tienen 12 meses de historia, dividida en el tiempo
  datos <- agregar_variables(panel) %>% filter(if_all(all_of(c(FEATURES, TARGET)), ~ !is.na(.x)))
  list(datos = datos, train = datos %>% filter(year(Mes) < ANIO_PRUEBA), test = datos %>% filter(year(Mes) == ANIO_PRUEBA))
}

periodo <- function(d) paste(format(min(d$Mes), "%Y-%m"), "a", format(max(d$Mes), "%Y-%m"))
