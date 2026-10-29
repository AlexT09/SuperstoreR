# model/train_descuentos.R — Modelo del margen según el descuento, para simular la recomendación
# "tope de descuento" en Recomendaciones aplicadas (app y libro).
#
# Cada fila es una línea de pedido. El modelo predice el margen (Profit / Sales) a partir del descuento,
# la cantidad, la subcategoría, la región y el segmento. Con él se responde: si una línea se hubiera
# vendido con menos descuento, ¿qué margen habría tenido?
#
# Flujo (el mismo criterio que train_model.R):
#   1. Separa en el tiempo: entrenamiento 2014–2016, prueba 2017.
#   2. Ajusta los modelos del curso con Grid Search y validación cruzada temporal; elige por validación.
#   3. Evalúa en 2017 contra la referencia "margen promedio de la subcategoría con ese mismo descuento".
#   4. Reentrena el elegido con 2014–2017 y simula cada tope de descuento (los niveles que existen en
#      los datos, de 0 % a 80 %) sobre las líneas de 2017, el año más reciente.
#
# Simulación de una línea con descuento d mayor que el tope t:
#   - Precio: la venta pasa a Sales × (1 − t) / (1 − d). Las unidades no cambian: en los datos el
#     descuento no se relaciona con la cantidad por línea (correlación 0,01).
#   - Margen: el real más el cambio que predice el modelo al pasar de d a t.
#   - Pedidos perdidos: no se puede estimar con estos datos; la app lo deja como supuesto.
#
# Salidas en model/artifacts/: descuentos_resultados.csv, descuentos_escenarios.csv, descuentos_resumen.json
# Ejecutar desde la raíz del proyecto:  Rscript model/train_descuentos.R

source("R/common.R", encoding = "UTF-8")
# SEED, knn_predecir y a_json
for (f in c("model/features.R", "model/modelos.R", "model/entrenamiento.R")) source(f, encoding = "UTF-8")

ANIO_PRUEBA_D <- 2017
CAT_D <- c("Sub-Category", "Region", "Segment")
NUM_D <- c("Discount", "Quantity")
TOPES <- c(0, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8) # solo descuentos que existen en los datos
REFERENCIA_D <- "Referencia (promedio subcategoría × descuento)"

# ---------------------------------------------------------------- datos y preprocesamiento
d <- load_data() %>% arrange(Order_Date) %>% mutate(margen = Profit / Sales, anio = year(Order_Date))
NIVELES_D <- lapply(setNames(CAT_D, CAT_D), function(col) sort(unique(d[[col]])))

pre_d <- function(df) {
  # One-hot para las categóricas y estandarización para las numéricas, ajustado solo con df
  list(media = sapply(df[NUM_D], mean), desv = sapply(df[NUM_D], function(x) sqrt(mean((x - mean(x))^2))))
}
matriz_d <- function(pre, df) {
  onehot <- do.call(cbind, lapply(CAT_D, function(col) {
    m <- matrix(sapply(NIVELES_D[[col]], function(n) as.numeric(df[[col]] == n)), nrow = nrow(df))
    colnames(m) <- make.names(paste0(col, "_", NIVELES_D[[col]]))
    m
  }))
  cbind(onehot, sweep(sweep(as.matrix(df[NUM_D]), 2, pre$media), 2, pre$desv, "/"))
}

# ---------------------------------------------------------------- modelos del curso (rejillas cortas)
MODELOS_D <- list(
  "Ridge" = list(
    rejilla = expand.grid(lambda = c(1e-3, 1e-2, 1e-1, 1)),
    ajustar = function(X, y, p) glmnet(X, y, alpha = 0, lambda = p$lambda, standardize = FALSE),
    predecir = function(fit, X) as.numeric(predict(fit, newx = X))),
  "KNN" = list(
    rejilla = expand.grid(k = c(5, 15, 30), pesos = c("uniforme", "distancia"), stringsAsFactors = FALSE),
    ajustar = function(X, y, p) list(X = X, y = y, k = p$k, pesos = p$pesos),
    predecir = knn_predecir),
  "Árbol de decisión" = list(
    rejilla = expand.grid(profundidad = c(4, 6, 10), hoja = c(5, 20, 50)),
    ajustar = function(X, y, p) rpart(y ~ ., data = data.frame(X, y = y), method = "anova",
                                      control = rpart.control(maxdepth = p$profundidad, minbucket = p$hoja,
                                                              minsplit = 2 * p$hoja, cp = 0, xval = 0)),
    predecir = function(fit, X) as.numeric(predict(fit, newdata = data.frame(X)))),
  "Random Forest" = list(
    rejilla = expand.grid(hoja = c(5, 20), variables = c(1 / 3, 0.5)),
    ajustar = function(X, y, p) {
      set.seed(SEED)
      randomForest(x = X, y = y, ntree = 200, nodesize = p$hoja, mtry = max(1, floor(p$variables * ncol(X))))
    },
    predecir = function(fit, X) as.numeric(predict(fit, X))),
  "XGBoost" = list(
    rejilla = expand.grid(profundidad = c(3, 5), tasa = c(0.05, 0.1), peso_hijo = c(1, 5)),
    ajustar = function(X, y, p) {
      set.seed(SEED)
      xgb.train(params = list(objective = "reg:squarederror", max_depth = p$profundidad, eta = p$tasa,
                              min_child_weight = p$peso_hijo, nthread = 1, seed = SEED),
                data = xgb.DMatrix(X, label = y), nrounds = 300, verbose = 0)
    },
    predecir = function(fit, X) as.numeric(predict(fit, xgb.DMatrix(X))))
)

ajustar_d <- function(nombre, p, df) {
  pre <- pre_d(df)
  list(modelo = nombre, pre = pre, fit = MODELOS_D[[nombre]]$ajustar(matriz_d(pre, df), df$margen, p))
}
predecir_d <- function(m, df) MODELOS_D[[m$modelo]]$predecir(m$fit, matriz_d(m$pre, df))

pliegues_d <- function(n, k = 4) {
  # Como TimeSeriesSplit de scikit-learn: k bloques de validación consecutivos al final de los datos
  tam <- n %/% (k + 1)
  lapply(seq_len(k), function(i) {
    ini <- n - (k - i + 1) * tam
    list(train = seq_len(ini), valid = (ini + 1):(ini + tam))
  })
}

metricas_d <- function(real_m, pred_m, sales) {
  g_real <- sum(real_m * sales)
  g_pred <- sum(pred_m * sales)
  list(MAE_margen_pts = mean(abs(real_m - pred_m)) * 100, ganancia_real = g_real, ganancia_pred = g_pred,
       desvio_ganancia_pct = (g_pred / g_real - 1) * 100)
}

referencia_d <- function(train, test) {
  # Margen promedio de la subcategoría con ese descuento en entrenamiento (si no hay, el de la subcategoría)
  por_desc <- train %>% group_by(`Sub-Category`, Discount) %>% summarise(m1 = mean(margen), .groups = "drop")
  por_sub <- train %>% group_by(`Sub-Category`) %>% summarise(m2 = mean(margen), .groups = "drop")
  test %>% left_join(por_desc, by = c("Sub-Category", "Discount")) %>% left_join(por_sub, by = "Sub-Category") %>%
    mutate(m = coalesce(m1, m2)) %>% pull(m)
}

simular_topes <- function(m, base) {
  # Ventas y ganancia de cada categoría × región con cada tope de descuento
  pred_actual <- predecir_d(m, base)
  bind_rows(lapply(TOPES, function(t) {
    cambia <- base$Discount > t + 1e-9
    delta <- numeric(nrow(base))
    if (any(cambia)) delta[cambia] <- predecir_d(m, base[cambia, ] %>% mutate(Discount = t)) - pred_actual[cambia]
    base %>% mutate(cambia = cambia,
                    s_nueva = ifelse(cambia, Sales * (1 - t) / (1 - Discount), Sales),
                    p_nueva = s_nueva * (margen + delta)) %>%
      group_by(Category, Region) %>%
      summarise(tope = t, ventas_base = sum(Sales), ganancia_base = sum(Profit),
                # Líneas que el tope modifica: antes y después (sin contar pedidos perdidos)
                ventas_cambian_antes = sum(Sales[cambia]), ganancia_cambian_antes = sum(Profit[cambia]),
                ventas_cambian_despues = sum(s_nueva[cambia]), ganancia_cambian_despues = sum(p_nueva[cambia]),
                lineas_cambian = sum(cambia), .groups = "drop")
  }))
}

# ---------------------------------------------------------------- entrenamiento y evaluación
train <- d %>% filter(anio < ANIO_PRUEBA_D)
test <- d %>% filter(anio == ANIO_PRUEBA_D)
message(sprintf("Entrenamiento: %d líneas (2014–2016) · Prueba: %d líneas (2017)", nrow(train), nrow(test)))

filas <- list(c(list(modelo = REFERENCIA_D, cv_MAE_margen_pts = NA, hiperparametros = NA),
                metricas_d(test$margen, referencia_d(train, test), test$Sales)))
busquedas <- list()
for (nombre in names(MODELOS_D)) {
  t0 <- Sys.time()
  rejilla <- MODELOS_D[[nombre]]$rejilla
  cv <- sapply(seq_len(nrow(rejilla)), function(j) {
    p <- rejilla[j, , drop = FALSE]
    mean(sapply(pliegues_d(nrow(train)), function(f) {
      m <- ajustar_d(nombre, p, train[f$train, ])
      mean(abs(train$margen[f$valid] - predecir_d(m, train[f$valid, ])))
    }))
  })
  mejor <- rejilla[which.min(cv), , drop = FALSE]
  m <- ajustar_d(nombre, mejor, train)
  met <- metricas_d(test$margen, predecir_d(m, test), test$Sales)
  busquedas[[nombre]] <- list(params = mejor, cv = min(cv))
  filas[[length(filas) + 1]] <- c(list(modelo = nombre, cv_MAE_margen_pts = min(cv) * 100,
                                       hiperparametros = a_json(as.list(mejor))), met)
  message(sprintf("  %-18s CV %5.2f pts · prueba %5.2f pts · ganancia %+5.1f %% · %4.1fs", nombre, min(cv) * 100,
                  met$MAE_margen_pts, met$desvio_ganancia_pct, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
res <- bind_rows(lapply(filas, as_tibble))
write_csv(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), file.path(ART, "descuentos_resultados.csv"))

# Elegido por validación cruzada; se reentrena con toda la historia y se simula sobre 2017
ganador <- names(busquedas)[which.min(sapply(busquedas, `[[`, "cv"))]
final <- ajustar_d(ganador, busquedas[[ganador]]$params, d)
write_csv(simular_topes(final, test) %>% mutate(across(where(is.double), ~ round(.x, 2))),
          file.path(ART, "descuentos_escenarios.csv"))

f <- res %>% filter(modelo == ganador)
r <- res %>% filter(modelo == REFERENCIA_D)
jsonlite::write_json(list(
  mejor_modelo = ganador, hiperparametros = as.list(busquedas[[ganador]]$params),
  cv_MAE_margen_pts = round(f$cv_MAE_margen_pts, 3),
  prueba = list(MAE_margen_pts = round(f$MAE_margen_pts, 3), desvio_ganancia_pct = round(f$desvio_ganancia_pct, 3)),
  referencia = list(MAE_margen_pts = round(r$MAE_margen_pts, 3), desvio_ganancia_pct = round(r$desvio_ganancia_pct, 3)),
  lineas_entrenamiento = nrow(train), lineas_prueba = nrow(test), anio_base = ANIO_PRUEBA_D
), file.path(ART, "descuentos_resumen.json"), auto_unbox = TRUE, pretty = TRUE)
message("Mejor modelo: ", ganador, " · artefactos en ", ART)
