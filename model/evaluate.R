# model/evaluate.R — Métricas de evaluación (en dólares) e importancia de variables.

metricas <- function(real, pred) {
  # MAE, RMSE y R² por categoría × región × mes, más el MAE como % de la venta media
  mae <- mean(abs(real - pred))
  list(MAE = mae, RMSE = sqrt(mean((real - pred)^2)),
       R2 = 1 - sum((real - pred)^2) / sum((real - mean(real))^2), MAE_pct = mae / mean(real) * 100)
}

metricas_total <- function(filas, pred) {
  # Las mismas métricas sobre el total mensual de la empresa, más el desvío del total del periodo
  t <- filas %>% mutate(pred = pred) %>% group_by(Mes) %>%
    summarise(Sales = sum(Sales), pred = sum(pred), .groups = "drop")
  m <- metricas(t$Sales, t$pred)
  m$desvio_total_pct <- (sum(t$pred) / sum(t$Sales) - 1) * 100
  setNames(m, paste0("total_", names(m)))
}

importancia <- function(final, filas, repeticiones = 30) {
  # Importancia por permutación: cuánto empeora el MAE (del índice) al desordenar cada variable.
  # Funciona igual para los 7 modelos (no depende de coeficientes ni de árboles).
  set.seed(SEED)
  base <- mean(abs(filas[[TARGET]] - predecir_indice(final, filas)))
  bind_rows(lapply(FEATURES, function(v) {
    aumentos <- replicate(repeticiones, {
      f <- filas
      f[[v]] <- sample(f[[v]])
      mean(abs(f[[TARGET]] - predecir_indice(final, f))) - base
    })
    tibble(variable = v, importancia = mean(aumentos), std = sd(aumentos) * sqrt((repeticiones - 1) / repeticiones),
           nombre = NOMBRES[[v]])
  })) %>% arrange(desc(importancia))
}
