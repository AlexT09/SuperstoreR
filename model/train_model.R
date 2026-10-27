# model/train_model.R — Entrena, compara y guarda el modelo de proyección de ventas mensuales.
#
# Flujo:
#   1. Lee data/ventas_mensuales.csv (categoría × región × mes) y arma las variables (features.R).
#   2. Separa en el tiempo: entrenamiento 2015–2016, prueba 2017 (2014 solo aporta historia).
#   3. Para cada uno de los 7 modelos del curso busca los mejores hiperparámetros (Grid Search) con
#      validación cruzada temporal: 4 pliegues, cada uno valida con los 3 meses siguientes.
#   4. Evalúa en 2017 contra la referencia "igual que el mismo mes del año anterior".
#   5. Elige el mejor modelo por su error de validación cruzada (no por el de prueba, que se usa
#      una sola vez para reportar), lo reentrena con 2015–2017 y proyecta 2018 mes a mes.
#
# Salidas en model/artifacts/:
#   model.rds                 modelo final (preprocesamiento + modelo)
#   resultados_modelos.csv    una fila por modelo, más la referencia
#   predicciones.csv          real vs. proyectado (entrenamiento y prueba) por modelo
#   importancia.csv           importancia de variables del mejor modelo
#   prueba_recursiva_2017.csv 2017 proyectado a 12 meses (como 2018) vs. real
#   proyeccion_2018.csv       proyección de 2018 por categoría × región × mes
#   resumen.json              mejor modelo, hiperparámetros y métricas principales
#
# Ejecutar desde la raíz del proyecto (después de data/generate_data.R):  Rscript model/train_model.R

source("R/common.R")
source("model/features.R")
source("model/modelos.R")
source("model/evaluate.R")
source("model/entrenamiento.R")

# ---------------------------------------------------------------- entrenamiento
dir.create(ART, showWarnings = FALSE, recursive = TRUE)
panel <- load_mensual()
d <- datos_modelo(panel)
datos <- d$datos
train <- d$train
test <- d$test
message(sprintf("Entrenamiento: %d filas (%s) · Prueba: %d filas (%s)", nrow(train), periodo(train),
                nrow(test), periodo(test)))

# Referencia: proyectar lo mismo que el mismo mes del año anterior
filas <- list(c(list(modelo = REFERENCIA), metricas(test$Sales, test$lag_12), metricas_total(test, test$lag_12)))
preds <- list()
preds[[REFERENCIA]] <- list(train = train$lag_12, test = test$lag_12)

busquedas <- list()
for (nombre in names(MODELOS)) {
  s <- buscar(nombre, train)
  pred <- a_ventas(predecir_indice(s$final, test), test)
  filas[[length(filas) + 1]] <- c(
    list(modelo = nombre), metricas(test$Sales, pred), metricas_total(test, pred),
    list(evaluaciones = s$evaluaciones, tiempo_s = s$tiempo, cv_MAE_indice = s$cv,
         hiperparametros = a_json(as.list(s$params)), rejilla = a_json(lapply(s$rejilla, unique)))
  )
  message(sprintf("  %-18s CV %.4f · MAE prueba %7.0f · %5.1fs", nombre, s$cv, filas[[length(filas)]]$MAE, s$tiempo))
  busquedas[[nombre]] <- s
  preds[[nombre]] <- list(train = a_ventas(predecir_indice(s$final, train), train), test = pred)
}
res <- bind_rows(filas) %>% mutate(elegible = modelo != REFERENCIA & !modelo %in% NO_ELEGIBLES)
write_csv(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), file.path(ART, "resultados_modelos.csv"), na = "")

# Real vs. proyectado de cada modelo, en entrenamiento y prueba
out <- bind_rows(train %>% mutate(conjunto = "Entrenamiento"), test %>% mutate(conjunto = "Prueba")) %>%
  select(Category, Region, Mes, conjunto, Sales)
for (nombre in names(preds)) out[[nombre]] <- c(preds[[nombre]]$train, preds[[nombre]]$test)
write_csv(out %>% mutate(across(where(is.numeric), ~ round(.x, 2))), file.path(ART, "predicciones.csv"))

# Mejor modelo: menor error de validación cruzada entre los elegibles (la prueba no interviene)
elegibles <- setdiff(names(busquedas), NO_ELEGIBLES)
ganador <- elegibles[which.min(sapply(busquedas[elegibles], `[[`, "cv"))]
s <- busquedas[[ganador]]
write_csv(importancia(s$final, test) %>% mutate(across(where(is.numeric), ~ round(.x, 5))),
          file.path(ART, "importancia.csv"))

# Prueba de la proyección a 12 meses: proyectar todo 2017 de forma recursiva, solo con historia
# hasta 2016 (igual que se hará con 2018). Es la medida honesta del error de la proyección anual.
meses_prueba <- seq(as.Date(sprintf("%d-01-01", ANIO_PRUEBA)), by = "month", length.out = 12)
rec <- proyectar(s$final, panel %>% filter(year(Mes) < ANIO_PRUEBA), meses_prueba) %>%
  inner_join(panel, by = c("Category", "Region", "Mes"))
write_csv(rec %>% mutate(across(where(is.numeric), ~ round(.x, 2))), file.path(ART, "prueba_recursiva_2017.csv"))
rec_m <- c(metricas(rec$Sales, rec$Proyeccion), metricas_total(rec, rec$Proyeccion))

# Modelo final: mismos hiperparámetros, reentrenado con toda la historia (2015–2017)
final <- ajustar_modelo(ganador, s$params, datos)
saveRDS(final, file.path(ART, "model.rds"))
meses <- seq(as.Date(sprintf("%d-01-01", ANIO_PRUEBA + 1)), by = "month", length.out = 12)
proy <- proyectar(final, panel, meses)
write_csv(proy %>% mutate(Proyeccion = round(Proyeccion, 2)), file.path(ART, "proyeccion_2018.csv"))

fila <- res %>% filter(modelo == ganador)
ref <- res %>% filter(modelo == REFERENCIA)
claves <- c("MAE", "RMSE", "R2", "MAE_pct", "total_MAE", "total_MAE_pct", "total_R2", "total_desvio_total_pct")
redondear <- function(x) lapply(x, function(v) round(as.numeric(v), 4))
resumen <- list(
  mejor_modelo = ganador,
  hiperparametros = as.list(s$params),
  prueba = redondear(as.list(fila[claves])),
  referencia = redondear(as.list(ref[claves])),
  prueba_recursiva = redondear(rec_m),
  entrenamiento = periodo(train),
  prueba_periodo = periodo(test),
  filas = list(entrenamiento = nrow(train), prueba = nrow(test)),
  validacion = "Validación cruzada temporal (4 pliegues, cada uno valida con los 3 meses siguientes)",
  ventas_2017 = round(sum(panel$Sales[year(panel$Mes) == 2017]), 2),
  proyeccion_2018 = round(sum(proy$Proyeccion), 2),
  semilla = SEED
)
write(jsonlite::toJSON(resumen, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA), file.path(ART, "resumen.json"))
message(sprintf("\nMejor modelo: %s · MAE prueba %s vs referencia %s", ganador, usd(fila$MAE), usd(ref$MAE)))
message(sprintf("Proyección 2018: %s (2017 real: %s)", usd(resumen$proyeccion_2018), usd(resumen$ventas_2017)))
