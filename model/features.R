# model/features.R — Variables del modelo y preprocesamiento, compartidos por el entrenamiento
# (train_model.R), la proyección y la pestaña Predicción de la app.
#
# Cada fila es una categoría × región × mes. El modelo no predice las ventas en dólares sino un
# índice: cuántas veces el promedio de los últimos 12 meses venderá ese mes (1.3 = 30 % más que el
# promedio). Las ventas se recuperan multiplicando el índice por ese promedio. Así una misma regla
# sirve para series grandes y pequeñas, y la proyección sigue el nivel reciente de cada serie.
#
# Todas las variables usan solo meses anteriores (lag), por lo que se conocen antes de empezar el
# mes que se proyecta: no hay fuga de información del futuro.

SEED <- 42
CATEGORICAS <- c("Category", "Region")
NUMERICAS <- c("r_lag_12", "r_lag_1", "r_prom_3m", "mes_sin", "mes_cos")
FEATURES <- c(CATEGORICAS, NUMERICAS)
TARGET <- "indice" # Sales / prom_12m
NIVELES <- list(Category = c("Furniture", "Office Supplies", "Technology"),
                Region = c("Central", "East", "South", "West"))

# Nombre legible de cada variable (tablas e importancia de variables)
NOMBRES <- c(
  Category = "Categoría",
  Region = "Región",
  r_lag_12 = "Ventas del mismo mes del año anterior (relativas)",
  r_lag_1 = "Ventas del mes anterior (relativas)",
  r_prom_3m = "Promedio de los últimos 3 meses (relativo)",
  mes_sin = "Mes del año (seno)",
  mes_cos = "Mes del año (coseno)"
)

agregar_variables <- function(panel) {
  # Agrega las variables del modelo a la tabla mensual (Category, Region, Mes, Sales).
  # Las filas sin 12 meses de historia (el primer año) quedan con NA y se descartan al entrenar.
  # Los meses futuros pueden venir con Sales vacío: sus variables solo dependen de meses previos.
  panel %>%
    arrange(Category, Region, Mes) %>%
    group_by(Category, Region) %>%
    mutate(
      lag_12 = lag(Sales, 12),
      lag_1 = lag(Sales, 1),
      prom_3m = (lag(Sales, 1) + lag(Sales, 2) + lag(Sales, 3)) / 3,
      prom_12m = as.numeric(stats::filter(lag(Sales), rep(1 / 12, 12), sides = 1))
    ) %>%
    ungroup() %>%
    mutate(
      base = ifelse(prom_12m > 0, prom_12m, NA),
      r_lag_12 = lag_12 / base,
      r_lag_1 = lag_1 / base,
      r_prom_3m = prom_3m / base,
      # Mes como punto en un círculo: diciembre (12) queda junto a enero (1)
      mes_sin = sin(2 * pi * month(Mes) / 12),
      mes_cos = cos(2 * pi * month(Mes) / 12),
      indice = Sales / base
    ) %>%
    select(-base) %>%
    # Orden cronológico: la validación temporal asume que las filas siguientes son posteriores
    arrange(Mes, Category, Region)
}

# ---------------------------------------------------------------- preprocesamiento
# Equivalente al ColumnTransformer de la app Python: one-hot para categoría y región y
# estandarización para las numéricas. Se ajusta solo con los datos de entrenamiento.
ajustar_pre <- function(df) {
  sd_pob <- function(x) sqrt(mean((x - mean(x))^2)) # igual que StandardScaler
  list(media = sapply(df[NUMERICAS], mean), desv = sapply(df[NUMERICAS], sd_pob))
}

aplicar_pre <- function(pre, df) {
  onehot <- do.call(cbind, lapply(CATEGORICAS, function(col) {
    m <- sapply(NIVELES[[col]], function(nivel) as.numeric(df[[col]] == nivel))
    m <- matrix(m, nrow = nrow(df))
    colnames(m) <- make.names(paste0(col, "_", NIVELES[[col]]))
    m
  }))
  num <- sweep(sweep(as.matrix(df[NUMERICAS]), 2, pre$media), 2, pre$desv, "/")
  cbind(onehot, num)
}

a_ventas <- function(indice, filas) {
  # Convierte el índice predicho en ventas (USD); nunca negativas.
  pmax(indice * filas$prom_12m, 0)
}
