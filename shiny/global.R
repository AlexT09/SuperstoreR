# --- Archivo: global.R ---
# Se ejecuta primero: carga de librerías, carga de datos y preprocesamiento.
# Todo lo que se define aquí está disponible en ui.R y server.R.

# Cargar las librerías necesarias
library(shiny)
library(tidyverse)
library(DT) # Para tablas interactivas
library(caret) # Para el entrenamiento del modelo
library(plotly) # Para gráficos interactivos
library(randomForest) # Requerido por caret para el método 'rf'

# Cargar y preparar los datos (salida del ETL del libro)
df <- read.csv("superstore_transformado.csv", check.names = FALSE, encoding = "UTF-8")

# Renombrar columnas con espacios o guiones para usarlas más fácil
df <- df %>%
  rename(Subcategoria = `Sub-Category`, Envio = `Ship Mode`)

# Convertir variables a tipo 'factor' y extraer el año del pedido
df <- df %>%
  mutate(
    Order_Date = as.Date(Order_Date),
    Anio = as.integer(format(Order_Date, "%Y")),
    Category = as.factor(Category),
    Region = as.factor(Region),
    Segment = as.factor(Segment),
    Subcategoria = as.factor(Subcategoria),
    Envio = as.factor(Envio)
  )

# ---------------------------------------------------------------- tabla mensual para el modelo
# Cada fila es una categoría en una región durante un mes (3 × 4 series × 48 meses)
meses <- seq(as.Date("2014-01-01"), as.Date("2017-12-01"), by = "month")

mensual <- df %>%
  mutate(Mes = as.Date(format(Order_Date, "%Y-%m-01"))) %>%
  group_by(Category, Region, Mes) %>%
  summarise(Sales = sum(Sales), .groups = "drop") %>%
  complete(Category, Region, Mes = meses, fill = list(Sales = 0))

# Variables del modelo: solo usan meses anteriores, así se conocen antes de empezar el mes.
# El modelo predice un índice (ventas del mes / promedio de los 12 meses anteriores) y luego
# se convierte a dólares, así la misma regla sirve para series grandes y pequeñas.
modelo_datos <- mensual %>%
  group_by(Category, Region) %>%
  arrange(Mes, .by_group = TRUE) %>%
  mutate(
    lag_12 = lag(Sales, 12),
    lag_1 = lag(Sales, 1),
    prom_3m = (lag(Sales, 1) + lag(Sales, 2) + lag(Sales, 3)) / 3,
    prom_12m = as.numeric(stats::filter(lag(Sales, 1), rep(1 / 12, 12), sides = 1))
  ) %>%
  ungroup() %>%
  mutate(
    r_lag_12 = lag_12 / prom_12m,
    r_lag_1 = lag_1 / prom_12m,
    r_prom_3m = prom_3m / prom_12m,
    mes_sin = sin(2 * pi * as.integer(format(Mes, "%m")) / 12),
    mes_cos = cos(2 * pi * as.integer(format(Mes, "%m")) / 12),
    indice = Sales / prom_12m
  ) %>%
  filter(!is.na(prom_12m), prom_12m > 0, !is.na(lag_12)) %>%
  arrange(Mes, Category, Region)

# Fórmula del modelo
formula_modelo <- indice ~ Category + Region + r_lag_12 + r_lag_1 + r_prom_3m + mes_sin + mes_cos

# Máximo de los sliders de la pestaña de predicción
max_ventas <- ceiling(max(mensual$Sales) / 1000) * 1000

# Formato de dinero
usd <- function(x) paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))

# Mensaje de confirmación en la consola
cat("Datos cargados y preprocesados en global.R\n")
