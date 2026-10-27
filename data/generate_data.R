# data/generate_data.R — ETL del proyecto.
#
# Extracción:     data/raw/Sample - Superstore.csv
# Transformación: fechas a tipo fecha, orden cronológico y tabla mensual de ventas
# Carga:          data/superstore_transformado.csv  (una fila por línea de pedido, la usa el EDA)
#                 data/ventas_mensuales.csv         (una fila por categoría × región × mes, la usa el modelo)
#
# Ejecutar desde la raíz del proyecto:  Rscript data/generate_data.R

source("R/common.R")

tabla_mensual <- function(df) {
  # Ventas totales por categoría × región × mes. Incluye todas las combinaciones
  # (3 categorías × 4 regiones × 48 meses): un mes sin pedidos queda con ventas 0 en lugar de
  # desaparecer, para que la serie no tenga huecos.
  meses <- seq(floor_date(min(df$Order_Date), "month"), floor_date(max(df$Order_Date), "month"),
               by = "month")
  df %>%
    mutate(Mes = floor_date(Order_Date, "month")) %>%
    group_by(Category, Region, Mes) %>%
    summarise(Sales = sum(Sales), .groups = "drop") %>%
    complete(Category, Region, Mes = meses, fill = list(Sales = 0)) %>%
    mutate(Sales = round(Sales, 2)) %>%
    arrange(Category, Region, Mes)
}

# --- Extracción
df <- read_csv(RAW_PATH, locale = locale(encoding = "latin1"), show_col_types = FALSE)

# --- Transformación
df <- df %>%
  mutate(`Order Date` = mdy(`Order Date`), `Ship Date` = mdy(`Ship Date`)) %>%
  rename(Order_Date = `Order Date`) %>%
  arrange(Order_Date)
mensual <- tabla_mensual(df)

# --- Carga
write_csv(df, DATA_PATH)
write_csv(mensual, MENSUAL_PATH)
message(sprintf("OK: %s filas -> superstore_transformado.csv · %s filas -> ventas_mensuales.csv",
                num(nrow(df)), num(nrow(mensual))))
