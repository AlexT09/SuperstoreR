# R/common.R — Utilidades compartidas por la app Shiny, el libro y los scripts del modelo:
# rutas, carga de datos y resultados, paleta de colores, estilo de las gráficas y formatos.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
})

# ---------------------------------------------------------------- rutas
# La raíz del proyecto es la carpeta que contiene Rbook.Rproj; así las rutas funcionan igual desde
# la raíz (libro y scripts) y desde app/ (Shiny cambia el directorio de trabajo a la carpeta de la app).
buscar_raiz <- function(dir = getwd()) {
  while (!file.exists(file.path(dir, "Rbook.Rproj"))) {
    padre <- dirname(dir)
    if (padre == dir) stop("No se encontró Rbook.Rproj: ejecute desde la carpeta del proyecto.")
    dir <- padre
  }
  normalizePath(dir, winslash = "/")
}
ROOT <- buscar_raiz()
RAW_PATH <- file.path(ROOT, "data", "raw", "Sample - Superstore.csv")
DATA_PATH <- file.path(ROOT, "data", "superstore_transformado.csv")
MENSUAL_PATH <- file.path(ROOT, "data", "ventas_mensuales.csv")
ART <- file.path(ROOT, "model", "artifacts") # salidas de model/train_model.R

# ---------------------------------------------------------------- colores
# Cada color tiene una función fija: azul = ventas reales, ámbar = proyección, verde = mejor
# resultado o crecimiento, coral = caídas. Las categorías tienen tonos distintos para distinguirlas.
REAL <- "#4a93e6"
PROY <- "#f0b851"
POS <- "#8fd3b0"
NEG <- "#f08a7e"
CATEGORY_COLORS <- c("Furniture" = "#2ca58d", "Office Supplies" = "#4a93e6", "Technology" = "#8e6cd8")
COLORWAY <- c("#4a93e6", "#2ca58d", "#8e6cd8", "#e0a030", "#5fa3c0", "#e66767")
GRIS <- "#7a8494" # referencia y líneas auxiliares

# ---------------------------------------------------------------- gráficas
# Como en el curso: cada gráfica se hace con ggplot2 y se vuelve interactiva con plotly::ggplotly().
theme_set(
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom",
          axis.title = element_text(color = "#4b5263"), plot.margin = ggplot2::margin(8, 12, 8, 8)) # (randomForest también tiene margin())
)

interactivo <- function(p, leyenda = TRUE) {
  # ggplot -> gráfica interactiva (sin barra de herramientas y con la leyenda abajo). Si la gráfica trae
  # un texto preparado para la ventana emergente (aes text), se muestra solo ese; si no, todos los valores.
  con_texto <- "text" %in% names(p$mapping) || any(vapply(p$layers, function(l) "text" %in% names(l$mapping), TRUE))
  ggplotly(p, tooltip = if (con_texto) "text" else "all") %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.18, title = list(text = "")), showlegend = leyenda,
           font = list(family = "Inter, Segoe UI, sans-serif", size = 12, color = "#3b4252"),
           hoverlabel = list(bgcolor = "white", bordercolor = "#d5dde6", font = list(color = "#1f2430", size = 12))) %>%
    config(displayModeBar = FALSE)
}

dolares <- scales::label_dollar(accuracy = 1)
dolares_k <- scales::label_dollar(scale = 1e-3, suffix = "k", accuracy = 1)

# ---------------------------------------------------------------- datos
.cache <- new.env()
cache <- function(clave, expr) {
  if (is.null(.cache[[clave]])) .cache[[clave]] <- force(expr)
  .cache[[clave]]
}

load_data <- function() {
  # Una fila por línea de pedido (salida de data/generate_data.R)
  cache("data", read_csv(DATA_PATH, show_col_types = FALSE, progress = FALSE))
}

load_mensual <- function() {
  # Ventas por categoría × región × mes (salida de data/generate_data.R)
  cache("mensual", read_csv(MENSUAL_PATH, show_col_types = FALSE, progress = FALSE))
}

# ---------------------------------------------------------------- resultados del modelo
# Los resultados que muestran la app y el libro vienen de model/train_model.R (model/artifacts/).
# La única excepción es la sección "Entrenamiento del modelo" de la app, que entrena en vivo.
REFERENCIA <- "Referencia (año anterior)"

modelo_listo <- function() file.exists(file.path(ART, "resumen.json"))

load_artifact <- function(nombre) {
  cache(paste0("art_", nombre), {
    ruta <- file.path(ART, nombre)
    if (grepl("\\.json$", nombre)) jsonlite::fromJSON(ruta, simplifyVector = TRUE)
    else read_csv(ruta, show_col_types = FALSE, progress = FALSE)
  })
}

mejores_por_modelo <- function() {
  # Resultados de los 7 modelos (sin la referencia), ordenados por MAE en la prueba
  load_artifact("resultados_modelos.csv") %>% filter(modelo != REFERENCIA) %>% arrange(MAE)
}

# ---------------------------------------------------------------- formatos
usd <- function(x, dec = 0) paste0("$", formatC(x, format = "f", digits = dec, big.mark = ","))
num <- function(x, dec = 0) formatC(x, format = "f", digits = dec, big.mark = ",")
pct <- function(x, dec = 1, signo = FALSE) {
  paste0(if (signo) ifelse(x >= 0, "+", "") else "", formatC(x, format = "f", digits = dec), " %")
}
enumerar <- function(x) {
  # c("a", "b", "c") -> "a, b y c"
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "y", x[length(x)])
}
MESES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
MESES_LARGOS <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
                  "septiembre", "octubre", "noviembre", "diciembre")
