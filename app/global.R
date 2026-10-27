# --- Archivo: global.R ---
# Se ejecuta primero: carga de librerías, código compartido, datos y resultados del modelo.
# Todo lo que se define aquí está disponible en ui.R y server.R.

# Cargar las librerías necesarias
library(shiny)
library(shinydashboard)
library(DT) # Para tablas interactivas

# Código compartido con el libro: datos, gráficas (ggplot2 + plotly) y modelo
source("../R/common.R", encoding = "UTF-8")
for (f in c("model/features.R", "model/modelos.R", "model/evaluate.R", "model/entrenamiento.R",
            "R/eda.R", "R/resultados.R", "R/contenido.R")) {
  source(file.path(ROOT, f), encoding = "UTF-8")
}

# ---------------------------------------------------------------- datos
df <- load_data() %>% mutate(Anio = year(Order_Date))
mensual <- load_mensual()
dm <- datos_modelo(mensual) # tabla del modelo: entrenamiento 2015–2016 y prueba 2017

# Resultados del modelo (los genera model/train_model.R)
listo <- modelo_listo()
if (listo) {
  resumen <- load_artifact("resumen.json")
  final <- readRDS(file.path(ART, "model.rds"))
}

# ---------------------------------------------------------------- EDA (se calcula una sola vez)
resumen_eda <- eda_resumen(df)
eda <- suppressWarnings(list(
  hist = eda_histograma(df), log = eda_log(df),
  bar_cat = eda_barras(df, "Category", "Categoría", REAL), bar_reg = eda_barras(df, "Region", "Región", "#2ca58d"),
  mapa = eda_mapa(df),
  box_cat = eda_box(df, "Category", "Categoría"), box_reg = eda_box(df, "Region", "Región"),
  box_seg = eda_box(df, "Segment", "Segmento"),
  burbuja = eda_burbuja(df), bins = eda_bins2d(df), corr = eda_correlacion(df),
  mensual = eda_mensual(df), movil = eda_movil(df), estac = eda_estacionalidad(df),
  tend_cat = eda_tendencia_categoria(df), atip = eda_atipicos(df), disp = eda_dispersion_rezago(mensual)
))

# ---------------------------------------------------------------- predicción
FUTURO <- as.Date("2018-01-01") # primer mes sin ventas reales
# Variables del modelo para cada serie y mes, incluido enero 2018 (sin ventas reales)
historia <- agregar_variables(bind_rows(
  mensual %>% select(Category, Region, Mes, Sales),
  mensual %>% distinct(Category, Region) %>% mutate(Mes = FUTURO, Sales = NA_real_)
)) %>% filter(!is.na(prom_12m), !is.na(lag_12), !is.na(lag_1), !is.na(prom_3m))
meses_pred <- sort(unique(historia$Mes), decreasing = TRUE)
etiqueta_mes <- function(mes) paste(tools::toTitleCase(MESES_LARGOS[month(mes)]), year(mes))
opciones_mes <- setNames(as.character(meses_pred),
                         paste0(etiqueta_mes(meses_pred), ifelse(meses_pred == FUTURO, " (futuro)", "")))
# Campos del formulario: id del input, columna de la historia y etiqueta
CAMPOS <- tibble(
  id = c("pred_prom12", "pred_lag12", "pred_lag1", "pred_prom3"),
  columna = c("prom_12m", "lag_12", "lag_1", "prom_3m"),
  etiqueta = c("Promedio mensual de los últimos 12 meses (USD):", "Ventas del mismo mes del año anterior (USD):",
               "Ventas del mes anterior (USD):", "Promedio de los últimos 3 meses (USD):")
)
fila_historia <- function(cat, reg, mes) historia %>% filter(Category == cat, Region == reg, Mes == as.Date(mes))
inicial <- fila_historia("Technology", "West", FUTURO)
# Botones de ejemplo: un mes de temporada alta y uno de temporada baja
EJEMPLOS <- list(alta = list(cat = "Technology", reg = "East", mes = "2017-11-01"),
                 baja = list(cat = "Furniture", reg = "South", mes = "2017-02-01"))

# ---------------------------------------------------------------- componentes de la interfaz
caja_grafico <- function(titulo, id, texto, width = 6, status = "primary", alto = "340px") {
  # Caja con una gráfica interactiva y su interpretación
  box(title = titulo, status = status, solidHeader = TRUE, width = width,
      plotlyOutput(id, height = alto), div(class = "texto-grafico", texto))
}

encabezado <- function(titulo, subtitulo) {
  # Título de la página con una línea que explica qué se ve en ella
  div(class = "encabezado", h2(titulo), p(subtitulo))
}

lista_html <- function(x, numerada = FALSE) (if (numerada) tags$ol else tags$ul)(lapply(x, tags$li))

tabla_dt <- function(datos, paginas = FALSE, filtro = FALSE, descargar = FALSE, archivo = "datos") {
  # Tabla interactiva (DT) con textos en español y, si se pide, botón para descargarla en CSV
  datatable(datos, rownames = FALSE, filter = if (filtro) "top" else "none",
            extensions = if (descargar) "Buttons" else list(),
            options = list(dom = paste0(if (descargar) "B" else "", if (paginas) "tip" else "t"),
                           paging = paginas, pageLength = 12, scrollX = TRUE,
                           buttons = list(list(extend = "csv", text = "Descargar CSV", filename = archivo)),
                           language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_ filas",
                                           infoEmpty = "Sin filas", zeroRecords = "Sin resultados",
                                           paginate = list(previous = "Anterior", `next` = "Siguiente"))))
}

# ---------------------------------------------------------------- resumen ejecutivo (portada)
if (listo) {
  lectura <- lectura_negocio()
  hallazgos <- c(lectura[2:4], conclusiones_modelo()[1])
}

aviso_sin_modelo <- function() {
  box(title = "Modelo no entrenado", status = "warning", solidHeader = TRUE, width = 12,
      p("Ejecute ", code("Rscript model/train_model.R"), " desde la raíz del proyecto y vuelva a abrir la app."))
}

fig_etl <- function() {
  t <- mensual %>% group_by(Mes, Category) %>% summarise(Sales = sum(Sales), .groups = "drop")
  ggplot(t, aes(Mes, Sales, color = Category, group = Category,
                text = sprintf("%s · %s: %s", Category, format(Mes, "%b %Y"), usd(Sales)))) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = CATEGORY_COLORS, name = NULL) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
}
