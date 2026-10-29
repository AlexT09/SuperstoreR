# R/eda.R — Gráficas e interpretaciones del análisis exploratorio (las usan la app y el libro).
#
# Cada gráfica sigue la técnica del capítulo de visualización del curso (ggplot2 + ggplotly):
# histograma, gráfico de barras, mapa (sf + geom_sf), gráfico de caja con resumen (stat_summary), burbujas (geom_count),
# contenedores 2D (geom_bin2d), correlación (geom_tile), dispersión con línea de tendencia (geom_smooth).
#
# Cada función devuelve list(fig = <ggplot>, texto = "<interpretación>"); las cifras de los textos se
# calculan con los datos. Unidad: cada fila del dataset es una LÍNEA de pedido (un pedido puede tener
# varios productos).

# ---------------------------------------------------------------- univariado
eda_resumen <- function(df) {
  s <- df$Sales
  q1 <- unname(quantile(s, .25))
  q3 <- unname(quantile(s, .75))
  # (tibble evalúa columna por columna: el RIC se calcula antes de crear las columnas q1 y q3)
  ric <- q3 - q1
  tabla <- tibble(n = num(length(s)), media = num(mean(s)), ds = num(sd(s)), mediana = num(median(s), 1),
                  q1 = num(q1, 1), q3 = num(q3), RIC = num(ric), minimo = num(min(s), 1), maximo = num(max(s)))
  texto <- sprintf(paste(
    "La media (%s) es %s veces la mediana (%s) y la desviación estándar (%s) supera a la media: una fuerte",
    "asimetría positiva. La mayoría de las líneas de pedido son de bajo valor y unas pocas muy grandes (hasta %s)",
    "jalan el promedio hacia arriba. El 50 %% central se mueve entre %s y %s (RIC de %s)."),
    usd(mean(s)), num(mean(s) / median(s), 1), usd(median(s), 1), usd(sd(s)), usd(max(s)), usd(q1, 1), usd(q3),
    usd(ric))
  list(tabla = tabla, texto = texto)
}

eda_histograma <- function(df, corte = 1000) {
  fuera <- df$Sales > corte
  fig <- ggplot(df %>% filter(!fuera), aes(x = Sales)) +
    geom_histogram(bins = 40, fill = "#5fa3c0", color = "white") +
    scale_x_continuous(labels = dolares) +
    labs(x = "Ventas por línea de pedido (USD)", y = "Líneas de pedido")
  texto <- sprintf(paste(
    "Confirma el sesgo a la derecha: la mayoría de las líneas son de bajo valor y la cola se alarga hacia la",
    "derecha. Para que se vea la forma, el eje se corta en %s: quedan fuera el %s de las líneas, que suman el %s",
    "de las ventas (ver Atípicos)."),
    usd(corte), pct(mean(fuera) * 100), pct(sum(df$Sales[fuera]) / sum(df$Sales) * 100, 0))
  list(fig = fig, texto = texto)
}

eda_log <- function(df) {
  d <- df %>% mutate(log_sales = log(Sales))
  q <- unname(quantile(d$log_sales, c(.25, .5, .75)))
  fig <- ggplot(d, aes(x = log_sales)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = REAL, color = "white", alpha = .85) +
    geom_density(color = PROY, linewidth = 1) +
    labs(x = "Log(Ventas)", y = "Densidad")
  texto <- sprintf(paste(
    "Con el logaritmo la distribución se vuelve casi simétrica (la curva ámbar es la densidad): la mitad",
    "central de las líneas queda entre log(Sales) = %.1f y %.1f (de %s a %s) y la mediana en %.1f (%s). Por eso",
    "las comparaciones entre grupos se hacen en escala logarítmica."),
    q[1], q[3], usd(exp(q[1])), usd(exp(q[3])), q[2], usd(exp(q[2])))
  list(fig = fig, texto = texto)
}

eda_barras <- function(df, col, xlabel, color = REAL) {
  t <- df %>% count(g = .data[[col]]) %>% mutate(pct = n / sum(n) * 100, g = reorder(g, n),
                                                  etiqueta = sprintf("%s (%.1f%%)", num(n), pct))
  pedidos <- df %>% group_by(g = .data[[col]]) %>% summarise(p = n_distinct(`Order ID`), .groups = "drop") %>%
    arrange(desc(p))
  fig <- ggplot(t, aes(x = g, y = n, text = paste0(g, ": ", etiqueta))) +
    geom_col(fill = color, width = .7) +
    geom_text(aes(y = n + max(n) * .05, label = etiqueta), size = 3.5) + # (ggplotly ignora vjust)
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, .12))) +
    labs(x = xlabel, y = "Líneas de pedido")
  d <- t %>% arrange(desc(n))
  mismo <- identical(as.character(d$g), pedidos$g)
  texto <- sprintf(paste(
    "%s concentra el %s de las líneas de pedido (%s), seguida de %s. Contando pedidos únicos (un pedido puede",
    "tener varias líneas) el orden %s: %s."),
    d$g[1], pct(d$pct[1]), num(d$n[1]), enumerar(sprintf("%s (%s, %s)", d$g[-1], pct(d$pct[-1]), num(d$n[-1]))),
    if (mismo) "es el mismo" else "cambia", enumerar(sprintf("%s %s", pedidos$g, num(pedidos$p))))
  list(fig = fig, texto = texto)
}

# ---------------------------------------------------------------- bivariado
eda_box <- function(df, col, xlabel) {
  niveles <- sort(unique(df[[col]]))
  colores <- if (col == "Category") CATEGORY_COLORS else setNames(COLORWAY[seq_along(niveles)], niveles)
  d <- df %>% mutate(g = factor(.data[[col]], levels = niveles))
  est <- d %>% group_by(g) %>% summarise(n = n(), media = mean(Sales), mediana = median(Sales), .groups = "drop")
  # Caja + punto en la media (visualización de estadísticas de resumen con stat_summary)
  fig <- ggplot(d, aes(x = g, y = Sales, fill = g)) +
    geom_boxplot(alpha = .55, outlier.size = .7, outlier.alpha = .35) +
    stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = PROY, color = "#5c4300") +
    scale_y_log10(labels = dolares) +
    scale_fill_manual(values = colores, guide = "none") +
    labs(x = xlabel, y = "Ventas por línea (USD, escala log)")
  e <- est %>% arrange(desc(mediana))
  medianas <- enumerar(sprintf("%s %s", e$g, usd(e$mediana, 1)))
  texto <- switch(col,
    Category = sprintf(paste(
      "La categoría es el factor que más diferencia el valor de cada línea: medianas de %s. %s y %s venden",
      "líneas de mayor valor; %s, unas %s veces menos, pero con mucho más volumen. El punto ámbar es la media,",
      "siempre por encima de la mediana por el sesgo."),
      medianas, e$g[1], e$g[2], e$g[3], num(e$mediana[2] / e$mediana[3], 1)),
    Region = sprintf(paste(
      "Las cajas de las cuatro regiones casi se superponen (medianas: %s). La región explica poco del valor de",
      "cada línea; lo que cambia entre regiones es el volumen (ver barras y mapa)."), medianas),
    Segment = {
      v <- est %>% arrange(desc(n))
      sprintf(paste(
        "Medianas: %s; ningún segmento sobresale. %s tiene %s líneas, %s veces las de %s (%s): genera más",
        "ventas totales por volumen, no por valor de cada línea."),
        medianas, v$g[1], num(v$n[1]), num(v$n[1] / v$n[3], 1), v$g[3], num(v$n[3]))
    })
  list(fig = fig, texto = texto)
}

eda_burbuja <- function(df) {
  # Gráfico de burbuja: volumen (líneas) contra valor (venta mediana) por subcategoría; tamaño = ventas totales
  t <- df %>% group_by(Category, sub = `Sub-Category`) %>%
    summarise(ventas = sum(Sales), lineas = n(), mediana = median(Sales), .groups = "drop") %>%
    mutate(share = ventas / sum(ventas) * 100)
  fig <- ggplot(t, aes(lineas, mediana, size = ventas, color = Category,
                       text = sprintf("%s<br>Líneas: %s<br>Venta mediana: %s<br>Ventas totales: %s (%s)",
                                      sub, num(lineas), usd(mediana), usd(ventas), pct(share)))) +
    geom_point(alpha = .8) +
    geom_text(aes(label = sub), size = 3, color = "#3b4252", position = position_nudge(y = .14), show.legend = FALSE) +
    scale_size_continuous(range = c(3, 16), guide = "none") +
    scale_y_log10(labels = dolares) +
    scale_color_manual(values = CATEGORY_COLORS, name = NULL) +
    labs(x = "Líneas de pedido (volumen)", y = "Venta mediana por línea (USD, escala log)")
  top <- t %>% arrange(desc(ventas))
  b <- function(s) t[t$sub == s, ]
  texto <- sprintf(paste(
    "Gráfico de burbujas: cada una es una subcategoría y su tamaño son sus ventas totales. %s y %s lideran (cada",
    "una con el %s del total) combinando volumen y valor. Binders tiene el mayor volumen (%s líneas) pero una",
    "mediana de solo %s, y Copiers lo contrario: %s líneas con una mediana de %s. Qué producto se vende es el",
    "factor que más diferencia el valor de cada línea."),
    top$sub[1], top$sub[2], pct(top$share[1], 0), num(b("Binders")$lineas), usd(b("Binders")$mediana),
    num(b("Copiers")$lineas), usd(b("Copiers")$mediana))
  list(fig = fig, texto = texto)
}

eda_bins2d <- function(df) {
  # Contenedores 2D: relación entre descuento y ventas sin que los puntos se tapen
  bajo <- mean(df$Discount <= 0.2) * 100
  r <- cor(df$Discount, df$Sales)
  med <- df %>% mutate(banda = ifelse(Discount <= 0.2, "baja", "alta")) %>% group_by(banda) %>%
    summarise(m = median(Sales), .groups = "drop")
  # Conteo por celdas de 5 puntos de descuento × $50 de venta (lo mismo que geom_bin2d, dibujado con
  # geom_tile porque ggplotly no convierte geom_bin2d)
  celdas <- df %>% filter(Sales <= 1500) %>%
    mutate(x = floor(Discount / .05) * .05 + .025, y = floor(Sales / 50) * 50 + 25) %>% count(x, y)
  fig <- ggplot(celdas, aes(x, y, fill = n, text = sprintf("Descuento %.0f–%.0f %% · ventas %s–%s: %s líneas",
                                                            (x - .025) * 100, (x + .025) * 100, usd(y - 25), usd(y + 25), num(n)))) +
    geom_tile(width = .05, height = 50) +
    scale_fill_gradient(low = "#dcebf5", high = "#1f5570", name = "Líneas", trans = "log10") +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_continuous(labels = dolares) +
    labs(x = "Descuento", y = "Ventas por línea (USD, hasta $1,500)")
  texto <- sprintf(paste(
    "El %s de las líneas tiene descuentos de 0 a 20 %%, donde se concentran las celdas más oscuras. Las ventas",
    "altas aparecen en casi todos los niveles de descuento: la mediana de venta es %s con descuento bajo y %s con",
    "descuento alto, y la correlación lineal es casi nula (%.2f). El descuento no se asocia con el valor de la línea."),
    pct(bajo, 0), usd(med$m[med$banda == "baja"], 1), usd(med$m[med$banda == "alta"], 1), r)
  list(fig = fig, texto = texto)
}

eda_correlacion <- function(df) {
  # Correlación de Sales con las otras variables numéricas. Profit no entra: es consecuencia de la venta.
  # Escala divergente: rojo negativo, blanco 0, azul positivo.
  vars <- c(Sales = "Ventas", Quantity = "Cantidad", Discount = "Descuento")
  m <- cor(df[names(vars)])
  dimnames(m) <- list(unname(vars), unname(vars))
  t <- as.data.frame(as.table(m)) %>% setNames(c("x", "y", "r")) %>%
    mutate(x = factor(x, levels = vars), y = factor(y, levels = rev(vars)))
  fig <- ggplot(t, aes(x, y, fill = r, text = sprintf("%s y %s: %.2f", x, y, r))) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", r)), size = 4) +
    scale_fill_gradient2(low = "#d64545", mid = "white", high = "#2f6fa8", midpoint = 0, limits = c(-1, 1), name = "r") +
    coord_fixed() + labs(x = NULL, y = NULL)
  med <- df %>% group_by(Quantity) %>% summarise(m = median(Sales), .groups = "drop")
  texto <- sprintf(paste(
    "Correlación de Pearson de Sales con las otras variables numéricas de cada línea de pedido. Con la cantidad es",
    "positiva pero débil (%.2f): la venta mediana sube de %s con 1 unidad a %s con 5, pero el precio del producto",
    "pesa más que las unidades. Con el descuento es prácticamente nula (%.2f). Profit no se incluye porque es una",
    "consecuencia de la venta, no un factor que la explique."),
    m["Ventas", "Cantidad"], usd(med$m[med$Quantity == 1]), usd(med$m[med$Quantity == 5]), m["Ventas", "Descuento"])
  list(fig = fig, texto = texto)
}

# ---------------------------------------------------------------- en el tiempo
ventas_mes <- function(df) {
  df %>% mutate(mes = floor_date(Order_Date, "month")) %>% group_by(mes) %>%
    summarise(Sales = sum(Sales), .groups = "drop")
}

movil12 <- function(x) as.numeric(stats::filter(x, rep(1 / 12, 12), sides = 1))

eda_mensual <- function(df) {
  t <- ventas_mes(df) %>% mutate(texto = sprintf("%s: %s", format(mes, "%b %Y"), usd(Sales)))
  fig <- ggplot(t, aes(mes, Sales)) +
    geom_line(color = REAL) +
    geom_point(aes(text = texto), color = REAL, size = 2) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
  list(fig = fig, texto = paste(
    "Se repite un patrón cada año: las ventas son bajas a comienzos de año y suben hacia el final, con los",
    "picos entre septiembre y diciembre. Es un comportamiento estacional."))
}

eda_movil <- function(df) {
  t <- ventas_mes(df) %>% mutate(movil = movil12(Sales))
  fig <- ggplot(t, aes(mes)) +
    geom_point(aes(y = Sales, text = sprintf("%s: %s", format(mes, "%b %Y"), usd(Sales))), color = REAL, size = 2) +
    geom_line(aes(y = movil), color = PROY, linewidth = 1.2, na.rm = TRUE) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas (USD)")
  v <- function(fecha) t$movil[t$mes == as.Date(fecha)]
  texto <- sprintf(paste(
    "La línea ámbar es el promedio móvil de 12 meses: quita la estacionalidad y deja ver la tendencia. Es casi",
    "plana entre 2014 y 2015 (%s por mes a fines de 2014 y %s a fines de 2015) y sube con claridad desde 2016,",
    "hasta %s a fines de 2017."), usd(v("2014-12-01")), usd(v("2015-12-01")), usd(v("2017-12-01")))
  list(fig = fig, texto = texto)
}

eda_estacionalidad <- function(df) {
  t <- df %>% mutate(anio = year(Order_Date), m = month(Order_Date)) %>%
    group_by(anio, m) %>% summarise(Sales = sum(Sales), .groups = "drop") %>%
    group_by(anio) %>% mutate(pct = Sales / sum(Sales) * 100) %>% ungroup() %>%
    mutate(mes = factor(MESES[m], levels = MESES), anio = factor(anio, levels = rev(sort(unique(anio)))))
  fig <- ggplot(t, aes(mes, anio, fill = pct, text = sprintf("%s %s: %.1f %% del año", mes, anio, pct))) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f", pct)), size = 3) +
    scale_fill_gradient(low = "#eaf3fb", high = "#2f6fa8", name = "% del año") +
    labs(x = NULL, y = NULL)
  bajos <- range(t$pct[t$m %in% 1:2])
  altos <- range(t$pct[t$m %in% c(9, 11, 12)])
  texto <- sprintf(paste(
    "Cada fila suma el 100 %% de las ventas del año. El patrón se repite los cuatro años: enero y febrero son los",
    "meses más bajos (entre %s y %s del año cada uno) y septiembre, noviembre y diciembre los más altos (entre",
    "%s y %s). Por eso el modelo usa el mes del año y las ventas del mismo mes del año anterior."),
    pct(bajos[1], 0), pct(bajos[2], 0), pct(altos[1], 0), pct(altos[2], 0))
  list(fig = fig, texto = texto)
}

eda_tendencia_categoria <- function(df) {
  t <- df %>% mutate(mes = floor_date(Order_Date, "month")) %>% group_by(Category, mes) %>%
    summarise(Sales = sum(Sales), .groups = "drop") %>% arrange(Category, mes) %>%
    group_by(Category) %>% mutate(movil = movil12(Sales)) %>% ungroup()
  fig <- ggplot(t, aes(mes, color = Category)) +
    geom_point(aes(y = Sales, text = sprintf("%s · %s: %s", Category, format(mes, "%b %Y"), usd(Sales))),
               alpha = .4, size = 1.5) +
    geom_line(aes(y = movil), linewidth = 1.2, na.rm = TRUE) +
    scale_color_manual(values = CATEGORY_COLORS, name = NULL) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas (USD)")
  ancho <- t %>% filter(!is.na(movil)) %>% select(Category, mes, movil) %>%
    pivot_wider(names_from = Category, values_from = movil)
  lider <- names(ancho)[-1][apply(as.matrix(ancho[, -1]), 1, which.max)]
  top <- names(sort(table(lider), decreasing = TRUE))[1]
  crec <- t %>% filter(mes %in% as.Date(c("2016-12-01", "2017-12-01"))) %>% group_by(Category) %>%
    summarise(c = movil[2] / movil[1] - 1, .groups = "drop") %>% arrange(desc(c))
  texto <- sprintf(paste(
    "Las líneas son el promedio móvil de 12 meses de cada categoría. Las tres crecen hacia el final del periodo;",
    "%s tiene el nivel más alto en %d de los %d meses, coherente con sus líneas de mayor valor. %s es la que más",
    "acelera en 2017 (%s frente a 2016)."),
    top, sum(lider == top), length(lider), crec$Category[1], pct(crec$c[1] * 100, 0, signo = TRUE))
  list(fig = fig, texto = texto)
}

# ---------------------------------------------------------------- atípicos y relación con el modelo
eda_atipicos <- function(df) {
  s <- df$Sales
  q <- quantile(s, c(.25, .75))
  lim <- q[[2]] + 1.5 * (q[[2]] - q[[1]])
  out <- s > lim
  p_lin <- mean(out) * 100
  p_ven <- sum(s[out]) / sum(s) * 100
  t <- tibble(
    grupo = rep(c("Líneas de pedido", "Ventas (USD)"), 2),
    tipo = factor(rep(c(paste0("Normales (≤ ", usd(lim), ")"), paste0("Atípicos (> ", usd(lim), ")")), each = 2)),
    valor = c(100 - p_lin, 100 - p_ven, p_lin, p_ven)
  ) %>% mutate(tipo = factor(tipo, levels = rev(levels(tipo))))
  fig <- ggplot(t, aes(grupo, valor, fill = tipo, text = sprintf("%s: %.1f %%", tipo, valor))) +
    geom_col(width = .6) +
    geom_text(aes(label = pct(valor)), position = position_stack(vjust = .5), size = 3.5) +
    scale_fill_manual(values = setNames(c(PROY, REAL), levels(t$tipo)), name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, " %")) +
    labs(x = NULL, y = "% del total")
  texto <- sprintf(paste(
    "Con la regla del rango intercuantil (atípico si supera Q3 + 1.5 × RIC = %s), %s líneas son atípicas: el %s",
    "de las líneas, pero el %s de las ventas. No son errores de captura sino las compras grandes que sostienen el",
    "negocio, por eso el ETL no las elimina. Explican los picos de algunos meses que ninguna variable puede",
    "anticipar, y por eso el modelo trabaja con ventas mensuales y no por línea."),
    usd(lim), num(sum(out)), pct(p_lin), pct(p_ven, 0))
  list(fig = fig, texto = texto, pct_lineas = p_lin, pct_ventas = p_ven)
}

eda_dispersion_rezago <- function(mensual) {
  # Gráfico de dispersión con línea de tendencia (geom_smooth con mínimos cuadrados)
  m <- agregar_variables(mensual) %>% filter(!is.na(indice), !is.na(r_lag_12), !is.na(r_lag_1))
  r12 <- cor(m$indice, m$r_lag_12)
  r1 <- cor(m$indice, m$r_lag_1)
  fig <- ggplot(m, aes(r_lag_12, indice)) +
    geom_point(aes(color = Category, text = sprintf("%s · %s · %s<br>%.2f → %.2f", Category, Region,
                                                    format(Mes, "%b %Y"), r_lag_12, indice)), alpha = .7) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = PROY, linewidth = 1.2) +
    scale_color_manual(values = CATEGORY_COLORS, name = NULL) +
    labs(x = "Índice del mismo mes del año anterior", y = "Índice del mes")
  texto <- sprintf(paste(
    "Cada punto es una categoría en una región durante un mes (2015–2017), en índice: ventas del mes ÷ promedio",
    "de los 12 meses anteriores. La línea de tendencia sube: un mes que vendió por encima de lo normal el año",
    "pasado tiende a repetirlo (correlación %.2f). Con el mes anterior la correlación es casi nula (%.2f). Es la",
    "relación que usa el modelo: pesa el mismo mes del año anterior, no el mes anterior. La nube es ancha, por",
    "eso el error por serie es alto."), r12, r1)
  list(fig = fig, texto = texto)
}

# ---------------------------------------------------------------- explorador interactivo (app)
VARIABLES_EXPLORADOR <- c("Categoría" = "Category", "Región" = "Region", "Segmento" = "Segment",
                          "Subcategoría" = "Sub-Category", "Modo de envío" = "Ship Mode",
                          "Ventas (Sales)" = "Sales", "Descuento" = "Discount", "Cantidad" = "Quantity",
                          "Ganancia (Profit)" = "Profit")

grafico_explorador <- function(d, variable, tipo) {
  # Gráfico del explorador del EDA según la variable y el tipo elegidos (como la app del curso)
  nombre <- names(VARIABLES_EXPLORADOR)[VARIABLES_EXPLORADOR == variable]
  categorica <- is.character(d[[variable]])
  if (tipo == "barras") {
    if (categorica) {
      t <- d %>% group_by(g = .data[[variable]], Category) %>% summarise(Ventas = sum(Sales), .groups = "drop")
      ggplot(t, aes(x = reorder(g, -Ventas, sum), y = Ventas, fill = Category,
                    text = sprintf("%s · %s: %s", g, Category, usd(Ventas)))) +
        geom_col() + scale_fill_manual(values = CATEGORY_COLORS, name = NULL) +
        scale_y_continuous(labels = dolares_k) +
        labs(x = nombre, y = "Ventas totales (USD)") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    } else {
      p <- ggplot(d, aes(x = .data[[variable]], fill = Category)) +
        geom_histogram(bins = 30, alpha = .7, position = "identity") +
        scale_fill_manual(values = CATEGORY_COLORS, name = NULL) + labs(x = nombre, y = "Líneas de pedido")
      if (variable == "Sales") p <- p + scale_x_log10(labels = dolares) + labs(x = "Ventas (escala log)")
      p
    }
  } else {
    if (categorica) {
      ggplot(d, aes(x = .data[[variable]], y = Sales, fill = .data[[variable]])) +
        geom_boxplot(alpha = .55, outlier.alpha = .3, show.legend = FALSE) +
        scale_y_log10(labels = dolares) + labs(x = nombre, y = "Ventas por línea (USD, escala log)") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    } else {
      p <- ggplot(d, aes(x = Category, y = .data[[variable]], fill = Category)) +
        geom_boxplot(alpha = .55, outlier.alpha = .3, show.legend = FALSE) +
        scale_fill_manual(values = CATEGORY_COLORS) + labs(x = "Categoría", y = nombre)
      if (variable == "Sales") p <- p + scale_y_log10(labels = dolares)
      p
    }
  }
}

# ---------------------------------------------------------------- geografía y otros factores de las ventas
ventas_estado <- function(df) {
  df %>% group_by(State, Region) %>%
    summarise(ventas = sum(Sales), mediana = median(Sales), lineas = n(), .groups = "drop") %>%
    mutate(share = ventas / sum(ventas) * 100, region = tolower(State)) %>% arrange(desc(ventas))
}

mapa_estados <- function(t, col, nombre, etiquetas) {
  # Mapa coroplético con sf y geom_sf (capítulo de visualización geográfica del curso): los límites de los
  # estados vienen del paquete maps y se convierten en un objeto sf con st_as_sf(). En gris, estados sin dato.
  estados <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
    rename(region = ID) %>% left_join(t, by = "region") %>%
    mutate(info = ifelse(is.na(State), "Sin dato",
                         sprintf("%s (%s)<br>Ventas totales: %s<br>Venta mediana por línea: %s<br>Líneas: %s",
                                 State, Region, usd(ventas), usd(mediana, 1), num(lineas))))
  ggplot(estados) +
    geom_sf(aes(fill = .data[[col]], text = info), color = "white", linewidth = .2) +
    scale_fill_gradient(low = "#eaf3fb", high = "#1f5570", na.value = "#c9ced8", name = nombre, labels = etiquetas) +
    coord_sf(crs = 5070) + # proyección cónica de Albers, la habitual para mapas de EE. UU.
    theme_void() + theme(legend.position = "right")
}

eda_mapa <- function(df) {
  # Ventas totales (Sales) por estado
  t <- ventas_estado(df)
  top <- head(t, 4)
  fig <- mapa_estados(t, "ventas", "Ventas", scales::label_dollar(scale = 1e-3, suffix = "k"))
  texto <- sprintf(paste(
    "Ventas totales (Sales) por estado. Se concentran en pocos estados: %s suman el %s del total, y solo %s",
    "aporta el %s. El estado explica mucho del total vendido, sobre todo por el número de pedidos."),
    enumerar(top$State), pct(sum(top$share), 0), top$State[1], pct(top$share[1], 0))
  list(fig = fig, texto = texto)
}

# ---------------------------------------------------------------- hallazgos y recomendaciones
hallazgos_eda <- function(df) {
  # Hallazgos del EDA convertidos en recomendaciones, calculados con los datos. Lo usan la sección
  # Hallazgos y recomendaciones del EDA y Conclusiones. Una fila por factor.
  total <- sum(df$Sales)
  # Peso de cada mes en las ventas y crecimiento anual
  mes <- df %>% group_by(m = month(Order_Date)) %>% summarise(v = sum(Sales) / total * 100, .groups = "drop")
  anual <- df %>% group_by(a = year(Order_Date)) %>% summarise(v = sum(Sales), .groups = "drop") %>%
    mutate(crec = (v / lag(v) - 1) * 100)
  # Descuentos de más de 20 %: su pérdida y el margen del resto
  alto <- df$Discount > 0.2
  margen_bajo <- sum(df$Profit[!alto]) / sum(df$Sales[!alto]) * 100
  # Subcategorías con pérdida y margen por categoría
  neg <- df %>% group_by(`Sub-Category`) %>% summarise(p = sum(Profit), d = mean(Discount), .groups = "drop") %>%
    filter(p < 0) %>% arrange(p)
  cat <- df %>% group_by(Category) %>% summarise(v = sum(Sales), p = sum(Profit), .groups = "drop") %>%
    mutate(m = p / v * 100, share = v / total * 100)
  fur <- cat %>% filter(Category == "Furniture")
  altos <- cat %>% filter(Category != "Furniture")
  # Región con peor y mejor margen, y estados de la peor que pierden dinero
  reg <- df %>% group_by(Region) %>% summarise(v = sum(Sales), p = sum(Profit), d = mean(Discount), .groups = "drop") %>%
    mutate(m = p / v * 100, share = v / total * 100)
  peor <- reg %>% slice_min(m, n = 1)
  mejor <- reg %>% slice_max(m, n = 1)
  est_neg <- df %>% filter(Region == peor$Region) %>% group_by(State) %>%
    summarise(p = sum(Profit), d = mean(Discount), .groups = "drop") %>% filter(p < 0) %>% arrange(p) %>% head(2)
  # Líneas atípicas por su valor (regla del rango intercuantil)
  q <- quantile(df$Sales, c(.25, .75))
  grande <- df$Sales > q[[2]] + 1.5 * (q[[2]] - q[[1]])
  # Ventas y clientes por segmento
  seg <- df %>% group_by(Segment) %>% summarise(v = sum(Sales), n = n_distinct(`Customer ID`), .groups = "drop") %>%
    mutate(vxc = v / n) %>% arrange(desc(v))
  estados <- if (nrow(est_neg)) sprintf("; %s pierden dinero con descuentos promedio de %d a %d %%.", enumerar(est_neg$State),
                                        round(min(est_neg$d) * 100), round(max(est_neg$d) * 100)) else "."

  tibble::tribble(
    ~Factor, ~Hallazgo, ~Recomendacion,
    "Temporada",
    sprintf("Septiembre a diciembre concentran el %s de las ventas del año; febrero, solo el %s. Es el factor más fuerte y el que más usa el modelo.",
            pct(sum(mes$v[mes$m >= 9]), 0), pct(mes$v[mes$m == 2])),
    "Planificar inventario, personal y campañas para septiembre a diciembre con la proyección mensual. Usar febrero para promociones controladas o para liquidar inventario.",
    "Tendencia",
    sprintf("Las ventas crecieron %s en 2016 y %s en 2017.", pct(anual$crec[anual$a == 2016], 0, signo = TRUE),
            pct(anual$crec[anual$a == 2017], 0, signo = TRUE)),
    "Fijar las metas de 2018 con la proyección como base, que ya incluye el crecimiento, y revisarlas cada mes con las ventas reales.",
    "Descuento",
    sprintf("El descuento casi no se relaciona con las ventas (correlación %.2f), pero las líneas con más de 20 %% de descuento perdieron %s. Con 20 %% o menos, el margen es de %s.",
            cor(df$Sales, df$Discount), usd(-sum(df$Profit[alto])), pct(margen_bajo, 0)),
    "Poner un tope de 20 % al descuento y pedir aprobación por encima de ese valor: es la acción con mayor efecto en la ganancia.",
    "Producto",
    sprintf("%s pierden dinero. Furniture vende el %s pero deja un margen de %s, frente a cerca de %s de %s.",
            enumerar(sprintf("%s (%d %% de descuento promedio)", neg$`Sub-Category`, round(neg$d * 100))),
            pct(fur$share, 0), pct(fur$m), pct(mean(altos$m), 0), enumerar(altos$Category)),
    sprintf("Revisar precios y descuentos de %s. Dar prioridad a %s en las campañas: cada venta deja más ganancia.",
            enumerar(head(neg$`Sub-Category`, 2)), enumerar(altos$Category)),
    "Región",
    sprintf("West y East suman el %s de las ventas. %s tiene el peor margen (%s) y un descuento promedio de %d %% (%s: %d %%)%s",
            pct(sum(reg$share[reg$Region %in% c("West", "East")]), 0), peor$Region, pct(peor$m, 0), round(peor$d * 100),
            mejor$Region, round(mejor$d * 100), estados),
    sprintf("Aplicar primero el tope de descuento en %s%s, y replicar ahí la política comercial de %s, la región con mejor margen.",
            peor$Region, if (nrow(est_neg)) paste0(", empezando por ", enumerar(est_neg$State)) else "", mejor$Region),
    "Pedidos grandes",
    sprintf("El %s de las líneas de pedido (las atípicas por su valor) genera el %s de las ventas.",
            pct(mean(grande) * 100, 0), pct(sum(df$Sales[grande]) / total * 100, 0)),
    "Dar atención y seguimiento dedicados a los clientes de pedidos grandes: perder uno pesa más que perder muchos pequeños.",
    "Segmento",
    sprintf("%s genera el %s de las ventas porque tiene más clientes (%d). El gasto por cliente es parecido en los tres segmentos (%s a %s en 4 años).",
            seg$Segment[1], pct(seg$v[1] / total * 100, 0), seg$n[1], usd(min(seg$vxc)), usd(max(seg$vxc))),
    sprintf("Para crecer, sumar clientes en %s, que gastan lo mismo por cliente pero son menos, en lugar de buscar subir el ticket.",
            enumerar(seg$Segment[-1]))
  )
}

eda_mapa_mediana <- function(df) {
  # Venta mediana por línea de pedido: la mediana con pocas líneas no es estable, solo estados con 30 o más
  t <- ventas_estado(df)
  ok <- t %>% filter(lineas >= 30)
  fig <- mapa_estados(ok, "mediana", "Mediana", dolares)
  g <- head(t, 3)
  texto <- sprintf(paste(
    "Venta mediana por línea de pedido en cada estado (solo los %d con 30 líneas o más; en gris, el resto). Va de",
    "%s a %s: el valor de cada compra cambia poco entre estados. Los que más venden en total no son los de compras",
    "más grandes (%s): su ventaja es el volumen. Por eso el modelo trabaja por región y no por estado."),
    nrow(ok), usd(min(ok$mediana)), usd(max(ok$mediana)), enumerar(sprintf("%s %s", g$State, usd(g$mediana))))
  list(fig = fig, texto = texto)
}
