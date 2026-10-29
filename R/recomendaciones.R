# R/recomendaciones.R — Recomendaciones aplicadas: cómo cambiaría la proyección 2018 si el negocio aplica
# las recomendaciones del EDA. Lo usan la app Shiny y el libro.
#
# Proyección con recomendaciones = proyección base 2018 (model/train_model.R) × efecto de cada recomendación:
#   - Tope de descuento: efecto estimado con el modelo de margen por línea (model/train_descuentos.R),
#     simulado sobre los pedidos de 2017 y llevado a 2018 como un factor por categoría × región.
#   - Temporada alta e impulso de una categoría: supuestos que fija el usuario.
# La ganancia base de 2018 usa el margen de 2017 de cada categoría × región.

TOPES_PCT <- c(0, 10, 15, 20, 30, 40, 50, 60, 70, 80) # % de descuento que existen en los datos

recomendaciones_listas <- function() modelo_listo() && file.exists(file.path(ART, "descuentos_escenarios.csv"))

escenarios_descuento <- function() {
  load_artifact("descuentos_escenarios.csv") %>% mutate(tope = round(tope * 100))
}

efecto_tope <- function(tope, region = TODAS, perdida = 0.25) {
  # Por categoría × región: factor de ventas y margen después del tope (sobre los pedidos de 2017).
  # perdida = fracción de las líneas con descuento recortado que ya no se venden (supuesto).
  escenarios_descuento() %>% filter(tope == !!tope) %>%
    mutate(aplica = region == TODAS | Region == region,
           ventas = ifelse(aplica, ventas_base - ventas_cambian_antes + ventas_cambian_despues * (1 - perdida), ventas_base),
           ganancia = ifelse(aplica, ganancia_base - ganancia_cambian_antes + ganancia_cambian_despues * (1 - perdida),
                             ganancia_base)) %>%
    transmute(Category, Region, ventas_2017 = ventas_base, margen_base = ganancia_base / ventas_base,
              factor_ventas = ventas / ventas_base, margen_sim = ganancia / ventas)
}

simular_rec <- function(tope = 80, region = TODAS, perdida = 0.25, temporada = 0, categoria = NA, impulso = 0) {
  # Proyección 2018 por categoría × región × mes: base y con cada recomendación aplicada en orden.
  # tope = 80 equivale a no aplicar tope (es el descuento máximo de los datos).
  load_artifact("proyeccion_2018.csv") %>%
    left_join(efecto_tope(tope, region, perdida), by = c("Category", "Region")) %>%
    mutate(v_base = Proyeccion, g_base = v_base * margen_base,
           v1 = v_base * factor_ventas, g1 = v1 * margen_sim,                         # 1) tope de descuento
           v2 = v1 * ifelse(month(Mes) >= 9, 1 + temporada / 100, 1), g2 = v2 * margen_sim, # 2) temporada alta
           v3 = v2 * ifelse(!is.na(categoria) & Category == categoria, 1 + impulso / 100, 1), # 3) impulso
           g3 = v3 * margen_sim)
}

equilibrio <- function(tope, region = TODAS) {
  # Desde qué % de pedidos perdidos el tope deja de mejorar la ganancia y las ventas (líneas afectadas)
  e <- escenarios_descuento() %>% filter(tope == !!tope, region == TODAS | Region == region)
  punto <- function(antes, despues) {
    g0 <- despues - antes # cambio si no se pierde ningún pedido
    g1 <- -antes          # cambio si se pierden todos esos pedidos
    if (g0 <= 0) NA else if (g1 >= 0) 1 else g0 / (g0 - g1)
  }
  list(ganancia = punto(sum(e$ganancia_cambian_antes), sum(e$ganancia_cambian_despues)),
       ventas = punto(sum(e$ventas_cambian_antes), sum(e$ventas_cambian_despues)), lineas = sum(e$lineas_cambian))
}

ganancia_2017 <- function(tope, region = TODAS, perdida = 0.25) {
  # Ganancia que habría tenido 2017 con ese tope
  e <- efecto_tope(tope, region, perdida)
  sum(e$ventas_2017 * e$factor_ventas * e$margen_sim)
}

curva_topes <- function(region = TODAS, perdidas = 0.25) {
  expand.grid(tope = TOPES_PCT, perdida = perdidas) %>% rowwise() %>%
    mutate(ganancia = ganancia_2017(tope, region, perdida)) %>% ungroup()
}

# ---------------------------------------------------------------- gráficas (ggplot2 + ggplotly)
fig_rec_mensual <- function(p, nombre = "Con recomendaciones") {
  real <- load_mensual() %>% filter(year(Mes) >= 2015) %>% group_by(Mes) %>% summarise(Sales = sum(Sales), .groups = "drop")
  m <- p %>% group_by(Mes) %>% summarise(base = sum(v_base), sim = sum(v3), .groups = "drop")
  ult <- tail(real, 1)
  # Las líneas de 2018 arrancan en el último mes real para que las curvas queden unidas
  lineas <- bind_rows(
    tibble(Mes = ult$Mes, ventas = ult$Sales, serie = c("Proyección base 2018", nombre)),
    m %>% pivot_longer(c(base, sim), names_to = "s", values_to = "ventas") %>%
      mutate(serie = ifelse(s == "base", "Proyección base 2018", nombre)) %>% select(-s)
  ) %>% mutate(serie = factor(serie, levels = c("Proyección base 2018", nombre)))
  colores <- setNames(c(REAL, PROY, "#2ca58d"), c("Real", "Proyección base 2018", nombre))
  ggplot() +
    geom_line(data = real, aes(Mes, Sales, color = "Real")) +
    geom_point(data = real, aes(Mes, Sales, color = "Real", text = sprintf("%s · real: %s", format(Mes, "%b %Y"), usd(Sales))),
               size = 1.2) +
    geom_line(data = lineas, aes(Mes, ventas, color = serie, linetype = serie), linewidth = 1) +
    geom_point(data = lineas %>% filter(Mes > ult$Mes),
               aes(Mes, ventas, color = serie, text = sprintf("%s · %s: %s", format(Mes, "%b %Y"), serie, usd(ventas))),
               size = 1.8) +
    scale_color_manual(values = colores, breaks = names(colores), name = NULL) +
    scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
}

fig_rec_aportes <- function(p, pasos = c("Tope de descuento", "Temporada alta", "Impulso de categoría")) {
  # Cascada: de la ganancia base 2018 a la ganancia con recomendaciones, paso a paso
  g <- colSums(p[c("g_base", "g1", "g2", "g3")])
  t <- tibble(paso = c("Ganancia base", pasos, "Con recomendaciones"),
              ini = c(0, g[["g_base"]], g[["g1"]], g[["g2"]], 0),
              fin = c(g[["g_base"]], g[["g1"]], g[["g2"]], g[["g3"]], g[["g3"]])) %>%
    mutate(id = row_number(), cambio = fin - ini,
           tipo = case_when(id %in% c(1, 5) ~ "Total", cambio >= 0 ~ "Sube", TRUE ~ "Baja"),
           etiqueta = ifelse(tipo == "Total", usd(fin), paste0(ifelse(cambio >= 0, "+", "−"), usd(abs(cambio)))),
           paso = factor(paso, levels = paso), y_texto = pmax(ini, fin) + max(fin) * 0.05)
  ggplot(t, aes(text = sprintf("%s: %s", paso, etiqueta))) +
    geom_rect(aes(xmin = id - 0.4, xmax = id + 0.4, ymin = pmin(ini, fin), ymax = pmax(ini, fin), fill = tipo)) +
    geom_text(aes(x = id, y = y_texto, label = etiqueta), size = 3.5) +
    scale_fill_manual(values = c(Total = PROY, Sube = "#2ca58d", Baja = NEG), guide = "none") +
    scale_x_continuous(breaks = t$id, labels = levels(t$paso)) +
    scale_y_continuous(labels = dolares_k, expand = expansion(mult = c(0, .12))) +
    labs(x = NULL, y = "Ganancia 2018 (USD)")
}

fig_rec_curva <- function(region = TODAS, perdidas = 0.25, tope_actual = NA) {
  # Ganancia de 2017 con cada tope de descuento, para uno o varios supuestos de pedidos perdidos
  c <- curva_topes(region, perdidas) %>% mutate(serie = factor(sprintf("Se pierde el %d %% de esos pedidos", round(perdida * 100)),
                                                         levels = sprintf("Se pierde el %d %% de esos pedidos", round(perdidas * 100))))
  real <- ganancia_2017(80, region)
  colores <- setNames(c("#2ca58d", REAL, PROY, NEG)[seq_along(perdidas)], levels(c$serie))
  g <- ggplot(c, aes(tope, ganancia, color = serie, group = serie)) +
    geom_hline(yintercept = real, linetype = "dashed", color = GRIS) +
    annotate("text", x = 80, y = real, label = paste("Real 2017:", usd(real)), hjust = 1, vjust = 1.5, size = 3.3, color = GRIS) +
    geom_line(linewidth = 1) +
    geom_point(aes(text = sprintf("Tope %d %% · %s<br>Ganancia: %s", tope, serie, usd(ganancia))), size = 2) +
    scale_color_manual(values = colores, name = NULL) +
    scale_x_continuous(breaks = TOPES_PCT, labels = function(x) paste0(x, " %")) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = "Tope de descuento", y = "Ganancia 2017 simulada (USD)")
  if (!is.na(tope_actual)) g <- g + geom_vline(xintercept = tope_actual, linetype = "dotted", color = PROY)
  g
}

# ---------------------------------------------------------------- textos
lectura_rec <- function(p, tope_on, tope, region, temp_on, temp, cat_on, categoria, impulso) {
  partes <- c()
  if (tope_on && tope < 80) {
    eq <- equilibrio(tope, region)
    donde <- if (region == TODAS) "toda la empresa" else region
    partes <- c(partes, sprintf("Con un tope de %d %% en %s se recorta el descuento de %s líneas de pedido de 2017.",
                                tope, donde, num(eq$lineas)),
                if (is.na(eq$ganancia)) "Este tope no mejora la ganancia ni aunque no se pierda ningún pedido."
                else if (eq$ganancia >= 1) "La ganancia mejora aunque se pierdan todos esos pedidos, porque hoy dejan pérdida."
                else sprintf("La ganancia mejora mientras se pierda menos del %s de esos pedidos.", pct(eq$ganancia * 100, 0)),
                if (!is.na(eq$ventas) && eq$ventas < 1) sprintf("Las ventas no bajan si se pierde menos del %s.", pct(eq$ventas * 100, 0)))
  }
  if (temp_on && temp > 0) partes <- c(partes, sprintf("La temporada alta suma %s en ventas (supuesto de +%d %%).",
                                                       usd(sum(p$v2) - sum(p$v1)), temp))
  if (cat_on && impulso > 0) partes <- c(partes, sprintf("Impulsar %s suma %s en ventas (supuesto de +%d %%).",
                                                         categoria, usd(sum(p$v3) - sum(p$v2)), impulso))
  if (!length(partes)) return("Active una recomendación para ver cómo cambia la proyección.")
  gb <- sum(p$g_base)
  gs <- sum(p$g3)
  paste(c(partes, sprintf("En total, la ganancia de 2018 pasa de %s a %s (%s).", usd(gb), usd(gs),
                          pct((gs / gb - 1) * 100, 0, signo = TRUE))), collapse = " ")
}

texto_curva <- function(region = TODAS, perdida = 0.25) {
  c <- curva_topes(region, perdida)
  mejor <- c %>% slice_max(ganancia, n = 1, with_ties = FALSE)
  peor <- curva_topes(region, 1) %>% slice_max(ganancia, n = 1, with_ties = FALSE)
  real <- ganancia_2017(80, region)
  sprintf(paste(
    "Ganancia que habría tenido 2017 con cada tope, en %s y suponiendo que se pierde el %s de los pedidos con descuento",
    "recortado. Con este supuesto, el tope que deja más ganancia es %d %% (%s). Los topes bajos dependen mucho del supuesto,",
    "porque recortan descuentos pequeños que hoy sí dejan ganancia. En el peor caso, si se perdieran todos esos pedidos,",
    "el mejor tope sería %d %% (%s%s)."),
    if (region == TODAS) "toda la empresa" else region, pct(perdida * 100, 0), mejor$tope, usd(mejor$ganancia),
    peor$tope, usd(peor$ganancia), if (peor$ganancia > real) paste(", aún por encima de la ganancia real de", usd(real)) else "")
}

tabla_modelo_descuentos <- function() {
  r <- load_artifact("descuentos_resultados.csv")
  bind_rows(r %>% filter(!is.na(cv_MAE_margen_pts)) %>% arrange(cv_MAE_margen_pts), r %>% filter(is.na(cv_MAE_margen_pts))) %>%
    transmute(Modelo = modelo, `Error CV (puntos de margen)` = ifelse(is.na(cv_MAE_margen_pts), "—", sprintf("%.2f", cv_MAE_margen_pts)),
              `Error en 2017 (puntos)` = sprintf("%.2f", MAE_margen_pts),
              `Desvío de la ganancia 2017` = pct(desvio_ganancia_pct, signo = TRUE))
}

texto_modelo_descuentos <- function() {
  s <- load_artifact("descuentos_resumen.json")
  sprintf(paste(
    "Cada fila de entrenamiento es una línea de pedido (%s de 2014–2016) y el modelo predice su margen (ganancia ÷ venta)",
    "según descuento, cantidad, subcategoría, región y segmento. Se eligió %s por validación cruzada, igual que en la",
    "proyección. En 2017 se equivoca en %.1f puntos de margen por línea, casi lo mismo que la referencia (%.1f): no la",
    "supera en precisión. Se usa porque la referencia solo conoce combinaciones que ya ocurrieron, y el simulador",
    "necesita otras (por ejemplo, una subcategoría con 20 %% de descuento en una región donde siempre se vendió con 70 %%).",
    "Ninguno de los dos prueba causa: estiman lo que suele pasar en los datos."),
    num(s$lineas_entrenamiento), s$mejor_modelo, s$prueba$MAE_margen_pts, s$referencia$MAE_margen_pts)
}

tabla_rec_regiones <- function() {
  # Ganancia 2018 extra al aplicar el tope de 20 % en una sola región
  base <- sum(simular_rec()$g_base)
  tibble(Region = c("Central", "East", "South", "West")) %>% rowwise() %>%
    mutate(con25 = sum(simular_rec(20, Region, 0.25)$g1) - base, peor = sum(simular_rec(20, Region, 1)$g1) - base) %>%
    ungroup() %>% arrange(desc(con25))
}

SUPUESTOS_REC <- c(
  "La proyección base es la del mejor modelo (pestaña Proyección). La ganancia base de 2018 usa el margen de 2017 de cada categoría en cada región.",
  "Con menos descuento, cada pedido conserva sus unidades y paga más por ellas: en los datos el descuento no se relaciona con la cantidad por línea (correlación 0,01).",
  "Lo que los datos no dicen es cuántos clientes dejarían de comprar sin el descuento. Por eso es un supuesto ajustable, y la lectura indica hasta qué pérdida conviene el tope.",
  "Los aumentos por temporada y por categoría son supuestos: los datos no registran campañas, así que el modelo no puede estimarlos.",
  "Son escenarios estimados, no causas probadas. Antes de aplicar una recomendación en toda la empresa, conviene probarla en una región y comparar con las demás."
)
