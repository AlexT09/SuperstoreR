# R/resultados.R — Gráficas, tablas y textos de los resultados del modelo (Modelos, Comparación y
# Proyección 2018). Las usan la app Shiny y el libro; todo se lee de model/artifacts/.

TODAS <- "Todas"

fmt_params <- function(json) {
  # {"profundidad":5,"hoja":3} -> "profundidad=5, hoja=3" (NA = sin límite)
  p <- jsonlite::fromJSON(json)
  paste(sprintf("%s=%s", names(p), sapply(p, function(v) if (is.null(v) || is.na(v)) "sin límite" else format(v))),
        collapse = ", ")
}

fmt_rejilla <- function(json) {
  r <- jsonlite::fromJSON(json)
  paste(sprintf("%s ∈ {%s}", names(r), sapply(r, function(v) paste(ifelse(is.na(v), "sin límite", v), collapse = ", "))),
        collapse = "; ")
}

# ---------------------------------------------------------------- Modelos
fig_ajuste <- function(modelo) {
  # Total mensual de la empresa: real vs. proyectado, en entrenamiento y prueba
  p <- load_artifact("predicciones.csv")
  t <- p %>% group_by(Mes, conjunto) %>% summarise(Sales = sum(Sales), pred = sum(.data[[modelo]]), .groups = "drop")
  ini <- min(t$Mes[t$conjunto == "Prueba"])
  ggplot(t, aes(Mes)) +
    annotate("rect", xmin = ini, xmax = max(t$Mes), ymin = -Inf, ymax = Inf, fill = REAL, alpha = .07) +
    annotate("text", x = ini, y = max(t$Sales, t$pred) * 1.05, label = "Prueba (2017)", hjust = 0, color = GRIS, size = 3.5) +
    geom_line(aes(y = Sales, color = "Real")) +
    geom_point(aes(y = Sales, color = "Real", text = sprintf("%s · real: %s", format(Mes, "%b %Y"), usd(Sales))), size = 1.5) +
    geom_line(aes(y = pred, color = "Proyección", linetype = conjunto, group = conjunto), linewidth = 1) +
    geom_point(aes(y = pred, color = "Proyección", text = sprintf("%s · proyección: %s", format(Mes, "%b %Y"), usd(pred))),
               size = .1, alpha = 0) +
    scale_color_manual(values = c(Real = REAL, `Proyección` = PROY), name = NULL) +
    scale_linetype_manual(values = c(Entrenamiento = "dotted", Prueba = "solid"), name = NULL) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
}

errores_por_conjunto <- function() {
  # MAE de cada modelo en entrenamiento y en prueba, como % de la venta media de ese periodo.
  # En porcentaje porque 2017 vende más que 2015–2016: en dólares el error de prueba sube aunque el
  # modelo no haya empeorado (le pasa incluso a la referencia).
  p <- load_artifact("predicciones.csv")
  modelos <- setdiff(names(p), c("Category", "Region", "Mes", "conjunto", "Sales"))
  bind_rows(lapply(modelos, function(m) {
    e <- p %>% group_by(conjunto) %>% summarise(v = mean(abs(Sales - .data[[m]])) / mean(Sales) * 100, .groups = "drop")
    tibble(modelo = m, Entrenamiento = e$v[e$conjunto == "Entrenamiento"], Prueba = e$v[e$conjunto == "Prueba"])
  })) %>% mutate(brecha = Prueba - Entrenamiento) %>% arrange(Prueba)
}

fig_sobreajuste <- function(t) {
  d <- t %>% mutate(modelo = factor(modelo, levels = t$modelo)) %>%
    pivot_longer(c(Entrenamiento, Prueba), names_to = "conjunto", values_to = "error") %>%
    mutate(conjunto = recode(conjunto, Entrenamiento = "Entrenamiento (2015–2016)", Prueba = "Prueba (2017)"))
  ggplot(d, aes(modelo, error, fill = conjunto, text = sprintf("%s · %s: %.1f %%", modelo, conjunto, error))) +
    geom_col(position = position_dodge(width = .8), width = .75) +
    scale_fill_manual(values = c("#bcd6f0", REAL), name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, " %")) +
    labs(x = NULL, y = "MAE (% de la venta media por serie)") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

fig_importancia <- function() {
  imp <- load_artifact("importancia.csv") %>% mutate(nombre = reorder(nombre, importancia))
  ggplot(imp, aes(importancia, nombre, text = sprintf("%s: %.4f (± %.4f)", nombre, importancia, std))) +
    geom_col(fill = REAL, width = .7) +
    geom_errorbar(aes(xmin = importancia - std, xmax = importancia + std), width = .25, orientation = "y", color = GRIS) +
    geom_vline(xintercept = 0, color = GRIS) +
    labs(x = "Aumento del error al desordenar la variable", y = NULL)
}

textos_modelos <- function() {
  r <- load_artifact("resumen.json")
  res <- mejores_por_modelo()
  ref <- load_artifact("resultados_modelos.csv") %>% filter(modelo == REFERENCIA)
  best <- r$mejor_modelo
  p <- r$prueba
  sob <- errores_por_conjunto()
  sm <- sob %>% filter(modelo != REFERENCIA)
  b <- sob %>% filter(modelo == best)
  rr <- sob %>% filter(modelo == REFERENCIA)
  mayor <- sm %>% slice_max(brecha, n = 1)
  estables <- sm$modelo[sm$brecha <= 1]
  imp <- load_artifact("importancia.csv")
  sin_aporte <- imp$nombre[imp$importancia <= 0]
  list(
    metricas = sprintf(paste(
      "MAE y RMSE miden el error de cada serie mensual (la venta media por serie y mes en 2017 fue de %s), por eso",
      "su R² es bajo: una categoría en una región varía mucho de un mes a otro. Al sumar las series, el error del",
      "total mensual de la empresa baja a %s. %d de %d modelos superan a la referencia en MAE."),
      usd(r$ventas_2017 / 144), pct(p$total_MAE_pct, 0), sum(res$MAE < ref$MAE), nrow(res)),
    sobreajuste = sprintf(paste(
      "Un modelo sobreajustado se equivoca poco con los datos que vio y mucho con los nuevos. %s La mayor brecha",
      "es la de %s (%s → %s), de los modelos más flexibles, que memorizan parte del entrenamiento. %s pasa de %s a",
      "%s, y en 2017 sigue por debajo de la referencia (%s). La validación cruzada mide el error en meses no vistos,",
      "así que al elegir el modelo ya tiene en cuenta este efecto."),
      if (length(estables)) paste0("Sin brecha relevante (1 punto o menos): ", enumerar(c(estables, "la referencia")), ".")
      else "Todos los modelos tienen algo de brecha.",
      mayor$modelo, pct(mayor$Entrenamiento, 0), pct(mayor$Prueba, 0), best, pct(b$Entrenamiento, 0),
      pct(b$Prueba, 0), pct(rr$Prueba, 0)),
    importancia = paste(
      "Importancia por permutación en 2017: cuánto aumenta el error si se desordena cada variable. Dominan el mes",
      "del año y las ventas del mismo mes del año anterior, es decir, la estacionalidad que mostró el EDA.",
      if (length(sin_aporte)) sprintf(paste(
        "%s %s importancia cero o negativa: desordenarlas no empeora el modelo, o sea que no aportan información",
        "útil en 2017."), enumerar(sin_aporte), if (length(sin_aporte) > 1) "tienen" else "tiene") else "",
      "Categoría y región pesan poco porque el índice ya pone cada serie en su propia escala.")
  )
}

# ---------------------------------------------------------------- Comparación
fig_ranking <- function() {
  res <- mejores_por_modelo()
  ref <- load_artifact("resultados_modelos.csv") %>% filter(modelo == REFERENCIA)
  best <- load_artifact("resumen.json")$mejor_modelo
  d <- res %>% mutate(
    etiqueta = reorder(ifelse(elegible, modelo, paste0(modelo, " (no elegible)")), -MAE),
    grupo = ifelse(modelo == best, "Elegido", ifelse(elegible, "Elegible", "No elegible")))
  ggplot(d, aes(MAE, etiqueta, fill = grupo, text = sprintf("%s: %s", etiqueta, usd(MAE)))) +
    geom_col(width = .7) +
    geom_text(aes(label = usd(MAE)), hjust = 1.1, size = 3.5) +
    geom_vline(xintercept = ref$MAE, linetype = "dashed", color = GRIS) +
    annotate("text", x = ref$MAE, y = nrow(d) + .6, label = paste("Referencia", usd(ref$MAE)), hjust = 1, size = 3.5, color = GRIS) +
    scale_fill_manual(values = c(Elegido = "#2ca58d", Elegible = REAL, `No elegible` = "#c3c9d2"), name = NULL) +
    scale_x_continuous(labels = dolares, limits = c(0, ref$MAE * 1.12)) +
    labs(x = "MAE en 2017 (USD, menor es mejor)", y = NULL)
}

texto_ranking <- function() {
  res <- mejores_por_modelo()
  r <- load_artifact("resumen.json")
  best <- r$mejor_modelo
  elegido <- res %>% filter(modelo == best)
  menor <- res %>% filter(elegible) %>% slice_min(MAE, n = 1)
  excluido <- res %>% filter(!elegible)
  nota_menor <- if (menor$modelo == best) "" else sprintf(paste(
    " %s tuvo el menor MAE entre los elegibles en 2017 (%s) y no se eligió porque elegir mirando 2017 haría que esa",
    "prueba dejara de ser independiente: en validación cruzada su error fue %.3f, contra %.3f de %s."),
    menor$modelo, usd(menor$MAE), menor$cv_MAE_indice, elegido$cv_MAE_indice, best)
  nota_arbol <- if (nrow(excluido)) sprintf(paste(
    " El %s se entrena y se compara, pero no es elegible como modelo final: un solo árbol es inestable (cambios",
    "mínimos en los datos cambian sus cortes); para eso están sus versiones estables, Random Forest y XGBoost."),
    tolower(excluido$modelo[1])) else ""
  sprintf(paste(
    "Los modelos con la barra a la izquierda de la línea superan a la referencia. Las diferencias entre ellos son",
    "pequeñas (de %s a %s): con 288 filas de entrenamiento, la señal disponible es sobre todo la estacionalidad, y",
    "todos la capturan. %s (en verde) se eligió por su error de validación cruzada, antes de mirar 2017.%s%s"),
    usd(min(res$MAE)), usd(max(res$MAE)), best, nota_menor, nota_arbol)
}

fig_recursiva <- function() {
  rec <- load_artifact("prueba_recursiva_2017.csv") %>% group_by(Mes) %>%
    summarise(`Real 2017` = sum(Sales), `Proyección a 12 meses` = sum(Proyeccion), .groups = "drop")
  ref <- load_artifact("predicciones.csv") %>% filter(conjunto == "Prueba") %>% group_by(Mes) %>%
    summarise(`Referencia (repetir 2016)` = sum(.data[[REFERENCIA]]), .groups = "drop")
  d <- rec %>% inner_join(ref, by = "Mes") %>% pivot_longer(-Mes, names_to = "serie", values_to = "ventas") %>%
    mutate(serie = factor(serie, levels = c("Real 2017", "Proyección a 12 meses", "Referencia (repetir 2016)")))
  ggplot(d, aes(Mes, ventas, color = serie, linetype = serie, group = serie)) +
    geom_line(linewidth = 1) +
    geom_point(aes(text = sprintf("%s · %s: %s", format(Mes, "%b"), serie, usd(ventas))), size = 1.5) +
    scale_color_manual(values = c(REAL, PROY, GRIS), name = NULL) +
    scale_linetype_manual(values = c("solid", "solid", "dashed"), name = NULL) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
}

# ---------------------------------------------------------------- Proyección 2018
filtrar <- function(df, cat, reg) {
  if (cat != TODAS) df <- df %>% filter(Category == cat)
  if (reg != TODAS) df <- df %>% filter(Region == reg)
  df
}

error_esperado <- function(cat = TODAS, reg = TODAS) {
  # Error de la proyección a 12 meses para este filtro, medido al proyectar 2017 con datos hasta 2016
  t <- filtrar(load_artifact("prueba_recursiva_2017.csv"), cat, reg) %>% group_by(Mes) %>%
    summarise(Sales = sum(Sales), Proyeccion = sum(Proyeccion), .groups = "drop")
  mean(abs(t$Sales - t$Proyeccion)) / mean(t$Sales) * 100
}

datos_proyeccion <- function(cat = TODAS, reg = TODAS) {
  real <- filtrar(load_mensual(), cat, reg) %>% filter(year(Mes) >= 2015) %>% group_by(Mes) %>%
    summarise(Sales = sum(Sales), .groups = "drop")
  proy <- filtrar(load_artifact("proyeccion_2018.csv"), cat, reg) %>% group_by(Mes) %>%
    summarise(Proyeccion = sum(Proyeccion), .groups = "drop")
  err <- error_esperado(cat, reg)
  list(real = real, proy = proy %>% mutate(lo = Proyeccion * (1 - err / 100), hi = Proyeccion * (1 + err / 100)), err = err)
}

fig_proyeccion <- function(cat = TODAS, reg = TODAS) {
  d <- datos_proyeccion(cat, reg)
  # La línea de la proyección arranca en el último mes real para que las dos curvas queden unidas
  union <- bind_rows(d$real %>% tail(1) %>% rename(Proyeccion = Sales), d$proy %>% select(Mes, Proyeccion))
  ggplot() +
    geom_ribbon(data = d$proy, aes(Mes, ymin = lo, ymax = hi), fill = PROY, alpha = .2) +
    geom_line(data = d$real, aes(Mes, Sales, color = "Real")) +
    geom_point(data = d$real, aes(Mes, Sales, color = "Real", text = sprintf("%s · real: %s", format(Mes, "%b %Y"), usd(Sales))), size = 1.2) +
    geom_line(data = union, aes(Mes, Proyeccion, color = "Proyección 2018"), linetype = "dashed", linewidth = 1) +
    geom_point(data = d$proy, aes(Mes, Proyeccion, color = "Proyección 2018",
                                  text = sprintf("%s · proyección: %s (rango %s – %s)", format(Mes, "%b %Y"), usd(Proyeccion), usd(lo), usd(hi))), size = 1.8) +
    scale_color_manual(values = c(Real = REAL, `Proyección 2018` = PROY), name = NULL) +
    scale_y_continuous(labels = dolares_k) +
    labs(x = NULL, y = "Ventas del mes (USD)")
}

kpis_proyeccion <- function(cat = TODAS, reg = TODAS) {
  d <- datos_proyeccion(cat, reg)
  v17 <- sum(d$real$Sales[year(d$real$Mes) == 2017])
  v18 <- sum(d$proy$Proyeccion)
  pico <- d$proy %>% slice_max(Proyeccion, n = 1)
  list(
    list("Proyección 2018", usd(v18), paste("real 2017:", usd(v17))),
    list("Variación esperada", pct((v18 / v17 - 1) * 100, signo = TRUE), "frente a 2017"),
    list("Mes pico", MESES[month(pico$Mes)], usd(pico$Proyeccion)),
    list("Error típico mensual", paste0("±", pct(d$err, 0)), "proyección a 12 meses (prueba 2017)")
  )
}

tabla_series <- function() {
  # Real 2017 vs proyección 2018 por categoría × región, ordenada por la proyección
  real <- load_mensual() %>% filter(year(Mes) == 2017) %>% group_by(Category, Region) %>%
    summarise(v2017 = sum(Sales), .groups = "drop")
  load_artifact("proyeccion_2018.csv") %>% group_by(Category, Region) %>%
    summarise(v2018 = sum(Proyeccion), .groups = "drop") %>%
    inner_join(real, by = c("Category", "Region")) %>%
    mutate(crec = (v2018 / v2017 - 1) * 100) %>% arrange(desc(v2018))
}

lectura_negocio <- function() {
  r <- load_artifact("resumen.json")
  t <- tabla_series()
  p18 <- load_artifact("proyeccion_2018.csv")
  temporada <- sum(p18$Proyeccion[month(p18$Mes) >= 9]) / sum(p18$Proyeccion) * 100
  top <- t %>% slice_max(crec, n = 1)
  bajo <- t %>% slice_min(crec, n = 1)
  caidas <- t %>% filter(crec < 0)
  c(
    sprintf("Se proyectan %s en 2018, %s frente a 2017 (%s). El modelo extiende el crecimiento de 2016–2017.",
            usd(r$proyeccion_2018), pct((r$proyeccion_2018 / r$ventas_2017 - 1) * 100, 0, signo = TRUE), usd(r$ventas_2017)),
    sprintf("La temporada alta (septiembre a diciembre) concentra el %s de las ventas del año en solo 4 meses: es el periodo para asegurar inventario y personal.",
            pct(temporada, 0)),
    sprintf("Mayor crecimiento esperado: %s en %s (%s).", top$Category, top$Region, pct(top$crec, 0, signo = TRUE)),
    if (nrow(caidas)) sprintf("Combinaciones con caída proyectada: %s.",
                              paste(sprintf("%s en %s (%s)", caidas$Category, caidas$Region, pct(caidas$crec, 0, signo = TRUE)), collapse = ", "))
    else sprintf("Ninguna combinación cae; la de menor crecimiento es %s en %s (%s): vigilarla mes a mes.",
                 bajo$Category, bajo$Region, pct(bajo$crec, 0, signo = TRUE)),
    "Usar la proyección como punto de partida para metas e inventario, ajustada con lo que el modelo no conoce: campañas, precios y competencia."
  )
}

# ---------------------------------------------------------------- Resumen ejecutivo (app)
comparacion_2017_2018 <- function(cat = TODAS, reg = TODAS) {
  # Real 2017 vs. proyección 2018: por región, o por categoría si se eligió una región
  por <- if (reg == TODAS) "Region" else "Category"
  real <- filtrar(load_mensual(), cat, reg) %>% filter(year(Mes) == 2017) %>%
    group_by(g = .data[[por]]) %>% summarise(`Real 2017` = sum(Sales), .groups = "drop")
  filtrar(load_artifact("proyeccion_2018.csv"), cat, reg) %>% group_by(g = .data[[por]]) %>%
    summarise(`Proyección 2018` = sum(Proyeccion), .groups = "drop") %>%
    inner_join(real, by = "g") %>% mutate(crec = (`Proyección 2018` / `Real 2017` - 1) * 100) %>%
    arrange(desc(`Proyección 2018`))
}

fig_barras_2017_2018 <- function(cat = TODAS, reg = TODAS) {
  t <- comparacion_2017_2018(cat, reg)
  d <- t %>% pivot_longer(c(`Real 2017`, `Proyección 2018`), names_to = "serie", values_to = "ventas") %>%
    mutate(g = factor(g, levels = t$g), serie = factor(serie, levels = c("Real 2017", "Proyección 2018")))
  ggplot(d, aes(g, ventas, fill = serie, text = sprintf("%s · %s: %s", g, serie, usd(ventas)))) +
    geom_col(position = position_dodge(width = .8), width = .75) +
    geom_text(data = t, aes(x = g, y = `Proyección 2018` * 1.09, label = pct(crec, 0, signo = TRUE)),
              inherit.aes = FALSE, size = 3.6, color = "#1e8e5a", fontface = "bold") +
    scale_fill_manual(values = c(`Real 2017` = REAL, `Proyección 2018` = PROY), name = NULL) +
    scale_y_continuous(labels = dolares_k, expand = expansion(mult = c(0, .12))) +
    labs(x = NULL, y = "Ventas del año (USD)")
}

fig_calor_proyeccion <- function(cat = TODAS, reg = TODAS) {
  # Proyección 2018 mes a mes: por categoría, o por región si se eligió una categoría
  por <- if (cat == TODAS) "Category" else "Region"
  t <- filtrar(load_artifact("proyeccion_2018.csv"), cat, reg) %>%
    group_by(g = .data[[por]], m = month(Mes)) %>% summarise(v = sum(Proyeccion), .groups = "drop") %>%
    mutate(mes = factor(MESES[m], levels = MESES))
  ggplot(t, aes(mes, g, fill = v, text = sprintf("%s · %s 2018: %s", g, mes, usd(v)))) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = dolares_k(v)), size = 3) +
    scale_fill_gradient(low = "#fdf3df", high = "#d9891a", name = "USD") +
    labs(x = NULL, y = NULL)
}

texto_resumen <- function(cat = TODAS, reg = TODAS) {
  # Lectura de la selección: nivel, crecimiento, temporada alta y confianza
  t <- comparacion_2017_2018(cat, reg)
  p <- filtrar(load_artifact("proyeccion_2018.csv"), cat, reg)
  temporada <- sum(p$Proyeccion[month(p$Mes) >= 9]) / sum(p$Proyeccion) * 100
  err <- error_esperado(cat, reg)
  nombre <- if (cat == TODAS && reg == TODAS) "La empresa" else if (cat == TODAS) paste("La región", reg)
             else if (reg == TODAS) cat else paste(cat, "en", reg)
  sprintf(paste(
    "%s proyecta %s en 2018 (%s frente a 2017); septiembre a diciembre concentra el %s del año. La banda ámbar es",
    "el error típico de la proyección para esta selección (±%s, confianza %s), medido al proyectar 2017 sin conocerlo."),
    nombre, usd(sum(t$`Proyección 2018`)), pct((sum(t$`Proyección 2018`) / sum(t$`Real 2017`) - 1) * 100, 0, signo = TRUE),
    pct(temporada, 0), pct(err, 0), if (err <= 20) "alta" else if (err <= 35) "media" else "baja")
}
