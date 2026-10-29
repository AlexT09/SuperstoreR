# R/contenido.R — Textos y tablas fijas del proyecto (los usan la app Shiny y el libro).

PREGUNTAS <- c(
  "¿Qué factores (categoría, región, segmento, tiempo) explican el nivel de ventas (Sales)?",
  "¿Cuánto venderá cada categoría en cada región, mes a mes, durante 2018?"
)

OBJETIVO_GENERAL <- paste(
  "Identificar los factores que explican las ventas de Sample Superstore y construir un modelo de machine",
  "learning que proyecte las ventas mensuales por categoría y región para apoyar la planificación comercial.")

OBJETIVOS_ESPECIFICOS <- c(
  "Preparar los datos (ETL) y construir la tabla de ventas mensuales por categoría × región.",
  "Analizar la distribución de Sales y cómo varía según Category, Region, Segment y el tiempo (EDA).",
  "Entrenar y ajustar 7 modelos de regresión (Ridge, Lasso, KNN, Árbol de decisión, Random Forest, XGBoost y SVR).",
  "Compararlos entre sí y contra una referencia simple (vender lo mismo que el mismo mes del año anterior) con MAE, RMSE y R².",
  "Proyectar las ventas de 2018 con el mejor modelo y traducirlas en recomendaciones para el negocio."
)

IMPACTO <- c(
  "Metas comerciales: fijar la meta de cada región según lo que se espera vender.",
  "Inventario y compras: abastecer cada categoría antes de su temporada alta.",
  "Alerta temprana: detectar dónde se proyecta una caída frente al año anterior.",
  "Presupuesto: proyectar los ingresos del año siguiente."
)

VARIABLES_DATASET <- tribble(
  ~Variable, ~Definición, ~Tipo, ~`Escala / valores`, ~Rol,
  "Sales", "Valor monetario de la línea de pedido", "Numérica continua", "USD (≥ 0)", "EDA",
  "log(Sales)", "Logaritmo natural de Sales (corrige la asimetría)", "Numérica continua", "Reales", "EDA",
  "Category", "Familia de producto", "Categórica nominal", "Furniture, Office Supplies, Technology", "EDA y modelo",
  "Sub-Category", "Subcategoría de producto dentro de Category", "Categórica nominal", "17 subcategorías (p. ej. Chairs, Binders, Phones)", "EDA (explorador)",
  "Region", "Región geográfica del pedido", "Categórica nominal", "Central, East, South, West", "EDA y modelo",
  "State", "Estado de EE. UU. del pedido", "Categórica nominal", "49 estados", "EDA (mapa)",
  "Segment", "Tipo de cliente", "Categórica nominal", "Consumer, Corporate, Home Office", "EDA",
  "Ship Mode", "Modalidad de envío", "Categórica nominal", "Standard, Second, First Class, Same Day", "EDA (explorador)",
  "Quantity", "Unidades por línea de pedido", "Numérica discreta", "Enteros (1–14)", "EDA (correlación)",
  "Discount", "Descuento aplicado", "Numérica continua", "Proporción 0–0.8", "EDA (bins 2D y correlación)",
  "Profit", "Ganancia de la línea de pedido", "Numérica continua", "USD (puede ser negativa)", "EDA (correlación)",
  "Order Date", "Fecha del pedido", "Fecha", "2014-01-03 a 2017-12-30", "EDA y modelo (mes)"
)

VARIABLES_MODELO <- tribble(
  ~Variable, ~Definición, ~Tipo, ~`Escala / valores`, ~Rol,
  "Ventas del mes", "Suma de Sales de una categoría en una región durante un mes", "Numérica continua", "USD", "Lo que se proyecta",
  "Índice de ventas", "Ventas del mes ÷ promedio de los 12 meses anteriores (1.3 = 30 % más que el promedio)", "Numérica continua", "≥ 0", "Variable objetivo (y)",
  "Categoría", "Familia de producto de la serie", "Categórica nominal (one-hot)", "3 valores", "Variable independiente",
  "Región", "Región de la serie", "Categórica nominal (one-hot)", "4 valores", "Variable independiente",
  "Mes del año", "Posición del mes en el año como punto en un círculo (seno y coseno)", "Numérica continua", "−1 a 1", "Variable independiente (estacionalidad)",
  "Ventas del mismo mes del año anterior", "Ventas de hace 12 meses ÷ promedio de 12 meses", "Numérica continua", "≥ 0", "Variable independiente",
  "Ventas del mes anterior", "Ventas de hace 1 mes ÷ promedio de 12 meses", "Numérica continua", "≥ 0", "Variable independiente",
  "Promedio de los últimos 3 meses", "Media de los 3 meses previos ÷ promedio de 12 meses", "Numérica continua", "≥ 0", "Variable independiente (ritmo reciente)"
)

MODELOS_INFO <- tribble(
  ~Modelo, ~`Idea principal`, ~`Paquete de R`,
  "Ridge y Lasso", "Regresión lineal con penalización (L2 y L1) que reduce el sobreajuste; Lasso puede anular variables poco útiles.", "glmnet",
  "KNN", "Promedia las ventas de los k meses más parecidos del historial.", "propio (sin paquete)",
  "Árbol de decisión", "Divide los datos con reglas sí/no (p. ej. «¿es noviembre?») hasta agrupar meses similares.", "rpart",
  "Random Forest", "Promedia cientos de árboles entrenados con muestras distintas: más estable que un solo árbol.", "randomForest",
  "XGBoost", "Encadena árboles pequeños donde cada uno corrige el error de los anteriores (boosting).", "xgboost",
  "SVR", "Busca una función que deje la mayoría de los puntos dentro de un margen de error ε, con kernels para relaciones no lineales.", "e1071"
)

METRICAS_INFO <- list(
  MAE = "Error absoluto medio: cuántos dólares se equivoca la proyección en promedio. Es la métrica principal porque se lee en dólares.",
  RMSE = "Raíz del error cuadrático medio: castiga más los errores grandes (un mes muy mal proyectado pesa más).",
  "R²" = "Proporción de la variación de las ventas que el modelo explica (1 = perfecto, 0 = igual que usar el promedio)."
)

PASOS <- tribble(
  ~Paso, ~Detalle,
  "ETL", "CSV crudo → dataset limpio y tabla mensual (categoría × región × mes)",
  "EDA", "Distribución de las ventas, factores, estacionalidad y tendencia",
  "Variables", "Mes del año e historial de ventas, sin información del futuro",
  "Modelos", "7 regresores con el mismo preprocesamiento",
  "Ajuste", "Grid Search con validación cruzada temporal",
  "Evaluación", "2017 reservado: MAE, RMSE y R² frente a la referencia",
  "Proyección", "Mejor modelo reentrenado con 2015–2017 → 2018 mes a mes"
)

recomendaciones_negocio <- function(df, negrita = FALSE) {
  # Recomendaciones para el negocio: salen de los hallazgos del EDA (hallazgos_eda, R/eda.R)
  h <- hallazgos_eda(df)
  if (negrita) paste0("**", h$Factor, ":** ", h$Recomendacion) else paste0(h$Factor, ": ", h$Recomendacion)
}

LIMITACIONES <- c(
  "Dataset de ejemplo (2014–2017) de una sola tienda: no generaliza a otros negocios.",
  "Solo 36 meses aptos para modelar: las métricas tienen incertidumbre y un año atípico las cambiaría.",
  "Sin precios, costos, campañas ni datos externos (economía, competencia): el modelo solo conoce el historial de ventas y el calendario.",
  "La proyección de 2018 extiende el crecimiento reciente; si ese ritmo cambia, el error crecerá con cada mes proyectado.",
  "A nivel de una categoría en una región el error mensual es alto; la proyección es más confiable para totales (por región, por categoría o de la empresa).",
  "El EDA es asociativo: no demuestra causalidad."
)

conclusiones_modelo <- function() {
  r <- load_artifact("resumen.json")
  p <- r$prueba
  rf <- r$referencia
  rec <- r$prueba_recursiva
  c(
    sprintf("%s fue el mejor modelo por validación cruzada. En 2017 —un año que no vio— su error medio por serie fue de %s, un %s menor que repetir el año anterior (%s).",
            r$mejor_modelo, usd(p$MAE), pct((1 - p$MAE / rf$MAE) * 100, 0), usd(rf$MAE)),
    sprintf("Al proyectar 2017 completo solo con datos hasta 2016, el total anual quedó a %s del real (la referencia, a %s), con un error típico mensual de ±%s.",
            pct(rec$total_desvio_total_pct, signo = TRUE), pct(rf$total_desvio_total_pct, signo = TRUE), pct(rec$total_MAE_pct, 0)),
    "Las variables más importantes son el mes del año y las ventas del mismo mes del año anterior: el modelo confirma que la estacionalidad es el principal motor de las ventas.",
    sprintf("Para 2018 se proyectan %s, %s frente a 2017, con el pico entre septiembre y diciembre.",
            usd(r$proyeccion_2018), pct((r$proyeccion_2018 / r$ventas_2017 - 1) * 100, 0, signo = TRUE))
  )
}

conclusiones_eda <- function(df) {
  a <- eda_atipicos(df)
  c(
    sprintf(paste(
      "La distribución de Sales es muy sesgada hacia valores bajos, con pocas líneas de alto valor: las atípicas son",
      "el %s de las líneas pero el %s de las ventas, así que no se eliminan. Category es el factor que más explica",
      "diferencias en el valor de venta: Technology y Furniture venden líneas de mayor valor, mientras que Office",
      "Supplies domina en volumen con líneas de menor valor."), pct(a$pct_lineas), pct(a$pct_ventas, 0)),
    paste(
      "Region y Segment muestran diferencias leves en el valor de cada línea; la diferencia entre regiones está en",
      "el volumen, concentrado en pocos estados. El descuento no se asocia linealmente con el valor de la línea,",
      "pero sí con menor ganancia. En el tiempo, las ventas tienen un patrón estacional que se repite cada año",
      "(bajas en enero y febrero, altas de septiembre a diciembre) y crecen desde 2016.")
  )
}
