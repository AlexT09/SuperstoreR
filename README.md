# Proyección de ventas Sample Superstore — R (Shiny + bookdown)

Análisis y proyección de ventas del dataset **Sample Superstore** (2014–2017), en dos partes:

1. **EDA:** ¿qué factores (categoría, región, segmento, tiempo) explican el nivel de ventas?
2. **Machine learning:** ¿cuánto venderá cada categoría en cada región, mes a mes, durante 2018?

Es la versión en R del proyecto [proyecto_superstorep2](https://github.com/AlexT09/proyecto_superstorep2)
(Python + Dash), con la misma metodología, las mismas pestañas y el mismo diseño.

Libro publicado: **https://alext09.github.io/rbook/**

## Contenido

- **App Shiny** (`app/`) con la estructura del capítulo *Visualización con shiny* del curso
  (`global.R`, `ui.R`, `server.R`, `shinydashboard`), con una sección del menú por etapa del proyecto.
- **Libro bookdown** (capítulos `.Rmd`, publicado desde `docs/`) con el mismo contenido en formato de lectura.

Los dos usan el mismo código (`R/`), así que muestran exactamente las mismas gráficas, cifras e
interpretaciones. Las gráficas siguen las técnicas del capítulo de visualización del curso (histograma,
barras, mapa, cajas con `stat_summary`, burbujas, contenedores 2D, correlación y dispersión con línea de
tendencia) hechas con **ggplot2** y vueltas interactivas con `ggplotly`.

| Pestaña / capítulo | Qué muestra |
|---|---|
| Introducción · Problema y objetivos · Marco teórico · Metodología | Planteamiento, modelos, variables y flujo |
| ETL | Del CSV crudo a la tabla mensual que usa el modelo |
| EDA | Distribución de las ventas, factores, estacionalidad, tendencia, atípicos y relación con el mes del año anterior |
| Modelos y resultados | 7 modelos evaluados en 2017, rejillas de Grid Search, sobreajuste e importancia de variables |
| Comparación | Ranking de los modelos frente a la referencia y prueba de la proyección a 12 meses |
| Proyección 2018 | Proyección por categoría, región y mes, con su error típico (filtrable en la app) |
| Entrenamiento del modelo (solo app) | Entrena el modelo elegido al presionar un botón y permite guardarlo como `.rds` |
| Predicción (solo app) | Formulario con ejemplos de temporada alta y baja: carga `model.rds` y predice las ventas de un mes |
| Conclusiones | Hallazgos, recomendaciones y limitaciones |

## Modelo

- **Qué predice:** las ventas del mes de cada categoría × región (12 series × 48 meses).
- **Variables:** categoría, región, mes del año y el historial de ventas (mismo mes del año anterior,
  mes anterior, promedio de 3 meses), todas conocidas antes de empezar el mes.
- **Objetivo:** un índice, ventas del mes ÷ promedio de los 12 meses anteriores. Se convierte a
  dólares al final, así la misma regla sirve para series grandes y pequeñas.
- **Modelos:** Ridge y Lasso (`glmnet`), KNN (propio), Árbol de decisión (`rpart`), Random Forest
  (`randomForest`), XGBoost (`xgboost`) y SVR (`e1071`), con el mismo preprocesamiento (one-hot +
  estandarización).
- **Ajuste:** Grid Search con validación cruzada temporal (4 pliegues de 3 meses).
- **Evaluación:** entrenamiento 2015–2016, prueba 2017, contra la referencia "repetir el mismo mes
  del año anterior". El mejor modelo se elige por validación cruzada, no por la prueba. El árbol de
  decisión individual se compara pero no es elegible como modelo final, por inestable.
- **Proyección 2018:** recursiva, mes a mes. Se valida igual: proyectando 2017 completo solo con
  datos hasta 2016.

Resultado actual: Random Forest, MAE por serie de $2.697 frente a $2.912 de la referencia. En la
prueba a 12 meses el total anual quedó a +4,4 % del real (la referencia, a −16,9 %). Proyección 2018:
$909.045 (+24 % frente a 2017).

## Requisitos

- **R** 4.1 o superior y **RStudio** (trae pandoc, que necesita el libro)
- Paquetes:

```r
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "plotly", "scales", "maps", "dplyr", "tidyr",
                   "readr", "lubridate", "jsonlite", "glmnet", "rpart", "randomForest", "xgboost", "e1071",
                   "bookdown", "knitr"))
```

## Cómo usar el proyecto

Abre `Rbook.Rproj` en RStudio (o una consola de R en la raíz del repositorio).

**App Shiny:**

```r
shiny::runApp("app")
```

**Libro** (queda en `docs/`, que es lo que publica GitHub Pages):

```powershell
Rscript build.R
```

Los datos procesados y los resultados del modelo ya vienen en el repositorio. Si cambias el CSV
original o el modelo, regenera en este orden:

```powershell
Rscript data/generate_data.R      # CSV crudo -> superstore_transformado.csv y ventas_mensuales.csv
Rscript model/train_model.R       # entrena y compara (~25 s) -> model/artifacts/
Rscript build.R                   # libro -> docs/
```

## Estructura

```
├── app/                       # App Shiny (estructura del curso)
│   ├── global.R               # Librerías, código compartido, datos y resultados del modelo
│   ├── ui.R                   # dashboardPage(): cabecera, menú lateral y una pestaña por sección
│   ├── server.R               # Lógica reactiva de cada sección
│   └── www/                   # Logo, favicon y ajustes de estilo (custom.css)
├── R/                         # Código compartido por la app y el libro
│   ├── common.R               # Rutas, carga de datos y resultados, colores y estilo de las figuras
│   ├── eda.R                  # Gráficas del EDA (ggplot2, técnicas del curso) e interpretaciones
│   ├── resultados.R           # Gráficas y textos de modelos, comparación y proyección
│   └── contenido.R            # Textos y tablas fijas (objetivos, variables, limitaciones…)
├── data/
│   ├── raw/                   # Dataset original
│   ├── generate_data.R        # ETL
│   ├── superstore_transformado.csv
│   └── ventas_mensuales.csv   # Categoría × región × mes (entrada del modelo)
├── model/
│   ├── features.R             # Variables y preprocesamiento
│   ├── modelos.R              # Los 7 modelos y sus rejillas de hiperparámetros
│   ├── evaluate.R             # Métricas e importancia de variables
│   ├── entrenamiento.R        # Validación cruzada temporal, Grid Search y proyección recursiva
│   ├── train_model.R          # Entrena, compara y proyecta 2018
│   └── artifacts/             # model.rds, resultados, predicciones y proyección (generados)
├── index.Rmd … 10-app-shiny.Rmd   # Capítulos del libro
├── _bookdown.yml · _output.yml    # Configuración del libro
├── build.R                    # Construye el libro en docs/
├── style.css                  # Estilo del libro
└── docs/                      # Libro generado (GitHub Pages)
```

## Problemas comunes

- **`there is no package called ...`:** instala los paquetes de la sección Requisitos.
- **El libro no compila (`pandoc` no encontrado):** ejecútalo desde RStudio, que trae pandoc.
- **Las pestañas del modelo dicen "Modelo no entrenado":** ejecuta `Rscript model/train_model.R`.

---

Proyecto: Alex Teran y David Estrada
