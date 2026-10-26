# Proyecto Sample Superstore — bookdown (R)

Análisis exploratorio de datos (EDA) del dataset **Sample Superstore** para responder la
pregunta de negocio: **¿qué factores (categoría, región, segmento) explican el nivel de ventas?**

Libro publicado: **https://alext09.github.io/rbook/**

## Requisitos

- **R** 4.1 o superior, con los paquetes `bookdown`, `tidyverse` y `plotly`
- **pandoc** (viene incluido con RStudio)
- Para la app Shiny: `shiny`, `shinydashboard`, `DT`, `caret` y `randomForest`

```r
install.packages(c("bookdown", "tidyverse", "plotly",
                   "shiny", "shinydashboard", "DT", "caret", "randomForest"))
```

## Cómo construir el libro

Desde RStudio (abriendo `Rbook.Rproj`) o desde una terminal:

```powershell
Rscript build.R
```

El libro queda en `docs/`, que es la carpeta que publica GitHub Pages (rama `main`, carpeta `/docs`).
`build.R` construye el libro con `bookdown::render_book()` y copia el dashboard a `docs/dashboard/`.

Para verlo con el panel de ventas funcionando, sírvelo por HTTP:

```powershell
python -m http.server 8000 --directory docs
```

Y abre **http://localhost:8000**. Abriendo `docs/index.html` con doble clic se ven todos los
capítulos y gráficas, pero el panel de ventas no carga sus datos (usa `fetch`).

## Capítulos

| Capítulo | Archivo | Pestaña de la app |
|---|---|---|
| Introducción | `index.Rmd` | Introducción |
| Contexto | `01-contexto.Rmd` | Contexto |
| Planteamiento del problema | `02-problema.Rmd` | Problema |
| Objetivos | `03-objetivos.Rmd` | Objetivos |
| Marco teórico | `04-marco-teorico.Rmd` | Marco teórico |
| Metodología (ETL) | `05-metodologia.Rmd` | Metodología |
| EDA | `06-eda.Rmd` | EDA |
| Dashboard (panel de ventas y app Shiny) | `07-dashboard.Rmd` | — |
| Limitaciones | `08-limitaciones.Rmd` | Limitaciones |
| Conclusiones | `09-conclusiones.Rmd` | Conclusiones |

El ETL (`05-metodologia.Rmd`) se ejecuta al construir el libro: lee `Sample - Superstore.csv` y
escribe `superstore_transformado.csv`, que es el que usa el EDA.

## Gráficas

Las gráficas del EDA se hacen con el paquete **plotly** de R, con los mismos tipos, colores y estilo
que la app (`R/common.R` es el equivalente de `common.py`). La curva loess usa `stats::lowess()`, el
mismo algoritmo que `trendline="lowess"` de plotly en Python. El código de cada gráfica está oculto
detrás de "Mostrar código" (`code-folding.html`).

## Dashboard

`dashboard/` contiene el **panel de ventas**: una página estática con el proyecto completo (EDA,
modelos, proyección 2018 y conclusiones). `panel_ventas.html` lee `panel_data.json`, que tiene los
resultados del modelo del proyecto de machine learning
([proyecto_superstorep2](https://github.com/AlexT09/proyecto_superstorep2), app Dash). El panel vive
solo en los libros: GitHub Pages no puede ejecutar la app Dash, pero sí esta página.

## App Shiny (`shiny/`)

App con **shinydashboard** y la estructura del capítulo *Visualización con shiny* del curso:

```
shiny/
├── global.R                     # Librerías, carga de datos y preprocesamiento (tabla mensual)
├── ui.R                         # dashboardPage(): cabecera, menú lateral y pestañas
├── server.R                     # Lógica reactiva de cada pestaña
├── superstore_transformado.csv
└── www/
    └── logo.svg
```

- **Análisis Exploratorio:** `valueBox` con el resumen del dataset, gráfico interactivo
  (`ggplotly`) según la variable y el tipo de gráfico, filtro por año y tabla con `DT`.
- **Entrenamiento del Modelo:** regresión lineal o Random Forest con `caret::train()`, al
  presionar el botón (`observeEvent`). Los meses se dividen en orden (por defecto, 2015–2016 para
  entrenar y 2017 para probar) y se comparan contra la referencia "mismo mes del año anterior".
  El botón *Guardar Modelo* lo guarda como `.rds`.
- **Predicciones:** formulario con categoría, región, mes e historia de la serie, botones de
  ejemplo (temporada alta y baja) y predicción con `eventReactive`, mostrada en un `infoBox`.

Para ejecutarla, desde la raíz del repositorio:

```r
shiny::runApp("shiny")
```

## Estructura

```
├── index.Rmd … 09-conclusiones.Rmd   # Capítulos
├── _bookdown.yml          # Orden de los capítulos y carpeta de salida (docs/)
├── _output.yml            # Formato gitbook
├── R/common.R             # Paleta, style_fig() y lowess_line()
├── build.R                # Construye el libro en docs/
├── code-folding.html      # "Mostrar código" en cada gráfica
├── style.css
├── dashboard/             # Panel de ventas (se copia a docs/dashboard/)
├── shiny/                 # App Shiny (global.R, ui.R, server.R, www/)
├── Sample - Superstore.csv
├── superstore_transformado.csv
└── docs/                  # Libro generado (GitHub Pages)
```

---

Proyecto: Alex Teran y David Estrada
