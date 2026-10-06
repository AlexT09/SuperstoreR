# Proyecto Sample Superstore — bookdown (R)

Análisis exploratorio de datos (EDA) del dataset **Sample Superstore** para responder la
pregunta de negocio: **¿qué factores (categoría, región, segmento) explican el nivel de ventas?**

Libro publicado: **https://alext09.github.io/rbook/**

## Requisitos

- **R** 4.1 o superior, con los paquetes `bookdown`, `tidyverse` y `plotly`
- **pandoc** (viene incluido con RStudio)

```r
install.packages(c("bookdown", "tidyverse", "plotly"))
```

## Cómo construir el libro

Desde RStudio (abriendo `Rbook.Rproj`) o desde una terminal:

```powershell
Rscript build.R
```

El libro queda en `docs/`, que es la carpeta que publica GitHub Pages (rama `main`, carpeta `/docs`).
`build.R` construye el libro con `bookdown::render_book()` y copia el dashboard a `docs/dashboard/`.

Para verlo con el dashboard 3D funcionando, sírvelo por HTTP:

```powershell
python -m http.server 8000 --directory docs
```

Y abre **http://localhost:8000**. Abriendo `docs/index.html` con doble clic se ven todos los
capítulos y gráficas, pero el dashboard 3D no carga sus datos (usa `fetch`).

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
| Dashboard | `07-dashboard.Rmd` | Dashboard |
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

`dashboard/` es una copia de `3DWebDashboard/` del proyecto Dash (incluye `dashboard_core.js` y
`dashboard_data.json`), y `dashboard/dashboard_2d.html` es el `assets/dashboard.html` que genera
`dashboard/build_dashboard.py`. Si cambian los datos, regenéralos en el proyecto Dash y copia esos
archivos aquí.

## Estructura

```
├── index.Rmd … 09-conclusiones.Rmd   # Capítulos
├── _bookdown.yml          # Orden de los capítulos y carpeta de salida (docs/)
├── _output.yml            # Formato gitbook
├── R/common.R             # Paleta, style_fig() y lowess_line()
├── build.R                # Construye el libro en docs/
├── code-folding.html      # "Mostrar código" en cada gráfica
├── style.css
├── dashboard/             # Dashboard 3D (se copia a docs/dashboard/)
├── Sample - Superstore.csv
├── superstore_transformado.csv
└── docs/                  # Libro generado (GitHub Pages)
```

---

Proyecto: Alex Teran y David Estrada
