# R/common.R
# Utilidades compartidas: paleta de colores y estilo común de las figuras de plotly.
# Es el equivalente en R del common.py de la app Dash (mismos colores y mismo estilo).

library(tidyverse)
library(plotly)

# ---------------------------------------------------------------- colores
# Un único color de marca (azul petróleo) en distintas tintas.
BRAND <- "#2E6F95"
BRAND_DARK <- "#1F5570"
BRAND_DARKER <- "#153B4E"
BRAND_LIGHT <- "#EAF2F7"

PASTEL <- c(
  blue = "#DCEBF2",
  green = "#B7D4E3",
  peach = "#8FBBD1",
  pink = "#63A0BC",
  yellow = "#3B85A8",
  lavender = "#1F5570"
)
COLORWAY <- unname(PASTEL)
TEXT <- "#2f3247"
GRID <- "#EEF0F6"
# Tintas más saturadas, para líneas/puntos que necesitan más contraste sobre blanco
ACCENT <- c(
  blue = "#3B85A8",
  pink = "#63A0BC",
  green = "#3B85A8",
  peach = "#1F5570",
  lavender = "#1F5570",
  yellow = "#8FBBD1"
)
MARK <- BRAND_DARKER # color de puntos de referencia (p. ej. la media en un boxplot)
# Tres tintas del mismo azul para distinguir categorías sin salir del color de marca
CATEGORY_COLORS <- c(
  "Furniture" = "#8FBBD1",
  "Office Supplies" = "#153B4E",
  "Technology" = "#3B85A8"
)
CATEGORY_ACCENT <- c(
  "Furniture" = "#63A0BC",
  "Office Supplies" = "#153B4E",
  "Technology" = "#1F5570"
)
MONO_SCALE <- list(c(0, "#FFFFFF"), c(0.5, PASTEL[["yellow"]]), c(1, BRAND_DARKER))

# Código de 2 letras de cada estado, requerido por los mapas de plotly (locationmode = "USA-states")
STATE_ABBR <- c(
  "Alabama" = "AL", "Arizona" = "AZ", "Arkansas" = "AR", "California" = "CA", "Colorado" = "CO",
  "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", "Florida" = "FL", "Georgia" = "GA",
  "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS", "Kentucky" = "KY",
  "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI",
  "Minnesota" = "MN", "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE",
  "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR",
  "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
  "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"
)

# Marcador blanco alrededor de los puntos (mismo borde que en la app)
WHITE_LINE <- list(width = 1, color = "white")

style_fig <- function(p, height = 360, margin = list(l = 40, r = 20, t = 20, b = 40)) {
  # Aplica el estilo común a cualquier figura de plotly (equivalente a style_fig() de la app):
  # se aplica al final, así que el estilo de los ejes vale para todos (también los de las facetas).
  p$height <- height
  p <- p %>%
    layout(
      font = list(family = "Nunito, Segoe UI, sans-serif", color = TEXT),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      colorway = COLORWAY,
      margin = margin,
      legend = list(orientation = "h", y = -0.2),
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE) %>%
    plotly_build()
  ejes <- grep("^[xy]axis[0-9]*$", names(p$x$layout), value = TRUE)
  for (eje in union(ejes, c("xaxis", "yaxis"))) {
    p$x$layout[[eje]] <- modifyList(p$x$layout[[eje]] %||% list(),
                                    list(gridcolor = GRID, zeroline = FALSE, ticks = "", automargin = TRUE))
  }
  p
}

lowess_line <- function(x, y) {
  # Curva loess: mismo algoritmo que trendline = "lowess" de plotly en Python
  # (frac = 2/3, 3 iteraciones robustas, sin interpolación: delta = 0).
  fit <- stats::lowess(as.numeric(x), y, f = 2 / 3, iter = 3, delta = 0)
  tibble(x = as.Date(fit$x, origin = "1970-01-01"), y = fit$y)
}
