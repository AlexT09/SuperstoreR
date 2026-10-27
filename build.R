# build.R — Construye el libro en docs/ (lo que publica GitHub Pages).
# Ejecutar desde la raíz del proyecto:  Rscript build.R
# Requiere pandoc (viene con RStudio). Fuera de RStudio, si no se encuentra, se usa el de RStudio.
if (!rmarkdown::pandoc_available()) {
  rstudio <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (dir.exists(rstudio)) Sys.setenv(RSTUDIO_PANDOC = rstudio)
}

unlink("docs", recursive = TRUE) # sin archivos viejos de versiones anteriores
bookdown::render_book("index.Rmd", quiet = TRUE)
file.copy("app/www/favicon.ico", "docs", overwrite = TRUE)
file.create("docs/.nojekyll")
message("OK: libro generado en docs/")
