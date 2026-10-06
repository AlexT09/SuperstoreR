# build.R — Construye el libro en docs/ (lo que publica GitHub Pages).
# Ejecutar:  Rscript build.R
bookdown::render_book("index.Rmd", quiet = TRUE)

# El dashboard 3D y su favicon no los referencia ningún .Rmd: se copian aparte
file.copy("dashboard", "docs", recursive = TRUE, overwrite = TRUE)
file.copy("favicon.ico", "docs", overwrite = TRUE)
file.create("docs/.nojekyll")
message("OK: libro generado en docs/")
