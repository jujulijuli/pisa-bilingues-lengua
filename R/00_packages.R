# =============================================================================
# 00_packages.R -- dependencias del proyecto PISA territorios bilingües
# =============================================================================

cran_pkgs <- c(
  "haven",      # lee SPSS (.sav) / Stata (.dta) -- formato de los ficheros INEE
  "readxl",      # por si el INEE solo da Excel para algún año
  "dplyr", "tidyr", "purrr", "forcats", "stringr", "readr",
  "ggplot2", "ggdist", "patchwork", "ggrepel",
  "lme4", "brms",
  "MAIHDA",
  "WeMix",       # mixPV() -- reglas de Rubin para valores plausibles múltiples
  "intsvy",      # paquete específico para PISA/TIMSS/PIRLS con diseño muestral complejo
                 # (pesos replicados BRR-Fay) -- útil para VALIDAR nuestras estimaciones
                 # de INLA frente al estándar del campo. Ver @caro2017intsvy
  "survey",
  "gt", "gtsummary",
  "scales",      # formateo (p.ej. cobertura de public_private como %)
  "here", "renv"
)

new_cran <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
if (length(new_cran)) install.packages(new_cran, repos = "https://cloud.r-project.org")

# INLA no está en CRAN (ya lo tienes instalado si vienes de pisa-maihda-trends)
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
    dep = TRUE
  )
}

invisible(lapply(c(cran_pkgs, "INLA"), library, character.only = TRUE))
