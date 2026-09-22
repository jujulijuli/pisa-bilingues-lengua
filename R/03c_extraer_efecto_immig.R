# =============================================================================
# 03c_extraer_efecto_immig.R
#
# OBJETIVO: extraer el efecto principal de origen migrante (immig) de los
# modelos YA AJUSTADOS por R/03_model_maihda_inla.R. `immig` entra en el
# modelo principal como covariable desde el principio (ver `covars` en
# fit_maihda_year()), asi que este coeficiente ya esta ahi dentro de cada
# ajuste guardado -- no hace falta reajustar nada, solo releer
# data/maihda_fits_by_year.rds y extraer.
#
# Este script queda tambien incorporado en R/03_model_maihda_inla.R para
# que una ejecucion completa futura lo genere automaticamente; este
# archivo es solo el atajo para no tener que reajustar los 3 modelos
# INLA (que ya estan calculados) solo para sacar esta tabla.
# =============================================================================

library(dplyr)
library(INLA)

maihda_fits <- readRDS("data/maihda_fits_by_year.rds")
score_scale <- readRDS("data/score_scale.rds")
score_mean <- score_scale$mean
score_sd <- score_scale$sd

extract_fixed_efecto_puntos <- function(fit, pattern, yr) {
  fit$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(grepl(pattern, term)) |>
    transmute(
      year = yr, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * score_sd,
      ci_low_puntos = `0.025quant` * score_sd,
      ci_high_puntos = `0.975quant` * score_sd
    )
}

immig_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "^immig", as.integer(yr))
})

saveRDS(immig_efecto, "data/tbl_immig_efecto.rds")

message("\n=== Efecto de origen migrante (puntos PISA, ajustado por el resto del modelo) ===")
print(immig_efecto, n = Inf, width = Inf)
