# =============================================================================
# 03b_vpc_ablation_congruencia.R
#
# OBJETIVO: el contraste de VPC de estrato observado entre este proyecto
# (5 dimensiones, con congruencia linguistica, 5 CCAA bilingues) y
# pisa-espana-ccaa (4 dimensiones, sin congruencia, Espana completa) --
# 17-21% vs 8-10% en 2022/2025 -- ¿se sostiene DENTRO de estas mismas 5
# CCAA, o es en parte un efecto de comparar dos muestras distintas (5
# CCAA vs Espana entera) a la vez que dos definiciones de estrato
# distintas?
#
# Este script aisla esa pregunta: ajusta el modelo NULO (sin covariables,
# igual que fit_null en R/03_model_maihda_inla.R) pero con
# `strata_4dim` (genero x educ. parental x ESCS x immig -- LAS MISMAS 4
# dimensiones que pisa-espana-ccaa, SIN congruencia) en vez de `strata`
# (5 dimensiones, con congruencia), sobre EXACTAMENTE la misma muestra de
# 5 CCAA bilingues que el resto del proyecto. Compara el VPC de estrato
# resultante, ano a ano y dominio a dominio, contra el ya calculado con
# 5 dimensiones (data/vpc_strata_by_year.rds).
#
# Requiere haber corrido primero R/01_load_data.R -> R/02_prepare_variables.R
# (que ya genera `strata_4dim`) -- NO hace falta volver a correr
# R/03_model_maihda_inla.R antes de esto, pero conviene haberlo hecho ya
# al menos una vez para tener data/vpc_strata_by_year.rds con el que
# comparar al final.
#
# Nota de coste: esto ajusta 3 modelos NULOS adicionales (uno por
# edicion) -- no reajusta el modelo principal con covariables, asi que es
# bastante mas rapido que volver a correr 03 entero.
# =============================================================================

library(dplyr)
library(INLA)

inla.setOption(num.threads = "4:1")

student_long <- readRDS("data/student_bilingue_long.rds")
domain_labels <- levels(student_long$domain)
stopifnot(length(domain_labels) == 3)

# IMPORTANTE (mismo patron que n_strata_global en 03_model_maihda_inla.R):
# strata_id = as.integer(strata_4dim) usa la numeracion GLOBAL de niveles
# del factor strata_4dim (fijada UNA VEZ en 02_prepare_variables.R sobre
# el pool completo de 3 ediciones), no una renumeracion 1..n propia de
# cada ano. Por eso el numero de niveles tiene que calcularse tambien
# GLOBALMENTE, antes del filtro por ano -- contar solo los valores
# distintos que aparecen en un ano concreto (como hacia una primera
# version de este script) subestima el maximo indice real de strata_id
# para ese ano, y el offset domain_id*n_strata se calcula mal, sacando
# indices fuera de rango del termino iid3d ("Covariate does not match
# 'values' ... veces").
n_strata_4dim_global <- n_distinct(student_long$strata_4dim)

# Misma estandarizacion global que uso 03_model_maihda_inla.R -- se
# reutiliza la guardada por ese script para que el VPC sea directamente
# comparable (aunque, al ser una razon de varianzas, el VPC es invariante
# a la escala -- esto es solo por higiene numerica de INLA).
score_scale <- readRDS("data/score_scale.rds")
score_mean <- score_scale$mean
score_sd <- score_scale$sd

extract_domain_variance_samples <- function(fit, effect_name, domain_labels) {
  hp_names <- names(fit$marginals.hyperpar)
  effect_hp <- grep(effect_name, hp_names, fixed = TRUE, value = TRUE)
  prec_names <- grep("recision", effect_hp, value = TRUE)
  out <- list()
  for (nm in prec_names) {
    digits <- as.integer(regmatches(nm, gregexpr("[1-3]", nm))[[1]])
    dom_idx <- if (length(digits) >= 1) digits[1] else NA_integer_
    lab <- if (!is.na(dom_idx) && dom_idx <= length(domain_labels)) domain_labels[dom_idx] else nm
    var_marg <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar[[nm]])
    out[[lab]] <- inla.rmarginal(4000, var_marg)
  }
  attr(out, "hp_names_found") <- effect_hp
  out
}

extract_vpc_by_domain <- function(fit, effect_name, domain_labels) {
  var_domain_samples <- extract_domain_variance_samples(fit, effect_name, domain_labels)
  var_resid <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar$`Precision for the Gaussian observations`)
  samp_resid <- inla.rmarginal(4000, var_resid)
  purrr::imap_dfr(var_domain_samples, function(samp_domain, lab) {
    vpc_samples <- samp_domain / (samp_domain + samp_resid)
    tibble::tibble(
      domain = lab,
      vpc_mean = mean(vpc_samples),
      vpc_lower = quantile(vpc_samples, 0.025),
      vpc_upper = quantile(vpc_samples, 0.975)
    )
  })
}

fit_null_4dim_year <- function(yr) {
  dat <- student_long |>
    filter(year == yr, !is.na(score)) |>
    mutate(domain_id = as.integer(domain), strata_id = as.integer(strata_4dim)) |>
    filter(!is.na(strata_id), !is.na(global_school_id), !is.na(stu_wgt), stu_wgt > 0)

  if (nrow(dat) == 0 || n_distinct(dat$strata_id) < 2) {
    message("Ano ", yr, ": SALTADO -- datos insuficientes para strata_4dim.")
    return(NULL)
  }

  tiene_ccaa <- n_distinct(dat$ccaa[!is.na(dat$ccaa)]) >= 2
  if (tiene_ccaa) dat <- dat |> mutate(ccaa_id = as.integer(factor(ccaa)))
  dat <- dat |> mutate(school_id_num = as.integer(factor(global_school_id)))

  message(
    "Ano ", yr, " (strata_4dim, SIN congruencia): ", nrow(dat), " filas, ",
    n_distinct(dat$strata_id), " estratos DISTINTOS en este ano (de ",
    n_strata_4dim_global, " niveles globales posibles, max 2x3x4x3=72), ",
    n_distinct(dat$school_id_num), " colegios..."
  )

  dat <- dat |>
    mutate(
      strata_domain_id = strata_id + (domain_id - 1L) * n_strata_4dim_global,
      score_z = (score - score_mean) / score_sd
    )

  re_terms <- c(
    sprintf("f(strata_domain_id, model = \"iid3d\", n = %d)", 3L * n_strata_4dim_global),
    "f(school_id_num, model = \"iid\")"
  )
  if (tiene_ccaa) re_terms <- c(re_terms, "f(ccaa_id, model = \"iid\")")
  formula_null <- reformulate(c("0", "domain", re_terms), response = "score_z")

  inla(
    formula_null, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )
}

years <- sort(unique(student_long$year))
fits_4dim <- lapply(years, fit_null_4dim_year)
names(fits_4dim) <- years
fits_4dim <- fits_4dim[!sapply(fits_4dim, is.null)]

vpc_strata_4dim_by_year <- purrr::map_dfr(
  fits_4dim, ~ extract_vpc_by_domain(.x, "strata_domain_id", domain_labels), .id = "year"
)
saveRDS(vpc_strata_4dim_by_year, "data/vpc_strata_4dim_ablation_by_year.rds")

message("\n=== VPC de estrato, strata_4dim (SIN congruencia, MISMAS 5 CCAA) ===")
print(vpc_strata_4dim_by_year, n = Inf)

if (file.exists("data/vpc_strata_by_year.rds")) {
  vpc_5dim <- readRDS("data/vpc_strata_by_year.rds") |>
    rename(vpc_mean_5dim = vpc_mean, vpc_lower_5dim = vpc_lower, vpc_upper_5dim = vpc_upper)
  comparacion <- vpc_strata_4dim_by_year |>
    mutate(year = as.integer(year)) |>
    rename(vpc_mean_4dim = vpc_mean, vpc_lower_4dim = vpc_lower, vpc_upper_4dim = vpc_upper) |>
    inner_join(vpc_5dim |> mutate(year = as.integer(year)), by = c("year", "domain")) |>
    mutate(diferencia_pp = (vpc_mean_5dim - vpc_mean_4dim) * 100) |>
    select(year, domain, vpc_mean_4dim, vpc_mean_5dim, diferencia_pp)
  message("\n=== Comparacion DENTRO de las mismas 5 CCAA: 4 dim (sin congruencia) vs 5 dim (con congruencia) ===")
  print(comparacion, n = Inf)
  saveRDS(comparacion, "data/vpc_ablation_comparacion.rds")
} else {
  message("\nAVISO: no encuentro data/vpc_strata_by_year.rds -- corre antes R/03_model_maihda_inla.R para tener el VPC de 5 dimensiones con el que comparar.")
}
