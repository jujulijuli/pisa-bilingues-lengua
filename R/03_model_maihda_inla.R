# =============================================================================
# 03_model_maihda_inla.R
# MAIHDA multivariante y multinivel (alumno < colegio < CCAA), por
# edicion, con la congruencia linguistica como 5a dimension del estrato
# interseccional, mas la interaccion congruencia x especializacion del
# centro (pregunta 2 del proyecto) y publico/privado (pregunta 3).
#
# Mismo patron probado que ../pisa-espana-ccaa/R/03_model_maihda_ccaa_inla.R
# (mismas opciones de INLA, mismas funciones de extraccion VPC/PCV,
# reutilizadas literalmente) -- ver ese script para el detalle largo de
# cada decision (estandarizacion score_z, int.strategy="eb"+cmin=0,
# dic/waic/config=FALSE, control.predictor compute=FALSE, por que
# school_id_num se calcula DESPUES del filtro de NA, etc.). Aqui solo se
# documentan las diferencias especificas de este proyecto.
#
# NOTA: las 3 ediciones (2018, 2022, 2025) tienen CCAA real (ver
# 01_load_data.R -- 2018 la obtiene de STRATUM, no de REGION). El
# codigo de todas formas comprueba  por edicion antes de
# anadir f(ccaa_id, model="iid") a la formula, por si en el futuro se
# anade alguna edicion sin esa identificacion (evita el error "only NA
# values" que ya se vio con strata_id en el proyecto hermano).
# =============================================================================

library(dplyr)
library(INLA)

inla.setOption(num.threads = "4:1")

student_long <- readRDS("data/student_bilingue_long.rds")

domain_labels <- levels(student_long$domain)
stopifnot(length(domain_labels) == 3)

n_strata_global <- n_distinct(student_long$strata)

score_mean <- mean(student_long$score, na.rm = TRUE)
score_sd <- sd(student_long$score, na.rm = TRUE)
saveRDS(list(mean = score_mean, sd = score_sd), "data/score_scale.rds")

fit_maihda_year <- function(yr) {

  dat <- student_long |>
    filter(year == yr, !is.na(score)) |>
    mutate(
      domain_id = as.integer(domain),
      strata_id = as.integer(strata)
    )

  n_before <- nrow(dat)
  dat <- dat |>
    filter(
      !is.na(strata_id), !is.na(global_school_id),
      !is.na(stu_wgt), stu_wgt > 0
    )
  n_after <- nrow(dat)
  if (n_after < n_before) {
    message(
      "Ano ", yr, ": ", n_before - n_after, " de ", n_before,
      " filas descartadas por strata/colegio/peso muestral ausente o no positivo (",
      scales::percent((n_before - n_after) / n_before, accuracy = 0.1), ")."
    )
  }
  if (n_after == 0 || n_distinct(dat$strata_id) < 2) {
    message(
      "Ano ", yr, ": SALTADO -- datos insuficientes tras eliminar NA (n=", n_after,
      ", estratos distintos=", n_distinct(dat$strata_id), ")."
    )
    return(NULL)
  }

  # CCAA real disponible solo si hay >= 2 valores no NA (2022/2025; 2018
  # queda excluido de este termino, ver aviso al principio del script).
  tiene_ccaa <- n_distinct(dat$ccaa[!is.na(dat$ccaa)]) >= 2
  if (tiene_ccaa) {
    dat <- dat |> mutate(ccaa_id = as.integer(factor(ccaa)))
  }

  dat <- dat |> mutate(school_id_num = as.integer(factor(global_school_id)))
  n_school_year <- n_distinct(dat$school_id_num)

  message(
    "Ano ", yr, ": ajustando con ", n_after, " filas, ", n_strata_global,
    " estratos, ", n_school_year, " colegios", if (tiene_ccaa) ", con CCAA real" else " -- SIN CCAA real para esta edicion (termino ccaa_id omitido)", "..."
  )

  dat <- dat |>
    mutate(
      strata_domain_id = strata_id + (domain_id - 1L) * n_strata_global,
      score_z = (score - score_mean) / score_sd
    )

  # PC prior en la precision de colegio y CCAA. Con el prior por
  # defecto (Gamma vago), cuando la varianza entre CCAA es casi cero
  # (como ocurre en 2022, donde el VPC de CCAA sale en torno al 0.4%),
  # la precision estimada se dispara a valores numericamente extremos
  # (media ~10000, sd ~900000, con la mediana en 0.02) y la
  # transformacion posterior a varianza (1/precision, usada para el
  # VPC) falla con "zero non-NA points" en splinefun(). El PC prior
  # (Simpson et al. 2017, recomendacion estandar de INLA para
  # componentes de varianza que pueden estar cerca de 0) se comporta
  # bien justo en ese limite -- aqui P(sd > 1) = 0.01 en la escala de
  # score_z (SD=1 en toda la poblacion), un prior debil que apenas
  # afecta a componentes bien identificadas pero evita el
  # comportamiento degenerado cuando la varianza real es casi nula.
  pc_prior_iid <- "hyper = list(prec = list(prior = \"pc.prec\", param = c(1, 0.01)))"
  re_terms <- c(
    sprintf("f(strata_domain_id, model = \"iid3d\", n = %d)", 3L * n_strata_global),
    sprintf("f(school_id_num, model = \"iid\", %s)", pc_prior_iid)
  )
  if (tiene_ccaa) re_terms <- c(re_terms, sprintf("f(ccaa_id, model = \"iid\", %s)", pc_prior_iid))

  formula_null <- reformulate(c("0", "domain", re_terms), response = "score_z")

  fit_null <- inla(
    formula_null, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )

  # Covariables del modelo principal. `congruencia_linguistica` y
  # `centro_especializacion` entran como efectos principales ADEMAS de
  # via el estrato (congruencia) -- misma razon que `immig` en el
  # proyecto hermano: sin el efecto principal, el PCV atribuiria de
  # forma espuria a interaccion interseccional varianza que en realidad
  # explica la congruencia por si sola. `centro_especializacion` no
  # esta en el estrato (es de colegio, no de alumno) asi que solo hace
  # falta como efecto principal + interaccion.
  #
  # PREGUNTA 2 del proyecto (interaccion individuo-colegio): termino
  # "congruencia_linguistica:centro_especializacion" -- si su
  # coeficiente es distinto de 0, la penalizacion/beneficio de la
  # (in)congruencia linguistica individual DEPENDE de en que tipo de
  # colegio esta el alumno.
  # PREGUNTA 3 (efecto colegio publico/privado): `public_private`.
  covars <- c("gender", "parent_educ", "escs_q")
  if ("immig" %in% names(dat)) covars <- c(covars, "immig")
  if ("congruencia_linguistica" %in% names(dat) && n_distinct(dat$congruencia_linguistica, na.rm = TRUE) >= 2) {
    covars <- c(covars, "congruencia_linguistica")
  }
  tiene_interaccion <- FALSE
  if ("centro_especializacion" %in% names(dat) && n_distinct(dat$centro_especializacion, na.rm = TRUE) >= 2) {
    covars <- c(covars, "centro_especializacion")
    if ("congruencia_linguistica" %in% covars) {
      covars <- c(covars, "congruencia_linguistica:centro_especializacion")
      tiene_interaccion <- TRUE
    }
  }
  if ("public_private" %in% names(dat) && n_distinct(dat$public_private, na.rm = TRUE) >= 2) {
    covars <- c(covars, "public_private")
  }

  formula_main <- reformulate(c("0", "domain", covars, re_terms), response = "score_z")

  # Combinaciones lineales: efecto TOTAL de la incongruencia linguistica
  # DENTRO de cada tipo de colegio, no solo el extra de la interaccion
  # respecto al colegio monolingue castellano (que es lo unico que da
  # el termino de interaccion por si solo -- ver interaccion_lengua_efecto
  # mas abajo). Sumar a mano el coeficiente principal + el de interaccion
  # da el punto estimado correcto, pero NO el intervalo de confianza
  # correcto (ignora la covarianza entre ambos coeficientes) --
  # inla.make.lincomb() calcula la
  # combinacion directamente de la posterior conjunta de los efectos
  # fijos implicados, con IC correcto, sin necesidad de volver a muestrear
  # todo el modelo (mucho mas barato que activar config=TRUE +
  # inla.posterior.sample(), que aqui se evita deliberadamente por
  # rendimiento -- ver aviso al principio del script).
  lincombs <- NULL
  if (tiene_interaccion) {
    niveles_centro <- levels(droplevels(dat$centro_especializacion))
    lc_list <- lapply(niveles_centro, function(niv) {
      args <- list("congruencia_linguisticaincongruente" = 1)
      if (niv != niveles_centro[1]) {
        term_interaccion <- paste0("congruencia_linguisticaincongruente:centro_especializacion", niv)
        args[[term_interaccion]] <- 1
      }
      lc <- do.call(inla.make.lincomb, args)
      names(lc) <- paste0("total_incongruente_", niv)
      lc
    })
    lincombs <- do.call(c, lc_list)
  }

  fit_main <- inla(
    formula_main, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    lincomb = lincombs,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )

  list(
    year = yr, null = fit_null, main = fit_main,
    n_strata = n_strata_global, n_school = n_school_year,
    tiene_ccaa = tiene_ccaa, tiene_interaccion = tiene_interaccion
  )
}

# --- Funciones de extraccion (identicas a las de pisa-espana-ccaa) ----------
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

  if (length(var_domain_samples) != length(domain_labels)) {
    message(
      "AVISO VPC (", effect_name, "): se esperaban ", length(domain_labels),
      " precisiones de dominio y se encontraron ", length(var_domain_samples), ". ",
      "Hiperparametros vistos para este efecto: ",
      paste(attr(var_domain_samples, "hp_names_found"), collapse = ", ")
    )
  }

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

extract_vpc_simple <- function(fit, effect_name) {
  var_effect <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar[[paste0("Precision for ", effect_name)]])
  var_resid  <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar$`Precision for the Gaussian observations`)
  samp_effect <- inla.rmarginal(4000, var_effect)
  samp_resid  <- inla.rmarginal(4000, var_resid)
  vpc_samples <- samp_effect / (samp_effect + samp_resid)
  tibble::tibble(
    vpc_mean = mean(vpc_samples),
    vpc_lower = quantile(vpc_samples, 0.025),
    vpc_upper = quantile(vpc_samples, 0.975)
  )
}

extract_pcv_by_domain <- function(fit_null, fit_main, effect_name, domain_labels) {
  var_null <- extract_domain_variance_samples(fit_null, effect_name, domain_labels)
  var_main <- extract_domain_variance_samples(fit_main, effect_name, domain_labels)
  common <- intersect(names(var_null), names(var_main))

  purrr::map_dfr(common, function(lab) {
    pcv_samples <- (var_null[[lab]] - var_main[[lab]]) / var_null[[lab]]
    tibble::tibble(
      domain = lab,
      pcv_mean = mean(pcv_samples),
      pcv_lower = quantile(pcv_samples, 0.025),
      pcv_upper = quantile(pcv_samples, 0.975)
    )
  })
}

extract_domain_correlations <- function(fit, effect_name, domain_labels) {
  hp_names <- rownames(fit$summary.hyperpar)
  effect_hp <- grep(effect_name, hp_names, fixed = TRUE, value = TRUE)
  cor_names <- grep("ho", effect_hp, value = TRUE, ignore.case = TRUE)

  if (length(cor_names) == 0) {
    message(
      "AVISO: no se encontraron hiperparametros de correlacion para '", effect_name,
      "'. Hiperparametros vistos para este efecto: ", paste(effect_hp, collapse = ", ")
    )
    return(tibble::tibble())
  }

  purrr::map_dfr(cor_names, function(nm) {
    digits <- as.integer(regmatches(nm, gregexpr("[1-3]", nm))[[1]])
    lab <- if (length(digits) >= 2) {
      paste0(domain_labels[digits[1]], "-", domain_labels[digits[2]])
    } else {
      nm
    }
    row <- fit$summary.hyperpar[nm, ]
    tibble::tibble(
      par = lab,
      cor_mean = row[["mean"]],
      cor_lower = row[["0.025quant"]],
      cor_upper = row[["0.975quant"]]
    )
  })
}

# Efectos fijos en puntos PISA (coeficiente x score_sd), para las 3
# piezas del proyecto -- mismo patron que pubpriv_efecto en
# ../pisa-espana-ccaa/R/09_modelo_covariables_colegio.R.
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

# Efecto TOTAL (combinacion lineal, ver lincombs mas arriba) de la
# incongruencia linguistica dentro de cada tipo de colegio, con IC
# correcto (no una suma a mano de dos coeficientes independientes).
extract_lincomb_efecto_puntos <- function(fit, yr) {
  lc <- fit$summary.lincomb.derived
  if (is.null(lc) || nrow(lc) == 0) return(tibble::tibble())
  lc |>
    tibble::as_tibble(rownames = "term") |>
    transmute(
      year = yr, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * score_sd,
      ci_low_puntos = `0.025quant` * score_sd,
      ci_high_puntos = `0.975quant` * score_sd
    )
}

years <- sort(unique(student_long$year))
maihda_fits <- lapply(years, fit_maihda_year)
names(maihda_fits) <- years

skipped_years <- names(maihda_fits)[sapply(maihda_fits, is.null)]
if (length(skipped_years) > 0) {
  message(
    "AVISO: ", length(skipped_years), " edicion(es) saltada(s) por datos insuficientes: ",
    paste(skipped_years, collapse = ", ")
  )
}
maihda_fits <- maihda_fits[!sapply(maihda_fits, is.null)]

vpc_strata_by_year <- purrr::map_dfr(maihda_fits, ~ extract_vpc_by_domain(.x$null, "strata_domain_id", domain_labels), .id = "year")
vpc_school_by_year <- purrr::map_dfr(maihda_fits, ~ extract_vpc_simple(.x$null, "school_id_num"), .id = "year")
vpc_ccaa_by_year    <- purrr::map_dfr(maihda_fits[sapply(maihda_fits, `[[`, "tiene_ccaa")], ~ extract_vpc_simple(.x$null, "ccaa_id"), .id = "year")
pcv_strata_by_year  <- purrr::map_dfr(maihda_fits, ~ extract_pcv_by_domain(.x$null, .x$main, "strata_domain_id", domain_labels), .id = "year")
cor_strata_by_year  <- purrr::map_dfr(maihda_fits, ~ extract_domain_correlations(.x$null, "strata_domain_id", domain_labels), .id = "year")

# Efectos fijos de las 3 piezas del proyecto, en puntos PISA.
congruencia_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "^congruencia_linguistica[^:]*$", as.integer(yr))
})
centro_especializacion_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "^centro_especializacion[^:]*$", as.integer(yr))
})
interaccion_lengua_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  if (!isTRUE(maihda_fits[[yr]]$tiene_interaccion)) return(tibble::tibble())
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "congruencia_linguistica.*:.*centro_especializacion|centro_especializacion.*:.*congruencia_linguistica", as.integer(yr))
})
pubpriv_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "^public_private", as.integer(yr))
})
# Efecto de origen migrante: immig entra como covariable principal desde
# el inicio (ver `covars` en fit_maihda_year()), asi que este coeficiente
# ya esta en cada ajuste -- ver R/03c_extraer_efecto_immig.R para
# obtenerlo sin reajustar, a partir de data/maihda_fits_by_year.rds ya
# guardado.
immig_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  extract_fixed_efecto_puntos(maihda_fits[[yr]]$main, "^immig", as.integer(yr))
})
interaccion_total_efecto <- purrr::map_dfr(names(maihda_fits), function(yr) {
  if (!isTRUE(maihda_fits[[yr]]$tiene_interaccion)) return(tibble::tibble())
  extract_lincomb_efecto_puntos(maihda_fits[[yr]]$main, as.integer(yr))
})

saveRDS(maihda_fits, "data/maihda_fits_by_year.rds")
saveRDS(vpc_strata_by_year, "data/vpc_strata_by_year.rds")
saveRDS(vpc_school_by_year, "data/vpc_school_by_year.rds")
saveRDS(vpc_ccaa_by_year, "data/vpc_ccaa_by_year.rds")
saveRDS(pcv_strata_by_year, "data/pcv_strata_by_year.rds")
saveRDS(cor_strata_by_year, "data/cor_strata_by_year.rds")
saveRDS(congruencia_efecto, "data/tbl_congruencia_efecto.rds")
saveRDS(centro_especializacion_efecto, "data/tbl_centro_especializacion_efecto.rds")
saveRDS(interaccion_lengua_efecto, "data/tbl_interaccion_lengua_efecto.rds")
saveRDS(pubpriv_efecto, "data/tbl_pubpriv_efecto.rds")
saveRDS(immig_efecto, "data/tbl_immig_efecto.rds")
saveRDS(interaccion_total_efecto, "data/tbl_interaccion_total_efecto.rds")

message("\n=== Interaccion congruencia x especializacion del centro (puntos PISA) ===")
print(interaccion_lengua_efecto)
message("\n=== Efecto TOTAL de la incongruencia DENTRO de cada tipo de colegio (puntos PISA, IC via combinacion lineal INLA) ===")
print(interaccion_total_efecto, n = Inf, width = Inf)
message("\n=== Efecto publico/privado (puntos PISA) ===")
print(pubpriv_efecto)
message("\n=== Efecto de origen migrante (puntos PISA, ajustado) ===")
print(immig_efecto, n = Inf, width = Inf)
