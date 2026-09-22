# =============================================================================
# 04_colegios_interaccion_congruencia.R
#
# OBJETIVO: mas alla de la clasificacion categorica
# centro_especializacion (monolingue_espanol/monolingue_regional/mixto),
# que solo da un efecto PROMEDIO por tipo de colegio -- que COLEGIOS
# CONCRETOS tienen resultados significativamente mejores de lo esperado
# para el alumnado incongruente, aun dentro de su mismo tipo de centro?
# Es decir: "interaccion entre niveles" alumno x colegio, a nivel de
# colegio INDIVIDUAL, no solo de categoria de colegio.
#
# DISENO: se anade una PENDIENTE ALEATORIA por colegio para la
# incongruencia linguistica (ademas del intercepto aleatorio de colegio
# que ya tenia 03_model_maihda_inla.R). Con esto, el efecto total de la
# incongruencia en el colegio i es:
#
#   efecto_fijo_incongruencia (+ efecto_fijo_interaccion_tipo_centro) +
#   pendiente_aleatoria_colegio_i
#
# La pendiente_aleatoria_colegio_i es justo lo que responde a la
# pregunta: colegios con pendiente POSITIVA tienen un efecto de la
# incongruencia MENOS negativo (o mas positivo) que lo que predice el
# tipo de centro al que pertenecen -- son las "buenas practicas" que se
# buscan. Se mantienen TODOS los efectos fijos ya establecidos en
# 03_model_maihda_inla.R (genero, educ. parental, ESCS, immig,
# congruencia, centro_especializacion, su interaccion, publico/privado)
# -- la pendiente aleatoria captura lo que queda SIN explicar por esos
# efectos poblacionales, no lo sustituye.
#
# LIMITACION IMPORTANTE, documentada para no sobre-interpretar: colegio
# se identifica con `global_school_id` = CCAA + edicion + CNTSCHID -- NO
# hay garantia de que sea el MISMO centro fisico de una edicion a otra
# (mismo criterio ya usado en el resto del proyecto para
# centro_especializacion). Este ranking es, por tanto, "colegio-edicion",
# no una lista de centros estables en el tiempo -- para identificar
# "experiencias" consistentes de verdad, conviene mirar si el mismo
# colegio aparece bien puntuado en mas de una edicion (requeriria
# verificar si CNTSCHID es comparable entre ediciones en los ficheros
# originales, que no esta comprobado).
#
# COSTE: mismo tamano de modelo que fit_main de 03 + una f() adicional
# (iid, barata) -- deberia tardar un orden de magnitud similar a 03
# (minutos, no horas), pero ejecutalo aparte y ven con el resultado.
# =============================================================================

library(dplyr)
library(INLA)

inla.setOption(num.threads = "4:1")

student_long <- readRDS("data/student_bilingue_long.rds")
domain_labels <- levels(student_long$domain)
stopifnot(length(domain_labels) == 3)

n_strata_global <- n_distinct(student_long$strata)

score_scale <- readRDS("data/score_scale.rds")
score_mean <- score_scale$mean
score_sd <- score_scale$sd

fit_colegio_slope_year <- function(yr) {
  dat <- student_long |>
    filter(year == yr, !is.na(score)) |>
    mutate(domain_id = as.integer(domain), strata_id = as.integer(strata)) |>
    filter(
      !is.na(strata_id), !is.na(global_school_id), !is.na(stu_wgt), stu_wgt > 0,
      !is.na(congruencia_linguistica), !is.na(centro_especializacion)
    )

  if (nrow(dat) == 0 || n_distinct(dat$strata_id) < 2) {
    message("Ano ", yr, ": SALTADO -- datos insuficientes.")
    return(NULL)
  }

  tiene_ccaa <- n_distinct(dat$ccaa[!is.na(dat$ccaa)]) >= 2
  if (tiene_ccaa) dat <- dat |> mutate(ccaa_id = as.integer(factor(ccaa)))
  dat <- dat |> mutate(school_id_num = as.integer(factor(global_school_id)))
  n_school_year <- n_distinct(dat$school_id_num)

  message(
    "Ano ", yr, ": ajustando pendiente colegio x incongruencia con ", nrow(dat),
    " filas, ", n_school_year, " colegios", if (tiene_ccaa) ", con CCAA real" else "", "..."
  )

  dat <- dat |>
    mutate(
      strata_domain_id = strata_id + (domain_id - 1L) * n_strata_global,
      score_z = (score - score_mean) / score_sd,
      incongruente_num = if_else(congruencia_linguistica == "incongruente", 1, 0),
      # INLA exige un nombre de indice DISTINTO por cada f() -- aunque
      # sea la misma agrupacion (colegio), el intercepto y la pendiente
      # necesitan su propia columna.
      school_id_slope = school_id_num
    )

  covars <- c("gender", "parent_educ", "escs_q")
  if ("immig" %in% names(dat)) covars <- c(covars, "immig")
  covars <- c(covars, "congruencia_linguistica", "centro_especializacion")
  if (n_distinct(dat$centro_especializacion, na.rm = TRUE) >= 2) {
    covars <- c(covars, "congruencia_linguistica:centro_especializacion")
  }
  if (n_distinct(dat$public_private, na.rm = TRUE) >= 2) covars <- c(covars, "public_private")

  re_terms <- c(
    sprintf("f(strata_domain_id, model = \"iid3d\", n = %d)", 3L * n_strata_global),
    "f(school_id_num, model = \"iid\")",
    "f(school_id_slope, incongruente_num, model = \"iid\")"
  )
  if (tiene_ccaa) re_terms <- c(re_terms, "f(ccaa_id, model = \"iid\")")

  formula_slope <- reformulate(c("0", "domain", covars, re_terms), response = "score_z")

  fit <- inla(
    formula_slope, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )

  list(fit = fit, dat = dat, n_school = n_school_year)
}

extract_ranking <- function(res, yr) {
  fit <- res$fit
  dat <- res$dat

  # OJO: dat esta en formato LARGO (una fila por alumno x dominio, 3 por
  # alumno) -- para contar ALUMNOS (no filas) hay que usar distinct()
  # por student_id antes de count().
  #
  # NOTA: se construyen tres left_join() independientes, todos con
  # clave unica school_id_num; global_school_id/ccaa/
  # centro_especializacion se incorporan via una tabla de consulta
  # aparte (colegio_info), evitando unir por una columna que todavia
  # no existe en el lado izquierdo del pipe en ese punto.
  n_incong <- dat |>
    filter(incongruente_num == 1) |>
    distinct(school_id_num, student_id) |>
    count(school_id_num, name = "n_incongruentes")
  n_total <- dat |>
    distinct(school_id_num, student_id) |>
    count(school_id_num, name = "n_alumnos_colegio")
  colegio_info <- dat |>
    distinct(school_id_num, global_school_id, ccaa, centro_especializacion)

  fit$summary.random$school_id_slope |>
    tibble::as_tibble() |>
    rename(school_id_num = ID) |>
    left_join(n_total, by = "school_id_num") |>
    left_join(n_incong, by = "school_id_num") |>
    left_join(colegio_info, by = "school_id_num") |>
    mutate(
      year = yr,
      n_incongruentes = tidyr::replace_na(n_incongruentes, 0L),
      slope_puntos = mean * score_sd,
      ci_low_puntos = `0.025quant` * score_sd,
      ci_high_puntos = `0.975quant` * score_sd,
      positivo_significativo = ci_low_puntos > 0
    ) |>
    select(
      year, school_id_num, global_school_id, ccaa, centro_especializacion,
      n_incongruentes, n_alumnos_colegio,
      slope_puntos, ci_low_puntos, ci_high_puntos, positivo_significativo
    ) |>
    arrange(desc(slope_puntos))
}

years <- sort(unique(student_long$year))
fits_slope <- lapply(years, fit_colegio_slope_year)
names(fits_slope) <- years
fits_slope <- fits_slope[!sapply(fits_slope, is.null)]

ranking_colegios <- purrr::map2_dfr(fits_slope, names(fits_slope), ~ extract_ranking(.x, as.integer(.y)))

saveRDS(ranking_colegios, "data/ranking_colegios_congruencia.rds")
saveRDS(fits_slope, "data/fits_colegio_slope_by_year.rds")

# Umbral minimo de alumnos incongruentes en el colegio para que la
# pendiente tenga algo de senal real detras -- por debajo de esto el
# shrinkage de INLA ya deja el valor cerca de 0 con IC ancho, pero
# conviene no destacarlos igualmente en el "top" impreso en consola.
MIN_N_INCONGRUENTES <- 5

message("\n=== TOP 15 colegios por pendiente colegio x incongruencia (puntos PISA), con >= ", MIN_N_INCONGRUENTES, " alumnos incongruentes ===")
ranking_colegios |>
  filter(n_incongruentes >= MIN_N_INCONGRUENTES) |>
  arrange(desc(slope_puntos)) |>
  head(15) |>
  print(n = Inf, width = Inf)

message("\n=== Colegios con pendiente SIGNIFICATIVAMENTE positiva (IC95 > 0), por ano, con >= ", MIN_N_INCONGRUENTES, " alumnos incongruentes ===")
ranking_colegios |>
  filter(n_incongruentes >= MIN_N_INCONGRUENTES, positivo_significativo) |>
  count(year) |>
  print(n = Inf)
