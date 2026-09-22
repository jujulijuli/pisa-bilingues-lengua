# =============================================================================
# 06_sensibilidad_umbral_centro.R
#
# OBJETIVO: documentar la construccion de centro_especializacion (idioma
# dominante de examen por colegio-edicion) con un descriptivo de la
# distribucion de pct_dominante_colegio, y comprobar la sensibilidad de
# la clasificacion "mixto" (< 90% de dominancia) frente a umbrales
# alternativos. Reproduce EXACTAMENTE la logica de centro_lang en
# R/02_prepare_variables.R (mismo agrupamiento por global_school_id,
# mismo lang_test sin colapsar), variando solo el umbral.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

student <- readRDS("data/student_bilingue_pooled.rds")

# --- recalculo del idioma dominante por colegio-edicion (identico a
#     02_prepare_variables.R, pero uniendo year en la clave de grupo,
#     ya que global_school_id ya es unico por edicion en este proyecto) ---
centro_lang <- student |>
  filter(!is.na(lang_test)) |>
  count(year, global_school_id, lang_test, name = "n_lang") |>
  group_by(year, global_school_id) |>
  mutate(n_colegio_lang = sum(n_lang), pct = n_lang / n_colegio_lang) |>
  slice_max(order_by = pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(year, global_school_id, lang_dominante_colegio = lang_test,
            pct_dominante_colegio = pct, n_colegio_lang)

# tamano de muestra por colegio, para ponderar "pct alumnos" ademas de
# "pct colegios" en el sensibilidad
n_alumnos_colegio <- student |>
  filter(!is.na(lang_test)) |>
  count(year, global_school_id, name = "n_alumnos")

centro_lang <- centro_lang |> left_join(n_alumnos_colegio, by = c("year", "global_school_id"))

# =============================================================================
# 1. Descriptivo de pct_dominante_colegio, por edicion (para Metodos)
# =============================================================================
descriptivo_centro_umbral <- centro_lang |>
  group_by(year) |>
  summarise(
    n_colegios = n(),
    pct_100 = mean(pct_dominante_colegio >= 0.999) * 100,
    pct_menos_10 = mean(n_colegio_lang < 10) * 100,
    p10 = quantile(pct_dominante_colegio, 0.10) * 100,
    p25 = quantile(pct_dominante_colegio, 0.25) * 100,
    mediana = median(pct_dominante_colegio) * 100,
    p75 = quantile(pct_dominante_colegio, 0.75) * 100,
    p90 = quantile(pct_dominante_colegio, 0.90) * 100,
    minimo = min(pct_dominante_colegio) * 100,
    .groups = "drop"
  )
message("=== Descriptivo pct_dominante_colegio por edicion ===")
print(descriptivo_centro_umbral, width = Inf)
saveRDS(descriptivo_centro_umbral, "data/descriptivo_centro_umbral.rds")

# =============================================================================
# 2. Sensibilidad: clasificacion resultante a distintos umbrales
# =============================================================================
umbrales <- c(0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)

clasificar <- function(lang_dom, pct, umbral) {
  case_when(
    pct >= umbral & lang_dom == "Spanish" ~ "monolingue_espanol",
    pct >= umbral & lang_dom != "Spanish" ~ "monolingue_regional",
    TRUE ~ "mixto"
  )
}

sensibilidad_umbral_centro <- map_dfr(umbrales, function(u) {
  centro_lang |>
    mutate(centro_especializacion_u = clasificar(lang_dominante_colegio, pct_dominante_colegio, u)) |>
    group_by(year, centro_especializacion_u) |>
    summarise(n_colegios = n(), n_alumnos = sum(n_alumnos), .groups = "drop") |>
    group_by(year) |>
    mutate(pct_colegios = n_colegios / sum(n_colegios) * 100,
           pct_alumnos = n_alumnos / sum(n_alumnos) * 100) |>
    ungroup() |>
    mutate(umbral = u * 100)
})
message("\n=== Sensibilidad de la clasificacion por umbral (% alumnos) ===")
print(sensibilidad_umbral_centro |> filter(centro_especializacion_u == "mixto") |>
        select(umbral, year, pct_colegios, pct_alumnos), n = Inf, width = Inf)
saveRDS(sensibilidad_umbral_centro, "data/sensibilidad_umbral_centro.rds")

# =============================================================================
# 3. "Flip": colegios que cambian de categoria entre el umbral base (90%)
#    y umbrales alternativos (80% y 95%), y que peso de alumnado tienen
# =============================================================================
base90 <- centro_lang |>
  mutate(cat_90 = clasificar(lang_dominante_colegio, pct_dominante_colegio, 0.90))

flip_umbral_centro <- map_dfr(c(0.80, 0.95), function(u) {
  base90 |>
    mutate(cat_alt = clasificar(lang_dominante_colegio, pct_dominante_colegio, u),
           cambia = cat_90 != cat_alt) |>
    group_by(year) |>
    summarise(
      umbral_alt = u * 100,
      n_colegios_cambia = sum(cambia),
      n_colegios_total = n(),
      pct_colegios_cambia = mean(cambia) * 100,
      n_alumnos_cambia = sum(n_alumnos[cambia]),
      n_alumnos_total = sum(n_alumnos),
      pct_alumnos_cambia = sum(n_alumnos[cambia]) / sum(n_alumnos) * 100,
      .groups = "drop"
    )
})
message("\n=== Colegios que cambian de categoria vs. el umbral base (90%) ===")
print(flip_umbral_centro, n = Inf, width = Inf)
saveRDS(flip_umbral_centro, "data/flip_umbral_centro.rds")

# =============================================================================
# 4. Reordenamiento por CCAA: es el mismo patron en las 5 CCAA en 2025?
# =============================================================================
composicion_centro_ccaa <- readRDS("data/composicion_centro_ccaa.rds")

ccaa_shift <- composicion_centro_ccaa |>
  filter(year %in% c(2022, 2025)) |>
  pivot_wider(names_from = year, values_from = pct_alumnos, values_fill = 0,
              names_prefix = "y") |>
  mutate(delta_2022_2025 = y2025 - y2022) |>
  arrange(ccaa, centro_especializacion)

message("\n=== Cambio 2022 -> 2025 en la mezcla de tipo de centro, por CCAA ===")
print(ccaa_shift, n = Inf, width = Inf)
saveRDS(ccaa_shift, "data/ccaa_shift_2025.rds")

cat("\nOK: 4 tablas guardadas en data/.\n")
