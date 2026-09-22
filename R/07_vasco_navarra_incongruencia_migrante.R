# =============================================================================
# 07_vasco_navarra_incongruencia_migrante.R
#
# OBJETIVO: dentro de los centros MONOLINGUES (castellano y cooficial) de
# Pais Vasco (y Navarra, en cuanto se incorpore -- ver R/01_load_data.R),
# contrastar la "caida" de puntuacion asociada a la incongruencia
# linguistica segun el origen del alumnado incongruente: nativo (su lengua
# de casa es la OTRA lengua oficial del territorio) frente a migrante (su
# lengua de casa es una lengua de inmigracion, ni castellano ni cooficial).
# Usa congruencia_detalle (6 categorias, ya calculada en
# 02_prepare_variables.R), que distingue exactamente esto.
#
# Puramente descriptivo (puntuacion bruta ponderada, sin modelo). Se
# agrupan las 3 ediciones porque, dentro de un solo territorio y un solo
# tipo de centro, las celdas de incongruencia-migrante quedan con muy
# pocos casos por edicion (n < 5 en varias celdas de 2018/2022) -- por
# edicion serian demasiado inestables para interpretar.
#
# Requiere haber corrido R/01_load_data.R -> R/02_prepare_variables.R.
# Si Navarra todavia no esta en ccaa_bilingues (R/01_load_data.R), este
# script funciona igual mostrando solo Pais Vasco.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

student_long <- readRDS("data/student_bilingue_long.rds")
student <- student_long |> distinct(year, student_id, .keep_all = TRUE)

perfil_incongruencia <- function(detalle) {
  case_when(
    detalle %in% c("congruente_espanol", "congruente_cooficial") ~ "Congruente",
    detalle %in% c("incongruente_examen_espanol_casa_cooficial",
                   "incongruente_examen_cooficial_casa_espanol") ~ "Incongruente, no migrante",
    detalle %in% c("incongruente_examen_espanol_casa_otro",
                   "incongruente_examen_cooficial_casa_otro") ~ "Incongruente, migrante",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Congruente", "Incongruente, no migrante", "Incongruente, migrante"))
}

base <- student |>
  filter(grepl("Vasco|Navarra", ccaa),
         centro_especializacion %in% c("monolingue_espanol", "monolingue_regional"),
         !is.na(congruencia_detalle)) |>
  mutate(perfil = perfil_incongruencia(congruencia_detalle))

message("=== N por CCAA (deberia incluir Navarra tras re-ejecutar 01_load_data.R) ===")
print(table(base$ccaa))

# --- puntuacion media (long: alumno x dominio) ------------------------------
long_base <- student_long |>
  semi_join(base |> select(year, student_id), by = c("year", "student_id")) |>
  filter(centro_especializacion %in% c("monolingue_espanol", "monolingue_regional"),
         !is.na(congruencia_detalle)) |>
  mutate(perfil = perfil_incongruencia(congruencia_detalle))

vasco_navarra_incong_migrante <- long_base |>
  group_by(ccaa, centro_especializacion, perfil) |>
  summarise(
    score_medio = weighted.mean(score, stu_wgt, na.rm = TRUE),
    n_alumnos = n_distinct(student_id),
    .groups = "drop"
  )

# "caida" = diferencia frente al congruente del MISMO tipo de centro y
# la MISMA CCAA
baseline <- vasco_navarra_incong_migrante |>
  filter(perfil == "Congruente") |>
  select(ccaa, centro_especializacion, score_congruente = score_medio)

vasco_navarra_incong_migrante <- vasco_navarra_incong_migrante |>
  left_join(baseline, by = c("ccaa", "centro_especializacion")) |>
  mutate(caida_vs_congruente = score_medio - score_congruente)

message("\n=== Puntuacion media y caida vs. congruente, por CCAA x tipo de centro x perfil ===")
print(vasco_navarra_incong_migrante |> arrange(ccaa, centro_especializacion, perfil) |> as.data.frame())

saveRDS(vasco_navarra_incong_migrante, "data/vasco_navarra_incong_migrante.rds")
cat("\nOK\n")
