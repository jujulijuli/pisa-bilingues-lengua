# =============================================================================
# 05_composicion_migrante_ccaa.R
#
# OBJETIVO: analisis puramente descriptivo (sin ajustar ningun modelo nuevo)
# de como ha cambiado, edicion a edicion, la composicion del alumnado por
# origen migrante, tipo de centro y CCAA -- como insumo para interpretar por
# que el efecto de la incongruencia linguistica y el de especializacion del
# centro se vuelven negativos y significativos precisamente en 2025 (ver
# qmd/05-interpretacion-2025.qmd). No requiere INLA: solo lee
# data/student_bilingue_long.rds y hace agregados ponderados con dplyr.
#
# Requiere haber corrido R/01_load_data.R -> R/02_prepare_variables.R.
# =============================================================================

library(dplyr)
library(tidyr)

student_long <- readRDS("data/student_bilingue_long.rds")

# Una fila por alumno (no por alumno x dominio) para todo lo que no sea
# puntuacion -- de lo contrario cada conteo triplicaria a los alumnos.
student <- student_long |> distinct(year, student_id, .keep_all = TRUE)

# -----------------------------------------------------------------------
# 1. Composicion migrante, global y dentro de cada tipo de centro
# -----------------------------------------------------------------------
composicion_migrante_global <- student |>
  group_by(year) |>
  summarise(pct_no_nativo = 100 * weighted.mean(immig != "nativo", stu_wgt, na.rm = TRUE), .groups = "drop")

composicion_migrante_centro <- student |>
  filter(!is.na(centro_especializacion)) |>
  group_by(year, centro_especializacion) |>
  summarise(pct_no_nativo = 100 * weighted.mean(immig != "nativo", stu_wgt, na.rm = TRUE), n = n(), .groups = "drop")

# -----------------------------------------------------------------------
# 2. Composicion migrante y de tipo de centro, dentro de cada CCAA
# -----------------------------------------------------------------------
composicion_migrante_ccaa <- student |>
  group_by(year, ccaa) |>
  summarise(pct_no_nativo = 100 * weighted.mean(immig != "nativo", stu_wgt, na.rm = TRUE), n = n(), .groups = "drop")

composicion_centro_ccaa <- student |>
  filter(!is.na(centro_especializacion)) |>
  group_by(year, ccaa, centro_especializacion) |>
  summarise(peso = sum(stu_wgt), .groups = "drop") |>
  group_by(year, ccaa) |>
  mutate(pct_alumnos = 100 * peso / sum(peso)) |>
  ungroup() |>
  select(year, ccaa, centro_especializacion, pct_alumnos)

# -----------------------------------------------------------------------
# 3. % de alumnado incongruente dentro de cada tipo de centro, por ano
#    (para distinguir "mas incongruencia" de "mismo % de incongruencia,
#    pero con perfil mas vulnerable")
# -----------------------------------------------------------------------
pct_incongruente_centro <- student |>
  filter(!is.na(centro_especializacion), !is.na(congruencia_linguistica)) |>
  group_by(year, centro_especializacion, congruencia_linguistica) |>
  summarise(peso = sum(stu_wgt), .groups = "drop") |>
  group_by(year, centro_especializacion) |>
  mutate(pct = 100 * peso / sum(peso)) |>
  ungroup() |>
  filter(congruencia_linguistica == "incongruente") |>
  select(year, centro_especializacion, pct_incongruente = pct)

# -----------------------------------------------------------------------
# 4. Puntuacion media (bruta, sin ajustar) por congruencia x tipo de
#    centro x ano -- para contrastar con el efecto ajustado del modelo
#    (03_model_maihda_inla.R) y ver si la brecha bruta se mueve igual.
# -----------------------------------------------------------------------
score_medio_centro_congruencia <- student_long |>
  filter(!is.na(centro_especializacion), !is.na(congruencia_linguistica)) |>
  group_by(year, centro_especializacion, congruencia_linguistica) |>
  summarise(score_medio = weighted.mean(score, stu_wgt, na.rm = TRUE), n = n(), .groups = "drop")

# -----------------------------------------------------------------------
# 5. Mezcla de tipo de centro DENTRO de cada CCAA -- el "reordenamiento"
#    del Pais Vasco entre 2022 y 2025 es el caso mas llamativo (ver nota
#    en el capitulo de interpretacion): recordar que centro_especializacion
#    se recalcula cada edicion a partir del idioma dominante de examen
#    entre los alumnos evaluados de cada colegio (ver operativizacion en
#    02_prepare_variables.R), asi que un colegio puede cambiar de
#    categoria entre ediciones sin que haya cambiado su modelo
#    linguistico real -- y con muestras de colegios distintas en cada
#    ciclo PISA, la mezcla agregada por CCAA puede moverse bastante.
n_colegios_ccaa <- student |>
  group_by(year, ccaa) |>
  summarise(n_alumnos = n(), n_colegios = n_distinct(global_school_id), .groups = "drop")

saveRDS(composicion_migrante_global, "data/composicion_migrante_global.rds")
saveRDS(composicion_migrante_centro, "data/composicion_migrante_centro.rds")
saveRDS(composicion_migrante_ccaa, "data/composicion_migrante_ccaa.rds")
saveRDS(composicion_centro_ccaa, "data/composicion_centro_ccaa.rds")
saveRDS(pct_incongruente_centro, "data/pct_incongruente_centro.rds")
saveRDS(score_medio_centro_congruencia, "data/score_medio_centro_congruencia.rds")
saveRDS(n_colegios_ccaa, "data/n_colegios_ccaa.rds")

message("\n=== Composicion migrante global, por ano ===")
print(composicion_migrante_global)
message("\n=== % no nativo dentro de cada tipo de centro, por ano ===")
print(composicion_migrante_centro |> select(-n) |> tidyr::pivot_wider(names_from = year, values_from = pct_no_nativo))
message("\n=== % no nativo por CCAA, por ano ===")
print(composicion_migrante_ccaa |> select(-n) |> tidyr::pivot_wider(names_from = year, values_from = pct_no_nativo))
message("\n=== Mezcla de tipo de centro dentro de cada CCAA, por ano ===")
print(composicion_centro_ccaa |> tidyr::pivot_wider(names_from = year, values_from = pct_alumnos), n = Inf)
message("\n=== N alumnos y colegios por CCAA, por ano ===")
print(n_colegios_ccaa |> tidyr::pivot_wider(names_from = year, values_from = c(n_alumnos, n_colegios)))
