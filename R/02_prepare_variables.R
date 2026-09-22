# =============================================================================
# 02_prepare_variables.R
# Construye las TRES piezas linguisticas del proyecto sobre
# data/student_bilingue_pooled.rds (01_load_data.R):
#
#   1. congruencia_linguistica (individual): coincide o no el idioma del
#      examen (lang_test) con el idioma de casa (lang_home).
#   2. centro_especializacion (colegio): idioma dominante de examen entre
#      TODOS los alumnos evaluados de ese colegio, y homogeneidad.
#   3. public_private ya viene de 01_load_data.R -- aqui solo se
#      confirma su cobertura por edicion.
#
# Ademas construye los cuartiles ESCS, y el estrato interseccional
# MAIHDA de 5 dimensiones (genero x educacion parental x cuartil ESCS x
# origen migrante x congruencia linguistica) -- decision confirmada:
# la congruencia linguistica entra como dimension propia del estrato,
# no solo como covariable.
# =============================================================================

library(dplyr)
library(tidyr)
library(forcats)

student <- readRDS("data/student_bilingue_pooled.rds")

# =============================================================================
# 1. Congruencia linguistica individual
# =============================================================================
# lang_test (LANGTEST_QQQ) en los ficheros de Espana es, en la practica,
# casi siempre "Spanish" o uno de los 4 idiomas cooficiales (ver
# 01_load_data.R) -- PISA no administra el examen en otros idiomas de
# inmigracion en Espana. lang_home (LANGN) si puede ser cualquier lengua
# hablada en el mundo. Para que "congruencia" capture especificamente el
# eje castellano/lengua-cooficial (que es la pregunta de este proyecto,
# no la de reclasificacion migrante por idioma de origen, que queda
# como extension futura), se colapsan ambas variables a 3 categorias:
#   "espanol" (castellano), "cooficial" (agrupa euskera/catalan/gallego/
#   valenciano), "otro_idioma" (cualquier lengua de inmigracion, p.ej.
#   arabe, rumano).
# El aranes se asimila a "cooficial": en el Valle de Aran es cooficial
# junto al catalan y el castellano, aunque PISA no lo trate como una de
# sus 4 lenguas cooficiales armonizadas. Se incluyen las dos grafias
# que pueden aparecer en las etiquetas SPSS.
idiomas_cooficiales <- c("Basque", "Catalan", "Galician", "Valencian", "Aranese", "Aranes")

collapse_lang <- function(x) {
  # 2025 etiqueta LANGN (no LANGTEST_QQQ) con el codigo ISO entre
  # parentesis, p.ej. "Spanish (spa)" en vez de "Spanish" como en
  # 2018/2022 -- verificado directamente contra los datos reales (sin
  # esto, congruencia_linguistica salia con ~0% de cobertura SOLO en
  # 2025, de forma silenciosa -- las otras 2 ediciones parecian
  # correctas). Se normaliza quitando el sufijo " (xxx)" antes de
  # comparar, para que la comparacion funcione igual en las 3 ediciones.
  x_norm <- sub(" \\([a-z]+\\)$", "", x)
  case_when(
    x_norm == "Spanish" ~ "espanol",
    x_norm %in% idiomas_cooficiales ~ "cooficial",
    !is.na(x_norm) ~ "otro_idioma",
    TRUE ~ NA_character_
  )
}

student <- student |>
  mutate(
    lang_test_3cat = collapse_lang(lang_test),
    lang_home_3cat = collapse_lang(lang_home),
    # Version detallada (6 categorias -- antes 4). Distingue tambien
    # los casos incongruentes en los que la lengua de casa es
    # "otro_idioma" (una lengua de inmigracion) en vez de colapsarlos
    # en un NA generico, de modo que queden representadas todas las
    # combinaciones posibles de lengua de examen x lengua de casa.
    # lang_test en Espana es, en la practica, siempre castellano o
    # cooficial (PISA no administra el examen en otras lenguas), asi
    # que no hay combinaciones con lang_test_3cat == "otro_idioma".
    congruencia_detalle = case_when(
      lang_test_3cat == "espanol"   & lang_home_3cat == "espanol"     ~ "congruente_espanol",
      lang_test_3cat == "cooficial" & lang_home_3cat == "cooficial"   ~ "congruente_cooficial",
      lang_test_3cat == "cooficial" & lang_home_3cat == "espanol"     ~ "incongruente_examen_cooficial_casa_espanol",
      lang_test_3cat == "espanol"   & lang_home_3cat == "cooficial"   ~ "incongruente_examen_espanol_casa_cooficial",
      lang_test_3cat == "espanol"   & lang_home_3cat == "otro_idioma" ~ "incongruente_examen_espanol_casa_otro",
      lang_test_3cat == "cooficial" & lang_home_3cat == "otro_idioma" ~ "incongruente_examen_cooficial_casa_otro",
      TRUE ~ NA_character_  # solo dato realmente ausente (lang_test o lang_home missing)
    ),
    # Version binaria, la que entra en el estrato MAIHDA y en el modelo.
    # El alumnado cuyo idioma de casa es una lengua de inmigracion no
    # se excluye (NA): son casos claros de incongruencia por
    # definicion, ya que su lengua de casa, por construccion, nunca
    # puede coincidir con la lengua del examen (siempre castellano o
    # cooficial en Espana). Excluirlos eliminaria justo la subpoblacion
    # de mayor interes (el 77% de este grupo es de origen migrante). NA
    # queda reservado para cuando falta el dato real de lang_test o
    # lang_home. Si algun analisis puntual necesita restringirse al eje
    # castellano/cooficial, basta con filtrar explicitamente por
    # lang_home_3cat == "otro_idioma" en ese analisis -- no es el
    # comportamiento por defecto de esta variable.
    congruencia_linguistica = case_when(
      is.na(lang_test_3cat) | is.na(lang_home_3cat) ~ NA_character_,
      lang_test_3cat == lang_home_3cat ~ "congruente",
      TRUE ~ "incongruente"
    ) |> factor(levels = c("congruente", "incongruente"))
  )

message("Congruencia linguistica -- distribucion (detalle):")
print(student |> count(congruencia_detalle, sort = TRUE))
message(
  "Congruencia linguistica -- cobertura de la version binaria usada en el modelo: ",
  scales::percent(mean(!is.na(student$congruencia_linguistica))), " de los alumnos ",
  "(el resto es dato realmente ausente de lang_test o lang_home; el alumnado con lengua ",
  "de casa de inmigracion ya NO se excluye, se clasifica como 'incongruente')."
)

# =============================================================================
# 2. Especializacion linguistica del centro
# =============================================================================
# Idioma dominante de examen (lang_test, SIN colapsar -- se distingue
# catalan/valenciano/euskera/gallego, no solo "cooficial") entre TODOS
# los alumnos evaluados de cada colegio-edicion (global_school_id), mas
# el grado de homogeneidad. Validado empiricamente antes de construir
# esto: 88-91% de los colegios (segun edicion) son 100% homogeneos en
# lang_test; el umbral del 90% de abajo es deliberadamente laxo para no
# etiquetar de "mixto" a colegios con 1-2 alumnos excepcionales sobre
# una muestra de ~30.
centro_lang <- student |>
  filter(!is.na(lang_test)) |>
  count(global_school_id, lang_test, name = "n_lang") |>
  group_by(global_school_id) |>
  mutate(n_colegio_lang = sum(n_lang), pct = n_lang / n_colegio_lang) |>
  slice_max(order_by = pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    global_school_id,
    lang_dominante_colegio = lang_test,
    pct_dominante_colegio = pct,
    n_colegio_lang,
    centro_especializacion = case_when(
      lang_dominante_colegio == "Spanish" & pct_dominante_colegio >= 0.90 ~ "monolingue_espanol",
      lang_dominante_colegio != "Spanish" & pct_dominante_colegio >= 0.90 ~ "monolingue_regional",
      TRUE ~ "mixto"
    ) |> factor(levels = c("monolingue_espanol", "monolingue_regional", "mixto"))
  )

student <- student |> left_join(centro_lang, by = "global_school_id")

message("\nEspecializacion linguistica del centro -- distribucion de colegios (no de alumnos):")
print(centro_lang |> count(centro_especializacion, sort = TRUE))
message(
  "Colegios con muestra muy pequena (n_colegio_lang < 10) -- pct_dominante_colegio puede ser ",
  "poco fiable en esos casos: ", sum(centro_lang$n_colegio_lang < 10), " de ", nrow(centro_lang)
)

# =============================================================================
# Cuartiles ESCS, educacion parental (ya categorizada), y estrato de 5
# dimensiones
# =============================================================================
student_prep <- student |>
  mutate(gender = factor(gender, levels = c("female", "male"))) |>
  group_by(year) |>
  mutate(escs_q = ntile(escs, 4) |> factor(labels = c("Q1_bajo", "Q2", "Q3", "Q4_alto"))) |>
  ungroup() |>
  mutate(
    # Estrato MAIHDA de 5 dimensiones: genero x educacion parental x
    # cuartil ESCS x origen migrante x congruencia linguistica (decision
    # confirmada -- congruencia entra como dimension propia del estrato,
    # no solo como covariable). 2 x 3 x 4 x 3 x 2 = 144 estratos
    # posibles (frente a los 72 de pisa-espana-ccaa).
    strata = interaction(gender, parent_educ, escs_q, immig, congruencia_linguistica, drop = TRUE) |> fct_drop(),
    # strata_4dim: las MISMAS 4 dimensiones que pisa-espana-ccaa (sin
    # congruencia), sobre la MISMA muestra de 5 CCAA bilingues -- permite
    # aislar, en R/03b_vpc_ablation_congruencia.R, cuanto del aumento
    # del VPC de estrato se debe a anadir la congruencia como 5a
    # dimension y cuanto a la restriccion muestral a estas 5 CCAA (el
    # proyecto hermano usa Espana completa, asi que comparar
    # directamente su VPC de estrato con el de aqui confundiria ambas
    # cosas).
    strata_4dim = interaction(gender, parent_educ, escs_q, immig, drop = TRUE) |> fct_drop()
  )

n_strata <- n_distinct(student_prep$strata)
message(
  "\nNumero de estratos interseccionales: ", n_strata,
  " (2 genero x 3 educ. parental x 4 ESCS x 3 immig x 2 congruencia = 144 posibles)"
)
message("Alumnos por edicion y CCAA (NA = 2018, ver 01_load_data.R):")
print(student_prep |> count(year, ccaa, ccaa_source) |> arrange(year, desc(n)))

# --- Diagnostico de NA por edicion -------------------------------------------
na_diag <- student_prep |>
  group_by(year) |>
  summarise(
    n = n(),
    pct_na_gender = scales::percent(mean(is.na(gender)), accuracy = 0.1),
    pct_na_parent_educ = scales::percent(mean(is.na(parent_educ)), accuracy = 0.1),
    pct_na_escs_q = scales::percent(mean(is.na(escs_q)), accuracy = 0.1),
    pct_na_immig = scales::percent(mean(is.na(immig)), accuracy = 0.1),
    pct_na_congruencia = scales::percent(mean(is.na(congruencia_linguistica)), accuracy = 0.1),
    pct_na_strata = scales::percent(mean(is.na(strata)), accuracy = 0.1),
    pct_na_centro_especializacion = scales::percent(mean(is.na(centro_especializacion)), accuracy = 0.1),
    pct_na_public_private = scales::percent(mean(is.na(public_private)), accuracy = 0.1),
    .groups = "drop"
  )
message("\nDiagnostico de valores ausentes por edicion:")
print(na_diag, n = Inf)

student_long <- student_prep |>
  select(
    year, ccaa, ccaa_source, school_id, student_id, global_school_id,
    gender, immig, parent_educ, escs_q, congruencia_linguistica, congruencia_detalle,
    centro_especializacion, pct_dominante_colegio, public_private, strata, strata_4dim, stu_wgt,
    math, read, science
  ) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  mutate(domain = factor(domain, levels = c("math", "read", "science")))

saveRDS(student_prep, "data/student_bilingue_prep.rds")
saveRDS(student_long, "data/student_bilingue_long.rds")

message("\nstudent_long: ", nrow(student_long), " filas (alumno x dominio)")
