# =============================================================================
# 01_load_data.R
# Carga y filtra los tres origenes de datos a los SEIS territorios
# bilingues/con lengua cooficial (Pais Vasco, Cataluna, Galicia,
# Comunidad Valenciana, Illes Balears, Navarra), anadiendo las
# variables linguisticas individuales (LANGTEST_QQQ, LANGN) que no
# existen en los proyectos hermanos.
#
# NOTA sobre Navarra: a diferencia de las otras 5, el euskera es
# cooficial solo en la "zona vascofona" de Navarra (no en toda la
# comunidad) -- la Ley Foral 18/1986 divide el territorio en zona
# vascofona, mixta y no vascofona. Esto NO exige ningun tratamiento
# especial aqui: centro_especializacion (02_prepare_variables.R) ya se
# calcula de forma empirica, colegio a colegio, a partir del idioma
# real en que cada alumno hizo la prueba -- un colegio navarro de la
# zona no vascofona sencillamente saldra "monolingue_espanol" porque
# el 100% de sus alumnos habra hecho el examen en castellano, exactamente
# igual que ocurriria si Navarra tuviera una unica zona linguistica.
# Solo los colegios de la zona vascofona/mixta con alumnado evaluado en
# euskera apareceran como "monolingue_regional" o "mixto". Se incluye
# Navarra completa (no solo la zona vascofona) porque REGION/STRATUM no
# permiten distinguir la sub-zona, y porque asi se trata igual que el
# resto de CCAA (algunas de las cuales tampoco son 100% homogeneas en
# su lengua cooficial en todo su territorio, p.ej. Comunidad
# Valenciana).
#
# TRES FUENTES DISTINTAS, verificadas directamente contra los ficheros
# reales (pyreadstat) antes de escribir este script:
#
#   - 2022: fichero INEE nacional (igual que pisa-espana-ccaa). REGION
#     SI identifica CCAA real (19 valores: 17 CCAA + Ceuta + Melilla).
#   - 2025: fichero INTERNACIONAL de la OCDE (CY09_MS_STU_PUF.sav,
#     filtrado a CNT=="Spain") -- igual que
#     ../pisa-espana-ccaa/R/01d_incorporate_pisa2025_ccaa.R. REGION
#     tambien identifica CCAA real aqui (19 valores), con SU PROPIA
#     tabla de codigos (region_lookup_2025 mas abajo) -- NO es la misma
#     tabla de codigos que la de 2022 aunque el nombre de la variable
#     coincida (ver el aviso largo en 01d de pisa-espana-ccaa).
#   - 2018: fichero INEE nacional (ESP_06_Diciembre_PISA2018.sav). NO
#     tiene REGION ni SUBNATIO utilizables (SUBNATIO colapsa a un unico
#     valor "Spain" para las 35.943 filas, y no existe columna REGION en
#     este fichero) -- pero SI tiene CCAA real por otra via, encontrada
#     al revisar por que 2018 parecia distinto de 2009/2012/2015/2022:
#     `STRATUM` codifica, en su etiqueta de texto, la CCAA + publico/
#     privado + (SOLO para Pais Vasco) el modelo linguistico AB/D. Ver
#     `stratum_lookup_2018` mas abajo, construida y verificada contra
#     las 43 etiquetas de valor reales del fichero. Con esto 2018 se
#     identifica con la MISMA fiabilidad que 2022/2025 (`ccaa_source =
#     "stratum_2018"`), sin proxy ni perdida de precision.
#
# Publico/privado: disponible en las 3 ediciones -- 2022
# (PISA2022_CentrosEducativos_Esp.sav), 2025 (CY09_MS_SCH_PUF.sav) y
# 2018 (via STRATUM, ver arriba -- no necesita fichero de centro aparte).
#
# Bonus no pedido explicitamente pero relevante para la pregunta abierta
# sobre el modelo linguistico vasco: `modelo_linguistico_pv` (AB/D) sale
# GRATIS de STRATUM en 2018 (las 4 combinaciones publico/privado x AB/D)
# y de forma PARCIAL en 2022 (solo distingue AB/D para centros PRIVADOS;
# el publico queda sin dividir). No es la clasificacion oficial completa
# que ibas a revisar tu mismo, pero da ya una primera aproximacion real
# sin esperar a esa revision.
# =============================================================================

library(haven)
library(dplyr)
library(purrr)
library(tidyselect)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# Las 5 CCAA de interes, con los nombres exactamente como los usan
# region_lookup_2022 / region_lookup_2025 mas abajo.
ccaa_bilingues <- c("País Vasco", "Cataluña", "Galicia", "Comunidad Valenciana", "Illes Balears", "Navarra")

# --- Rutas a los ficheros fuente ---------------------------------------------
# Ajusta si alguna no coincide con tu disco.
path_2022_stu <- "../pisa-espana-ccaa/data/PISA2022_Estudiantes_Esp.sav"
path_2022_sch <- "../pisa-espana-ccaa/data/PISA2022_CentrosEducativos_Esp.sav"
path_2018_stu <- "/Volumes/discojuli/PISA/dataesp/ESP_06_Diciembre_PISA2018.sav"
path_2025_stu <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.sav"
path_2025_sch <- "/Volumes/discojuli/PISA/dataesp/CY09_MS_SCH_PUF.sav"

for (p in c(path_2022_stu, path_2022_sch, path_2018_stu, path_2025_stu)) {
  if (!file.exists(p)) stop("No encuentro: ", p, " -- ajusta la ruta al principio del script.")
}

harmonize_public_private <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("1", "public", "publico", "público")  ~ "publico",
    x_chr %in% c("2", "private", "privado")             ~ "privado",
    TRUE ~ NA_character_
  )
}

pv_mean <- function(df, domain_suffix) {
  cols <- grep(paste0("^PV\\d+", domain_suffix, "$"), names(df), value = TRUE)
  stopifnot(length(cols) == 10)
  rowMeans(df[cols], na.rm = TRUE)
}

# =============================================================================
# 2022 -- fichero INEE nacional, REGION real
# =============================================================================
region_lookup_2022 <- tibble::tribble(
  ~region_code, ~ccaa,
  72401, "Andalucía",
  72402, "Aragón",
  72403, "Asturias",
  72404, "Illes Balears",
  72405, "Canarias",
  72406, "Cantabria",
  72407, "Castilla y León",
  72408, "Castilla-La Mancha",
  72409, "Cataluña",
  72410, "Extremadura",
  72411, "Galicia",
  72412, "La Rioja",
  72413, "Madrid",
  72414, "Murcia",
  72415, "Navarra",
  72416, "País Vasco",
  72417, "Comunidad Valenciana",
  72418, "Ceuta",
  72419, "Melilla"
)

hisced_to_3cat_2022 <- function(x) {
  case_when(
    x %in% c(1, 2, 3)     ~ "baja",
    x %in% c(4, 5, 6)     ~ "media",
    x %in% c(7, 8, 9, 10) ~ "alta",
    TRUE ~ NA_character_
  )
}

message("Cargando 2022...")
raw2022 <- haven::read_sav(path_2022_stu)
stopifnot("LANGTEST_QQQ" %in% names(raw2022), "LANGN" %in% names(raw2022), "STRATUM" %in% names(raw2022))

# modelo_linguistico_pv -- BONUS, PARCIAL: verificado que en 2022 STRATUM
# solo distingue AB/D para centros PRIVADOS de Pais Vasco (stratum 32/33:
# "Pais Vasco,Private,Private AB" / "...D"); el publico queda como una
# sola categoria sin dividir ("Pais Vasco,Public,Public", stratum 31) --
# a diferencia de 2018, que si separa las 4 combinaciones. Se deja NA
# para el resto de CCAA y para Pais Vasco publico.
student_2022 <- raw2022 |>
  transmute(
    year = 2022L,
    region_code = as.integer(REGION),
    stratum_label = as.character(as_factor(STRATUM)),
    school_id = as.character(CNTSCHID),
    student_id = as.character(CNTSTUID),
    gender = case_when(ST004D01T == 1 ~ "female", ST004D01T == 2 ~ "male", TRUE ~ NA_character_),
    immig = case_when(IMMIG == 1 ~ "nativo", IMMIG == 2 ~ "segunda_gen", IMMIG == 3 ~ "primera_gen", TRUE ~ NA_character_),
    parent_educ = factor(hisced_to_3cat_2022(HISCED), levels = c("baja", "media", "alta")),
    escs = ESCS,
    stu_wgt = W_FSTUWT,
    lang_test = as.character(as_factor(LANGTEST_QQQ)),
    lang_home = as.character(as_factor(LANGN)),
    modelo_linguistico_pv = case_when(
      stratum_label == "ESP - stratum 32: País Vasco,Private,Private AB" ~ "AB",
      stratum_label == "ESP - stratum 33: País Vasco,Private,Private D"  ~ "D",
      TRUE ~ NA_character_
    ),
    math = pv_mean(raw2022, "MATH"),
    read = pv_mean(raw2022, "READ"),
    science = pv_mean(raw2022, "SCIE")
  ) |>
  select(-stratum_label) |>
  left_join(region_lookup_2022, by = "region_code") |>
  filter(ccaa %in% ccaa_bilingues) |>
  mutate(ccaa_source = "region_oficial") |>
  # region_code solo hacia falta para el join de arriba. Se descarta
  # aqui en vez de intentar unificar su tipo con el de 2025 (alli sale
  # character, no integer): bind_rows() no admite combinar, entre
  # ediciones, una columna integer con una character del mismo nombre
  # ("Can't combine ...$region_code <integer> and <character>").
  select(-region_code)

sch2022 <- haven::read_sav(path_2022_sch)
school_type_2022 <- sch2022 |>
  transmute(
    school_id = as.character(CNTSCHID),
    public_private = coalesce(
      if ("PRIVATESCH" %in% names(sch2022)) harmonize_public_private(PRIVATESCH) else NA_character_,
      if ("SC013Q01TA" %in% names(sch2022)) harmonize_public_private(SC013Q01TA) else NA_character_
    ) |> factor(levels = c("publico", "privado"))
  )
student_2022 <- student_2022 |> left_join(school_type_2022, by = "school_id")

message("2022 -- alumnos en las ", length(ccaa_bilingues), " CCAA bilingues: ", nrow(student_2022))
print(student_2022 |> count(ccaa, sort = TRUE))

# =============================================================================
# 2025 -- fichero internacional OCDE, REGION real (propia tabla de
# codigos, ver aviso al principio del script)
# =============================================================================
region_lookup_2025 <- tibble::tribble(
  ~region_code, ~ccaa,
  "72401", "Andalucía",
  "72402", "Aragón",
  "72403", "Asturias",
  "72404", "Illes Balears",
  "72405", "País Vasco",
  "72406", "Canarias",
  "72407", "Cantabria",
  "72408", "Castilla y León",
  "72409", "Castilla-La Mancha",
  "72410", "Cataluña",
  "72411", "Ceuta",
  "72412", "Comunidad Valenciana",
  "72413", "Extremadura",
  "72414", "Galicia",
  "72415", "La Rioja",
  "72416", "Madrid",
  "72417", "Melilla",
  "72418", "Murcia",
  "72419", "Navarra"
)

hisced_to_3cat_2025 <- function(x) {
  case_when(
    x %in% c(1, 2)       ~ "baja",
    x %in% c(3, 4, 5)    ~ "media",
    x %in% c(6, 7, 8, 9) ~ "alta",
    TRUE ~ NA_character_
  )
}

message("Cargando 2025 (fichero internacional, ~2.2GB, solo columnas necesarias)...")
raw2025_all <- haven::read_sav(
  path_2025_stu,
  col_select = c(
    "CNT", "CNTSCHID", "CNTSTUID", "REGION", "ST004D01T", "MALE", "IMMIG", "HISCED",
    "ESCS", "W_FSTUWT", "LANGTEST_QQQ", "LANGN",
    tidyselect::matches("^PV\\d+(MATH|READ|SCIE)$")
  )
)
# CNT es una variable etiquetada (haven_labelled): as.character()
# aplicado sin pasar antes por as_factor() devuelve el codigo ISO
# crudo ("ESP"), no la etiqueta formateada ("Spain"). Comparar
# directamente contra "Spain" no selecciona ninguna fila para 2025 sin
# avisar de ello (mismo mecanismo, en sentido inverso, que LANGN con
# sufijo ISO): la comparacion correcta es contra el codigo crudo,
# verificado con pyreadstat: CNT == "ESP" para España.
raw2025 <- raw2025_all |> filter(as.character(CNT) == "ESP")
rm(raw2025_all)
message("2025 -- alumnos ESP en el fichero internacional: ", nrow(raw2025))

student_2025 <- raw2025 |>
  transmute(
    year = 2025L,
    region_code = as.character(REGION),
    school_id = as.character(CNTSCHID),
    student_id = as.character(CNTSTUID),
    # ST004D01T ausente al 100% para Espana en 2025 (hallazgo de
    # ../pisa-espana-ccaa/R/01d) -- se usa MALE como respaldo.
    gender = coalesce(
      case_when(ST004D01T == 1 ~ "female", ST004D01T == 2 ~ "male", TRUE ~ NA_character_),
      case_when(MALE == 1 ~ "male", MALE == 0 ~ "female", TRUE ~ NA_character_)
    ),
    immig = case_when(IMMIG == 1 ~ "nativo", IMMIG == 2 ~ "segunda_gen", IMMIG == 3 ~ "primera_gen", TRUE ~ NA_character_),
    parent_educ = factor(hisced_to_3cat_2025(HISCED), levels = c("baja", "media", "alta")),
    escs = ESCS,
    stu_wgt = W_FSTUWT,
    lang_test = as.character(as_factor(LANGTEST_QQQ)),
    lang_home = as.character(as_factor(LANGN)),
    math = pv_mean(raw2025, "MATH"),
    read = pv_mean(raw2025, "READ"),
    science = pv_mean(raw2025, "SCIE")
  ) |>
  left_join(region_lookup_2025, by = "region_code") |>
  filter(ccaa %in% ccaa_bilingues) |>
  mutate(ccaa_source = "region_oficial") |>
  select(-region_code)  # ver nota de la misma correccion en el bloque de 2022

if (file.exists(path_2025_sch)) {
  sch2025 <- haven::read_sav(
    path_2025_sch,
    col_select = tidyselect::any_of(c("CNT", "CNTSCHID", "PRIVATESCH", "SC013Q01TA"))
  )
  school_type_2025 <- sch2025 |>
    filter(as.character(CNT) == "ESP") |>  # ver nota de la misma correccion mas arriba
    transmute(
      school_id = as.character(CNTSCHID),
      public_private = coalesce(
        if ("PRIVATESCH" %in% names(sch2025)) harmonize_public_private(PRIVATESCH) else NA_character_,
        if ("SC013Q01TA" %in% names(sch2025)) harmonize_public_private(SC013Q01TA) else NA_character_
      ) |> factor(levels = c("publico", "privado"))
    )
  student_2025 <- student_2025 |> left_join(school_type_2025, by = "school_id")
} else {
  message("2025: no encuentro ", path_2025_sch, " -- public_private quedara NA para esta edicion.")
  student_2025$public_private <- factor(NA_character_, levels = c("publico", "privado"))
}

message("2025 -- alumnos en las ", length(ccaa_bilingues), " CCAA bilingues: ", nrow(student_2025))
print(student_2025 |> count(ccaa, sort = TRUE))

# =============================================================================
# 2018 -- CORREGIDO: SI tiene identificacion de CCAA, via STRATUM (no via
# REGION/SUBNATIO, que en efecto no la traen -- ver aviso al principio
# del script). Verificado directamente contra las 43 etiquetas de valor
# reales de STRATUM en este fichero: codifica CCAA + publico/privado +,
# SOLO para Pais Vasco, el modelo linguistico AB/D (las 4 combinaciones:
# publico AB, publico D, privado AB, privado D). Ejemplos reales:
#   "ESP - stratum 21: Galicia, public"
#   "ESP - stratum 34: Basque Country, public, AB"
#   "ESP - stratum 35: Basque Country, public, D"
#   "ESP - stratum 25: Madrid, Bilingual Public"  (bilingue INGLES-espanol
#   de Madrid, sin relacion con las lenguas cooficiales -- se colapsa a
#   "Madrid, publico" igual que el resto de sub-estratos de Madrid)
# Con esto YA NO hace falta el proxy via LANGTEST_QQQ a nivel de colegio
# que se uso en la primera version de este script -- se sustituye por
# completo. Bonus: esto TAMBIEN da public_private para 2018, que en la
# primera version del script quedaba NA (no habia fichero de centro).
message("Cargando 2018...")
raw2018 <- haven::read_sav(path_2018_stu)
stopifnot("LANGTEST_QQQ" %in% names(raw2018), "LANGN" %in% names(raw2018), "STRATUM" %in% names(raw2018))

stratum_lookup_2018 <- tibble::tribble(
  ~stratum_label, ~ccaa, ~public_private_chr, ~modelo_linguistico_pv,
  "ESP - stratum 01: Andalusia, public", "Andalucía", "publico", NA_character_,
  "ESP - stratum 02: Andalusia, private", "Andalucía", "privado", NA_character_,
  "ESP - stratum 03: Aragon, public", "Aragón", "publico", NA_character_,
  "ESP - stratum 04: Aragon, private", "Aragón", "privado", NA_character_,
  "ESP - stratum 05: Asturias, public", "Asturias", "publico", NA_character_,
  "ESP - stratum 06: Asturias, private", "Asturias", "privado", NA_character_,
  "ESP - stratum 07: Balearic Islands, public", "Illes Balears", "publico", NA_character_,
  "ESP - stratum 08: Balearic Islands, private", "Illes Balears", "privado", NA_character_,
  "ESP - stratum 09: Canary Islands, public", "Canarias", "publico", NA_character_,
  "ESP - stratum 10: Canary Islands, private", "Canarias", "privado", NA_character_,
  "ESP - stratum 11: Cantabria, public", "Cantabria", "publico", NA_character_,
  "ESP - stratum 12: Cantabria, private", "Cantabria", "privado", NA_character_,
  "ESP - stratum 13: Castile and Leon, public", "Castilla y León", "publico", NA_character_,
  "ESP - stratum 14: Castile and Leon, private", "Castilla y León", "privado", NA_character_,
  "ESP - stratum 15: Castile – La Mancha, public", "Castilla-La Mancha", "publico", NA_character_,
  "ESP - stratum 16: Castile – La Mancha, private", "Castilla-La Mancha", "privado", NA_character_,
  "ESP - stratum 17: Catalonia, public", "Cataluña", "publico", NA_character_,
  "ESP - stratum 18: Catalonia, private", "Cataluña", "privado", NA_character_,
  "ESP - stratum 19: Extremadura, public", "Extremadura", "publico", NA_character_,
  "ESP - stratum 20: Extremadura, private", "Extremadura", "privado", NA_character_,
  "ESP - stratum 21: Galicia, public", "Galicia", "publico", NA_character_,
  "ESP - stratum 22: Galicia, private", "Galicia", "privado", NA_character_,
  "ESP - stratum 23: La Rioja, public", "La Rioja", "publico", NA_character_,
  "ESP - stratum 24: La Rioja, private", "La Rioja", "privado", NA_character_,
  "ESP - stratum 25: Madrid, Bilingual Public", "Madrid", "publico", NA_character_,
  "ESP - stratum 26: Madrid, Non-Bilingual Public", "Madrid", "publico", NA_character_,
  "ESP - stratum 27: Madrid, Bilingual Private (Publicly Funded)", "Madrid", "privado", NA_character_,
  "ESP - stratum 28: Madrid, Non-Bilingual Private (Publicly Funded)", "Madrid", "privado", NA_character_,
  "ESP - stratum 29: Madrid, private", "Madrid", "privado", NA_character_,
  "ESP - stratum 30: Murcia, public", "Murcia", "publico", NA_character_,
  "ESP - stratum 31: Murcia, private", "Murcia", "privado", NA_character_,
  "ESP - stratum 32: Navarra, public", "Navarra", "publico", NA_character_,
  "ESP - stratum 33: Navarra, private", "Navarra", "privado", NA_character_,
  "ESP - stratum 34: Basque Country, public, AB", "País Vasco", "publico", "AB",
  "ESP - stratum 35: Basque Country, public, D", "País Vasco", "publico", "D",
  "ESP - stratum 36: Basque Country, private, AB", "País Vasco", "privado", "AB",
  "ESP - stratum 37: Basque Country, private, D", "País Vasco", "privado", "D",
  "ESP - stratum 38: Valencia, public", "Comunidad Valenciana", "publico", NA_character_,
  "ESP - stratum 39: Valencia, private", "Comunidad Valenciana", "privado", NA_character_,
  "ESP - stratum 40: Ceuta, public", "Ceuta", "publico", NA_character_,
  "ESP - stratum 41: Ceuta, private", "Ceuta", "privado", NA_character_,
  "ESP - stratum 42: Melilla, public", "Melilla", "publico", NA_character_,
  "ESP - stratum 43: Melilla, private", "Melilla", "privado", NA_character_
)

student_2018 <- raw2018 |>
  transmute(
    year = 2018L,
    stratum_label = as.character(as_factor(STRATUM)),
    school_id = as.character(CNTSCHID),
    student_id = as.character(CNTSTUID),
    gender = case_when(ST004D01T == 1 ~ "female", ST004D01T == 2 ~ "male", TRUE ~ NA_character_),
    immig = case_when(IMMIG == 1 ~ "nativo", IMMIG == 2 ~ "segunda_gen", IMMIG == 3 ~ "primera_gen", TRUE ~ NA_character_),
    # HISCED en 2018 -- SIN VERIFICAR contra las etiquetas de valor reales
    # (a diferencia de 2022/2025 arriba, verificadas explicitamente). Se
    # asume aqui la misma escala 1-10 que 2022. Revisa el diagnostico de
    # NA de parent_educ para 2018 en la consola -- si sale ~100% NA, la
    # escala no es la misma y hay que corregir aqui.
    parent_educ = factor(hisced_to_3cat_2022(HISCED), levels = c("baja", "media", "alta")),
    escs = ESCS,
    stu_wgt = W_FSTUWT,
    lang_test = as.character(as_factor(LANGTEST_QQQ)),
    lang_home = as.character(as_factor(LANGN)),
    math = pv_mean(raw2018, "MATH"),
    read = pv_mean(raw2018, "READ"),
    science = pv_mean(raw2018, "SCIE")
  ) |>
  left_join(stratum_lookup_2018, by = "stratum_label")

n_sin_stratum_2018 <- sum(is.na(student_2018$ccaa))
if (n_sin_stratum_2018 > 0) {
  message(
    n_sin_stratum_2018, " filas de 2018 con STRATUM sin reconocer -- revisa ",
    "stratum_lookup_2018 contra las etiquetas reales del fichero (pueden haber ",
    "cambiado respecto a lo verificado con pyreadstat)."
  )
}

pct_na_parent_educ_2018 <- mean(is.na(student_2018$parent_educ))
message(
  "2018 -- % NA en parent_educ con la escala HISCED asumida (1-10, igual que 2022): ",
  scales::percent(pct_na_parent_educ_2018, accuracy = 0.1),
  if (pct_na_parent_educ_2018 > 0.30) " *** AVISO: parece demasiado alto, revisa la escala HISCED de 2018 antes de continuar ***" else ""
)

student_2018 <- student_2018 |>
  filter(ccaa %in% ccaa_bilingues) |>
  mutate(
    ccaa_source = "stratum_2018",
    public_private = factor(public_private_chr, levels = c("publico", "privado"))
  ) |>
  select(-stratum_label, -public_private_chr)

message("2018 -- alumnos en las ", length(ccaa_bilingues), " CCAA bilingues (via STRATUM real): ", nrow(student_2018))
print(student_2018 |> count(ccaa, sort = TRUE))
message("2018 -- Pais Vasco, modelo linguistico (AB/D) segun STRATUM:")
print(student_2018 |> filter(ccaa == "País Vasco") |> count(modelo_linguistico_pv, public_private))

# =============================================================================
# Union y guardado
# =============================================================================
student_bilingue <- bind_rows(student_2022, student_2025, student_2018) |>
  mutate(global_school_id = paste(coalesce(ccaa, "sin_ccaa"), year, school_id, sep = "_"))

message(
  "\nTOTAL -- alumnos: ", nrow(student_bilingue),
  " | por edicion y origen de CCAA:"
)
print(student_bilingue |> count(year, ccaa_source))

saveRDS(student_bilingue, "data/student_bilingue_pooled.rds")
