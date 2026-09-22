# El papel de la lengua en PISA: territorios bilingües de España

Proyecto Quarto/R hermano de [`pisa-espana-ccaa`](../pisa-espana-ccaa) y
[`pisa-maihda-trends`](../../pisa-maihda-trends), centrado en los cinco
territorios bilingües de España -- País Vasco, Cataluña, Galicia,
Comunidad Valenciana e Illes Balears -- e incorporando la lengua como
factor explicativo en tres niveles, confirmados contigo:

1. **Congruencia lingüística individual**: coincide o no el idioma del
   examen (`LANGTEST_QQQ`) con el idioma de casa (`LANGN`).
2. **Interacción individuo-colegio**: la "especialización lingüística
   del centro" (idioma dominante de examen entre sus alumnos
   evaluados) en interacción con la congruencia individual.
3. **Público/privado**, ajustado, como en los proyectos hermanos.

Reutiliza el mismo armazón MAIHDA + INLA bayesiano, con la congruencia
lingüística como **5ª dimensión del estrato interseccional** (género ×
educación parental × ESCS × origen migrante × congruencia lingüística
-- 144 estratos posibles, frente a los 72 de `pisa-espana-ccaa`).

## Estado: pipeline ejecutado con datos reales (2026-09)

`R/01_load_data.R` → `R/02_prepare_variables.R` → `R/03_model_maihda_inla.R`
se han ejecutado con éxito contra los tres ficheros reales (29.086
alumnos, 5 CCAA, 2018/2022/2025). Como era de esperar, la primera
ejecución real sacó varios bugs que solo aparecían con los datos reales
(no con `parse()`): el filtro `CNT=="Spain"` de 2025 comparaba contra la
etiqueta en vez del código crudo `"ESP"` y daba 0 filas; `bind_rows()`
fallaba por `region_code` con tipos distintos entre 2022 (integer) y
2025 (character). Ambos corregidos y documentados en el propio código.
Resultados reales ya en `qmd/04-resultados.qmd`.

### Corrección importante (misma sesión): 2018 SÍ tiene CCAA real

Una primera versión de este script concluyó que 2018 no tenía
identificación de comunidad autónoma (`SUBNATIO` colapsa a un único
valor, no existe `REGION` en ese fichero) y usaba un proxy vía huella
lingüística del centro. Revisando `STRATUM` se encontró que sí la
tiene -- su etiqueta de texto codifica CCAA + público/privado + (solo
para País Vasco) modelo lingüístico AB/D. Ya corregido en
`R/01_load_data.R` (tabla `stratum_lookup_2018`, verificada contra las
43 etiquetas reales del fichero); ver `qmd/02-datos-lengua.qmd` para el
detalle. Esto también da público/privado para 2018 (antes no
disponible) y una primera aproximación parcial al modelo lingüístico
vasco en 2018 (completa) y 2022 (solo centros privados).

### Congruencia lingüística: tratamiento del alumnado con lengua de inmigración

El alumnado cuya lengua de casa no es ni castellano ni una lengua
cooficial (lengua de inmigración) se incorpora a `congruencia_linguistica`
en lugar de quedar excluido (`NA`), como ocurría en una primera versión
de la variable:

- El aranés se asimila a `cooficial` (cooficial en el Valle de Arán
  junto con catalán y castellano) -- 14 alumnos.
- El alumnado con lengua de casa de inmigración no se excluye: como
  `lang_test` en España es siempre castellano o cooficial, este grupo
  es, por construcción, siempre `incongruente` -- es precisamente el
  caso de mayor interés (77% de origen migrante), no uno a descartar
  por defecto. `NA` queda reservado para el dato realmente ausente.
- `congruencia_detalle` pasa de 4 a 6 categorías para distinguir los
  dos tipos de incongruencia con lengua de casa de inmigración.
- Cualquier exclusión de esta subpoblación, si hace falta para un
  análisis concreto, se aplica como filtro explícito puntual
  (`lang_home_3cat == "otro_idioma"`), no como comportamiento por
  defecto.

Validado contra los datos reales de las 3 ediciones (vía
`build_series.py`, réplica en Python del pipeline R -- ver más abajo):
la cobertura de `congruencia_linguistica` sube de 91,1% a 98,1% (NA:
2.582 → 555 alumnos, sobre un pool de 29.086). Detalle completo en
`qmd/02-datos-lengua.qmd`.

### Validación de la serie agregada 2018/2022/2025 con datos reales

Este entorno no tiene R/INLA disponible, así que la carga y
preparación de datos (equivalente a `01_load_data.R` +
`02_prepare_variables.R`) se ha replicado y ejecutado en Python
(`build_series.py`, usa pandas/pyreadstat directamente sobre tus
ficheros `.sav`) para validar la lógica contra los datos reales antes
de que ejecutes el pipeline R. Resultado: 29.086 alumnos en las 5 CCAA
bilingües (10.705 en 2018, 9.866 en 2022, 8.515 en 2025), guardado en
`data/student_bilingue_pooled.csv`. Este script es solo de validación
-- el pipeline autoritativo para ejecutar es el de R.

### Fuentes de datos, verificadas directamente con pyreadstat antes de escribir el código

| Edición | Fuente | CCAA | Público/privado | Modelo lingüístico PV (AB/D) |
|---|---|---|---|---|
| 2018 | `/Volumes/discojuli/PISA/dataesp/ESP_06_Diciembre_PISA2018.sav` | `STRATUM` | Sí, vía `STRATUM` | Completo (4 combinaciones) |
| 2022 | `../pisa-espana-ccaa/data/PISA2022_Estudiantes_Esp.sav` (+ centros) | `REGION` real | Sí | Parcial (solo privados) |
| 2025 | `/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.sav` (fichero internacional, filtrado a España) | `REGION` real (tabla de códigos PROPIA, no coincide con la de 2022 aunque el nombre de variable sea el mismo) | Sí, vía `CY09_MS_SCH_PUF.sav` | No disponible |

### Pendiente de verificar en la primera ejecución real

- **Escala de HISCED en 2018**: se ha asumido la misma escala 1-10 que
  2022 (sin verificar contra las etiquetas de valor reales del fichero
  de 2018, a diferencia de 2022/2025 que sí están verificadas). El
  script ya imprime el % de NA resultante en `parent_educ` para 2018 --
  si sale sospechosamente alto (>30%), la escala no es la misma y hay
  que corregir `hisced_to_3cat_2022()` usado para 2018 en
  `R/01_load_data.R`.
- **Tamaño de los estratos**: con 144 estratos posibles y la muestra
  restringida a 5 CCAA (mucho más pequeña que el análisis nacional
  completo), conviene revisar cuántos estratos quedan con muestra muy
  pequeña por edición -- MAIHDA está diseñado para manejar esto (el
  efecto aleatorio hace *shrinkage*), pero merece la pena mirarlo.
- **`centro_especializacion` con colegios de muestra pequeña**: el
  script avisa de cuántos colegios tienen menos de 10 alumnos con
  `lang_test` válido (el % dominante es menos fiable ahí).

### Solución de problemas de INLA

Si aparece `Segmentation fault`, `matrix is not positive definite`, o
un ajuste que tarda horas sin terminar, son los mismos problemas ya
documentados y resueltos en `../pisa-espana-ccaa/README.md` y
`../../pisa-maihda-trends/README.md` -- este script ya incorpora todas
esas correcciones (`num.threads="4:1"`, filtrado de `stu_wgt`,
`score_z` estandarizado, `int.strategy="eb"` + `cmin=0`,
`dic`/`waic`/`config=FALSE`, `control.predictor(compute=FALSE)`).

## Próximos pasos

1. ~~Ejecutar el pipeline y confirmar que no hay errores~~ -- hecho.
2. Opcional: correr `R/03b_vpc_ablation_congruencia.R` (ajusta 3
   modelos nulos adicionales, más rápido que rehacer `03` entero) para
   confirmar, dentro de las mismas 5 CCAA, cuánto del VPC de estrato se
   debe específicamente a haber añadido `congruencia_linguistica` como
   5ª dimensión -- ver `qmd/04-resultados.qmd`, que recoge este
   resultado automáticamente en cuanto exista
   `data/vpc_ablation_comparacion.rds`.
3. Pendiente, aportado por ti en paralelo: clasificación oficial del
   modelo lingüístico A/B/D en País Vasco (fuera del alcance de
   `LANGTEST_QQQ`, que solo aproxima el idioma de examen, no el modelo
   de escolarización) -- se puede incorporar como covariable adicional
   más adelante sin rehacer el resto del pipeline.
4. La estabilidad del PCV en el dominio "lectura" (sobre todo 2018)
   queda como limitación conocida sin investigar más -- ver aviso en
   `qmd/04-resultados.qmd`.
