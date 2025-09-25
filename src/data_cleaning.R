suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringi)
  library(tidyr)
  library(haven)
  library(forcats)
})

# =========================
# HELPERs COMUNES
# =========================

# Nombres limpios: sin acentos/ñ, minúsculas, _ como separador
.normalize_names <- function(df) {
  nm <- names(df)
  nm <- stringi::stri_trans_general(nm, "Latin-ASCII")
  nm <- tolower(nm)
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("^_|_$", "", nm)
  names(df) <- nm
  df
}

# Encuentra una columna cuyo nombre contenga TODOS los tokens dados
.find_col <- function(nm, tokens_and) {
  idx <- which(Reduce(`&`, lapply(tokens_and, function(tk) grepl(tk, nm, ignore.case = TRUE))))
  if (length(idx) == 0) return(NA_character_)
  nm[idx[1]]
}

# Parseo robusto de fechas (varios formatos comunes)
.parse_fecha_smart <- function(x) {
  suppressWarnings(parse_date_time(x, orders = c("d/m/Y","d-m-Y","Y-m-d","d.m.Y"), tz = "UTC")) |> as.Date()
}

# Corta edades en tramos
.cut_age_bands <- function(x) {
  cut(as.numeric(x),
      breaks = c(18, 30, 40, 50, 60, Inf),
      labels = c("De 18 a 30 años","De 31 a 40 años","De 41 a 50 años","De 51 a 60 años","Más de 60 años"),
      include.lowest = TRUE)
}

# =========================
# CSV (2018–2019 y 2020–2024)
# =========================

# Lector robusto: ; separador, , decimal, . miles
.read_intro_csv <- function(path_csv) {
  readr::read_delim(
    file = path_csv, delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE, trim_ws = TRUE,
    na = c("", "NA", "NaN")
  )
}

# Pipeline genérico que normaliza nombres y busca las columnas por patrones
pipeline_intro_generic <- function(path_csv) {
  raw <- .read_intro_csv(path_csv) |> .normalize_names()
  nm  <- names(raw)
  
  col_fecha   <- .find_col(nm, c("fecha"))
  col_val_mal <- .find_col(nm, c("valor", "mala"))                  # valoración mala/muy mala
  col_pct_tot <- .find_col(nm, c("porcentaje","total"))
  col_prob_1  <- .find_col(nm, c("principal","problema"))
  col_prob_2  <- .find_col(nm, c("segundo","problema"))
  
  if (is.na(col_prob_1)) col_prob_1 <- .find_col(nm, c("principal","problema","econ"))
  if (is.na(col_prob_2)) col_prob_2 <- .find_col(nm, c("segundo","problema","econ"))
  
  need_one <- function(x, label) {
    if (is.na(x)) stop(sprintf("No se encontró columna para '%s'. Nombres disponibles:\n%s",
                               label, paste(nm, collapse = ", ")))
    x
  }
  col_fecha   <- need_one(col_fecha,   "fecha")
  col_val_mal <- need_one(col_val_mal, "valoración mala/muy mala")
  
  as_num <- function(v) suppressWarnings(as.numeric(gsub("\\.", "", gsub(",", ".", v))))
  
  out <- tibble::tibble(
    fecha             = .parse_fecha_smart(raw[[col_fecha]]),
    malavaloracion    = as_num(raw[[col_val_mal]]),
    porcentaje_total  = if (!is.na(col_pct_tot)) as_num(raw[[col_pct_tot]]) else NA_real_,
    principalproblema = if (!is.na(col_prob_1))  as_num(raw[[col_prob_1]])  else NA_real_,
    segundoproblema   = if (!is.na(col_prob_2))  as_num(raw[[col_prob_2]])  else NA_real_
  ) |>
    arrange(fecha) |>
    filter(!is.na(fecha))
  out
}

pipeline_intro_2020_2024 <- function(path_csv) pipeline_intro_generic(path_csv)
pipeline_intro_2018_2019 <- function(path_csv) pipeline_intro_generic(path_csv)

# =========================
# SAV (Postelectorales 2023, 2019-nov, 2019-abr)
# =========================

# ---- 2023 (3420.sav)
load_post_2023 <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero       = d$SEXO,
    Edad         = d$EDAD,
    Sociotropico = d$ECOESP,
    Recuvoto     = d$RECUVOTOG1R,
    Educacion    = d$ESTUDIOS,
    Ingresos     = d$INGRESHOG,
    Ideologia    = d$ESCIDEOL
  )
}

clean_post_2023 <- function(df) {
  df |>
    tidyr::drop_na() |>
    mutate(
      Sociotropico = if_else(Sociotropico %in% c(8, 9), NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = if_else(Recuvoto %in% c(1, 2, 3, 21), as.numeric(Recuvoto), NA_real_),
      Ingresos     = if_else(Ingresos %in% c(8, 9), NA_real_, as.numeric(Ingresos)),
      Ideologia    = if_else(Ideologia %in% c(98, 99), NA_real_, as.numeric(Ideologia))
    ) |>
    mutate(
      Genero       = factor(Genero, levels = c(1,2), labels = c("Hombre","Mujer")),
      Edad         = as.numeric(Edad),
      Sociotropico = factor(Sociotropico, levels = c(1,2,3,4,5),
                            labels = c("Muy buena","Buena","Regular","Mala","Muy mala")),
      Recuvoto     = factor(Recuvoto, levels = c(1,2,3,21), labels = c("PSOE","PP","VOX","Sumar")),
      Ingresos     = factor(Ingresos, levels = c(1,2,3,4,5,6),
                            labels = c("Más de 5.000 Euros","De 3.901 a 5.000 Euros",
                                       "De 2.701 a 3.900 Euros","De 1.801 a 2.700 Euros",
                                       "De 1.100 a 1.800 Euros","Menos de 1.100 Euros")),
      Ingresos     = forcats::fct_relevel(Ingresos, "Menos de 1.100 Euros"),
      Educacion    = factor(dplyr::case_when(
        Educacion %in% c(1,2) ~ "Educación Básica o Ninguna",
        Educacion %in% c(3,4,5) ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion == 6 ~ "Educación Superior",
        TRUE ~ NA_character_
      ),
      levels = c("Educación Básica o Ninguna",
                 "Educación Secundaria y Formación Profesional(FP)",
                 "Educación Superior")),
      Edad         = .cut_age_bands(Edad)
    ) |>
    mutate(
      voto = case_when(
        Recuvoto %in% c("PSOE","Sumar") ~ 1,
        Recuvoto %in% c("PP","VOX")     ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) |>
    tidyr::drop_na()
}

pipeline_post_2023 <- function(path_sav) load_post_2023(path_sav) |> clean_post_2023()

# ---- 2019 NOV (3269.sav)
load_post_2019_nov <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero       = d$C9,
    Edad         = d$C10,
    Sociotropico = d$A1,
    Recuvoto     = d$B22,
    Educacion    = d$ESTUDIOS,
    Ingresos     = d$C20,
    Ideologia    = d$C3
  )
}

clean_post_2019_nov <- function(df) {
  df |>
    mutate(
      Ideologia    = if_else(Ideologia %in% c(98,99), NA_real_, as.numeric(Ideologia)),
      Sociotropico = if_else(Sociotropico %in% c(8,9),  NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = if_else(Recuvoto %in% c(1,2,4,18,21,50,6,67), as.numeric(Recuvoto), NA_real_),
      Ingresos     = if_else(Ingresos %in% c(99,1), NA_real_, as.numeric(Ingresos))
    ) |>
    mutate(
      Sociotropico = factor(Sociotropico, levels = c(1,2,3,4,5),
                            labels = c("Muy buena","Buena","Regular","Mala","Muy mala")),
      Educacion = factor(case_when(
        Educacion %in% c(1,2) ~ "Educación Básica o Ninguna",
        Educacion %in% c(3,4,5) ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion == 6 ~ "Educación Superior",
        TRUE ~ NA_character_
      ),
      levels = c("Educación Básica o Ninguna",
                 "Educación Secundaria y Formación Profesional(FP)",
                 "Educación Superior")),
      Edad    = .cut_age_bands(Edad),
      Genero  = factor(Genero, levels = c(1,2), labels = c("Hombre","Mujer")),
      Recuvoto = factor(Recuvoto,
                        levels = c(1,2,4,18,21,6,67),
                        labels = c("PP","PSOE","C'S","VOX","Unidas Podemos","En Comú Podem","En Común-Unidas Podemos"))
    ) |>
    mutate(
      Ingresos = case_when(
        Ingresos %in% c(10,11) ~ "Menos de 1.100 Euros",
        Ingresos == 9          ~ "De 1.100 a 1.800 Euros",
        Ingresos %in% c(7,8)   ~ "De 1.801 a 2.700 Euros",
        Ingresos == 6          ~ "De 2.701 a 3.900 Euros",
        Ingresos %in% c(2,3,4,5) ~ "De 3901 a 5.000 Euros",
        TRUE                   ~ "Más de 5.000 Euros"
      ),
      Ingresos = factor(Ingresos,
                        levels = c("Más de 5.000 Euros","De 3901 a 5.000 Euros",
                                   "De 2.701 a 3.900 Euros","De 1.801 a 2.700 Euros",
                                   "De 1.100 a 1.800 Euros","Menos de 1.100 Euros")),
      Ingresos = forcats::fct_relevel(Ingresos, "Menos de 1.100 Euros")
    ) |>
    mutate(
      voto = case_when(
        Recuvoto %in% c("PSOE","Unidas Podemos","En Comú Podem","En Común-Unidas Podemos") ~ 1,
        Recuvoto %in% c("PP","VOX","C'S") ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) |>
    tidyr::drop_na()
}

pipeline_post_2019_nov <- function(path_sav) load_post_2019_nov(path_sav) |> clean_post_2019_nov()

# ---- 2019 ABR (3248.sav)
load_post_2019_apr <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero       = d$P41,
    Edad         = d$P42,
    Sociotropico = d$P6,
    Recuvoto     = d$P39A,
    Educacion    = d$P47A,
    Ingresos     = d$P54,
    Ideologia    = d$P32
  )
}

clean_post_2019_apr <- function(df) {
  df |>
    tidyr::drop_na() |>
    mutate(
      Sociotropico = if_else(Sociotropico %in% c(8,9), NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = if_else(Recuvoto %in% c(1,2,3,4,5,6,18), as.numeric(Recuvoto), NA_real_),
      Ingresos     = if_else(Ingresos %in% c(99,99), NA_real_, as.numeric(Ingresos)),
      Ideologia    = if_else(Ideologia %in% c(98,99), NA_real_, as.numeric(Ideologia))
    ) |>
    mutate(
      Educacion = factor(case_when(
        Educacion %in% c(1,2,3,4,5,7,8) ~ "Educación Básica o Ninguna",
        Educacion %in% c(9:22)          ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion %in% c(23,24,25,26)   ~ "Educación Superior",
        TRUE ~ NA_character_
      ),
      levels = c("Educación Básica o Ninguna",
                 "Educación Secundaria y Formación Profesional(FP)",
                 "Educación Superior")),
      Edad         = .cut_age_bands(Edad),
      Genero       = factor(Genero, levels = c(1,2), labels = c("Hombre","Mujer")),
      Sociotropico = factor(Sociotropico, levels = c(2,3,4,5),
                            labels = c("Buena","Regular","Mala","Muy mala")),
      Recuvoto     = factor(Recuvoto, levels = c(1,2,3,4,5,6,18),
                            labels = c("PP","PSOE","Podemos","Ciudadanos","IU","En Comú Podem","VOX"))
    ) |>
    mutate(
      Ingresos = as.numeric(Ingresos),
      Ingresos = case_when(
        Ingresos %in% c(1,2,3,4) ~ "Menos de 1.100 Euros",
        Ingresos %in% c(5,6)     ~ "De 1.100 a 1.800 Euros",
        Ingresos == 7            ~ "De 1.801 a 2.700 Euros",
        Ingresos %in% c(8,9)     ~ "De 2.701 a 3.900 Euros",
        Ingresos == 10           ~ "De 3.901 a 5.000 Euros",
        Ingresos == 11           ~ "Más de 5.000 Euros",
        Ingresos %in% c(98,99)   ~ NA_character_,
        TRUE ~ NA_character_
      ),
      Ingresos = factor(Ingresos,
                        levels = c("Menos de 1.100 Euros","De 1.100 a 1.800 Euros",
                                   "De 1.801 a 2.700 Euros","De 2.701 a 3.900 Euros",
                                   "De 3.901 a 5.000 Euros","Más de 5.000 Euros")),
      Ingresos = forcats::fct_relevel(Ingresos, "Menos de 1.100 Euros")
    ) |>
    mutate(
      voto = case_when(
        Recuvoto %in% c("PSOE","Podemos","En Comú Podem","IU") ~ 1,
        Recuvoto %in% c("PP","VOX","Ciudadanos") ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) |>
    tidyr::drop_na()
}

pipeline_post_2019_apr <- function(path_sav) load_post_2019_apr(path_sav) |> clean_post_2019_apr()
