suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(haven)
  library(lubridate)
  library(stringr)
  library(forcats)
})

# =========================
# Helpers comunes
# =========================

cut_age_bands <- function(x) {
  cut(
    as.numeric(x),
    breaks = c(18, 30, 40, 50, 60, Inf),
    labels = c("De 18 a 30 años", "De 31 a 40 años", "De 41 a 50 años", "De 51 a 60 años", "Más de 60 años"),
    include.lowest = TRUE
  )
}

# =========================
# 0) Series de introducción (CSV 2020-2024 y 2018-2019)
# =========================

load_intro_2020_2024 <- function(path_csv) {
  readr::read_csv2(path_csv, show_col_types = FALSE) %>%
    rename(
      "malavaloración"   = "Valorecon_malaomuy.mala",
      "principalproblema" = "Principal_Problema_País_crisiseconómica",
      "segundoproblema"   = "Segundo_Problema_crisiseconomica",
      "fecha"             = "...Fecha"
    )
}

clean_intro_2020_2024 <- function(df) {
  # Si alguna columna no existe por nombre exacto (acentos/ñ), intenta alternativas
  if (!"malavaloración" %in% names(df) && "malavaloración" %in% iconv(names(df), to = "ASCII//TRANSLIT")) {
    names(df) <- iconv(names(df), to = "ASCII//TRANSLIT")
  }
  df %>%
    tidyr::drop_na() %>%
    mutate(
      fecha = as.Date(fecha, format = "%d/%m/%Y"),
      etiquetas_fechas = format(fecha, "%Y - %B")
    )
}

load_intro_2018_2019 <- function(path_csv) {
  readr::read_csv2(path_csv, show_col_types = FALSE)
}

clean_intro_2018_2019 <- function(df) {
  df %>%
    rename(Valoracion = Valorecon_malaomuy.mala) %>%
    mutate(
      fecha = as.Date(Fecha, format = "%d/%m/%Y"),
      etiquetas_fechas = format(fecha, "%Y - %B")
    )
}

# =========================
# 1) Postelectoral 2023 (3420.sav)
# =========================

load_post_2023 <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero      = d$SEXO,
    Edad        = d$EDAD,
    Sociotropico= d$ECOESP,
    Recuvoto    = d$RECUVOTOG1R,
    Educacion   = d$ESTUDIOS,
    Ingresos    = d$INGRESHOG,
    Ideologia   = d$ESCIDEOL
  )
}

clean_post_2023 <- function(df) {
  df %>%
    tidyr::drop_na() %>%  # partías con na.omit
    mutate(
      # Reemplazos a NA para NS/NC y filtros de valores válidos
      Sociotropico = dplyr::if_else(Sociotropico %in% c(8, 9), NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = dplyr::if_else(Recuvoto %in% c(1, 2, 3, 21), as.numeric(Recuvoto), NA_real_),
      Ingresos     = dplyr::if_else(Ingresos %in% c(8, 9), NA_real_, as.numeric(Ingresos)),
      Ideologia    = dplyr::if_else(Ideologia %in% c(98, 99), NA_real_, as.numeric(Ideologia))
    ) %>%
    mutate(
      Genero       = factor(Genero, levels = c(1, 2), labels = c("Hombre", "Mujer")),
      Edad         = as.numeric(Edad),
      Sociotropico = factor(Sociotropico, levels = c(1,2,3,4,5),
                            labels = c("Muy buena","Buena","Regular","Mala","Muy mala")),
      Recuvoto     = factor(Recuvoto, levels = c(1,2,3,21), labels = c("PSOE","PP","VOX","Sumar")),
      Ingresos     = factor(Ingresos, levels = c(1,2,3,4,5,6),
                            labels = c("Más de 5.000 Euros","De 3.901 a 5.000 Euros",
                                       "De 2.701 a 3.900 Euros","De 1.801 a 2.700 Euros",
                                       "De 1.100 a 1.800 Euros","Menos de 1.100 Euros"))
    ) %>%
    mutate(Ingresos = forcats::fct_relevel(Ingresos, "Menos de 1.100 Euros")) %>%
    mutate(
      Educacion = factor(dplyr::case_when(
        Educacion %in% c(1, 2) ~ "Educación Básica o Ninguna",
        Educacion %in% c(3, 4, 5) ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion == 6 ~ "Educación Superior",
        TRUE ~ NA_character_
      ), levels = c("Educación Básica o Ninguna",
                    "Educación Secundaria y Formación Profesional(FP)",
                    "Educación Superior"))
    ) %>%
    mutate(
      Edad = cut_age_bands(Edad)
    ) %>%
    mutate(
      voto = dplyr::case_when(
        Recuvoto %in% c("PSOE","Sumar") ~ 1,
        Recuvoto %in% c("PP","VOX") ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) %>%
    tidyr::drop_na()
}

# =========================
# 2) Postelectoral 2019 NOV (3269.sav)
# =========================

load_post_2019_nov <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero      = d$C9,
    Edad        = d$C10,
    Sociotropico= d$A1,
    Recuvoto    = d$B22,
    Educacion   = d$ESTUDIOS,
    Ingresos    = d$C20,
    Ideologia   = d$C3
  )
}

clean_post_2019_nov <- function(df) {
  df %>%
    mutate(
      Ideologia    = if_else(Ideologia %in% c(98, 99), NA_real_, as.numeric(Ideologia)),
      Sociotropico = if_else(Sociotropico %in% c(8, 9), NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = if_else(Recuvoto %in% c(1, 2, 4, 18, 21, 50, 6, 67), as.numeric(Recuvoto), NA_real_),
      Ingresos     = if_else(Ingresos %in% c(99, 1), NA_real_, as.numeric(Ingresos))
    ) %>%
    mutate(
      Sociotropico = factor(Sociotropico, levels = c(1,2,3,4,5),
                            labels = c("Muy buena","Buena","Regular","Mala","Muy mala")),
      Educacion = factor(case_when(
        Educacion %in% c(1, 2) ~ "Educación Básica o Ninguna",
        Educacion %in% c(3, 4, 5) ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion == 6 ~ "Educación Superior",
        TRUE ~ NA_character_
      ), levels = c("Educación Básica o Ninguna",
                    "Educación Secundaria y Formación Profesional(FP)",
                    "Educación Superior")),
      Edad    = cut_age_bands(Edad),
      Genero  = factor(Genero, levels = c(1,2), labels = c("Hombre","Mujer")),
      Recuvoto = factor(Recuvoto,
                        levels = c(1,2,4,18,21,6,67),
                        labels = c("PP","PSOE","C'S","VOX","Unidas Podemos","En Comú Podem","En Común-Unidas Podemos"))
    ) %>%
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
    ) %>%
    mutate(
      voto = case_when(
        Recuvoto %in% c("PSOE","Unidas Podemos","En Comú Podem","En Común-Unidas Podemos") ~ 1,
        Recuvoto %in% c("PP","VOX","C'S") ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) %>%
    tidyr::drop_na()
}

# =========================
# 3) Postelectoral 2019 ABR (3248.sav)
# =========================

load_post_2019_apr <- function(path_sav) {
  d <- haven::read_sav(path_sav)
  data.frame(
    Genero      = d$P41,
    Edad        = d$P42,
    Sociotropico= d$P6,
    Recuvoto    = d$P39A,
    Educacion   = d$P47A,
    Ingresos    = d$P54,
    Ideologia   = d$P32
  )
}

clean_post_2019_apr <- function(df) {
  df %>%
    tidyr::drop_na() %>%   # había NA al importar
    mutate(
      Sociotropico = if_else(Sociotropico %in% c(8,9), NA_real_, as.numeric(Sociotropico)),
      Recuvoto     = if_else(Recuvoto %in% c(1,2,3,4,5,6,18), as.numeric(Recuvoto), NA_real_),
      Ingresos     = if_else(Ingresos %in% c(99,99), NA_real_, as.numeric(Ingresos)),
      Ideologia    = if_else(Ideologia %in% c(98,99), NA_real_, as.numeric(Ideologia))
    ) %>%
    mutate(
      Educacion = factor(case_when(
        Educacion %in% c(1,2,3,4,5,7,8) ~ "Educación Básica o Ninguna",
        Educacion %in% c(9:22)          ~ "Educación Secundaria y Formación Profesional(FP)",
        Educacion %in% c(23,24,25,26)   ~ "Educación Superior",
        TRUE ~ NA_character_
      ), levels = c("Educación Básica o Ninguna",
                    "Educación Secundaria y Formación Profesional(FP)",
                    "Educación Superior")),
      Edad = cut_age_bands(Edad)
    ) %>%
    mutate(
      Genero       = factor(Genero, levels = c(1,2), labels = c("Hombre","Mujer")),
      Sociotropico = factor(Sociotropico, levels = c(2,3,4,5),
                            labels = c("Buena","Regular","Mala","Muy mala")),
      Recuvoto     = factor(Recuvoto, levels = c(1,2,3,4,5,6,18),
                            labels = c("PP","PSOE","Podemos","Ciudadanos","IU","En Comú Podem","VOX"))
    ) %>%
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
    ) %>%
    mutate(
      voto = case_when(
        Recuvoto %in% c("PSOE","Podemos","En Comú Podem","IU") ~ 1,
        Recuvoto %in% c("PP","VOX","Ciudadanos") ~ 0,
        TRUE ~ NA_real_
      ),
      voto = factor(voto, levels = c(0,1),
                    labels = c("No apoyo a la coalición gobernante","Apoyo a la coalición gobernante"))
    ) %>%
    tidyr::drop_na()
}

# =========================
# Pipelines (listas para usar en main.R)
# =========================

pipeline_intro_2020_2024 <- function(path_csv) {
  load_intro_2020_2024(path_csv) |> clean_intro_2020_2024()
}

pipeline_intro_2018_2019 <- function(path_csv) {
  load_intro_2018_2019(path_csv) |> clean_intro_2018_2019()
}

pipeline_post_2023 <- function(path_sav) {
  load_post_2023(path_sav) |> clean_post_2023()
}

pipeline_post_2019_nov <- function(path_sav) {
  load_post_2019_nov(path_sav) |> clean_post_2019_nov()
}

pipeline_post_2019_apr <- function(path_sav) {
  load_post_2019_apr(path_sav) |> clean_post_2019_apr()
}

