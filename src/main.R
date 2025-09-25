suppressPackageStartupMessages({
  library(dplyr)
  source("src/data_cleaning.R")  # CSV y SAV pipelines
  source("src/data_viz.R")       # solo gráficos para CSV
  source("src/modeling.R")     # modelos
})

# Ejecuta todo el flujo. Devuelve listas con data.frames y (opcional) modelos.
run <- function() {
  # ---------- CSV: limpieza + visualización ----------
  intro_2020_2024 <- pipeline_intro_2020_2024("data/raw/datosCIS_2020-2024.csv")
  intro_2018_2019 <- pipeline_intro_2018_2019("data/raw/datosCIS_2018_2019.csv")
  
  p_val_2020_2024 <- plot_intro_malvaloracion(intro_2020_2024, title = "2020–2024: Valoración economía")
  p_prob_2020_2024 <- plot_intro_problemas(intro_2020_2024, title = "2020–2024: Problemas económicos")
  
  p_val_2018_2019 <- plot_intro_malvaloracion(intro_2018_2019, title = "2018–2019: Valoración economía")
  has_probs_1819 <- all(c("porcentaje_total","principalproblema","segundoproblema") %in% names(intro_2018_2019))
  p_prob_2018_2019 <- if (has_probs_1819) plot_intro_problemas(intro_2018_2019, title = "2018–2019: Problemas económicos") else NULL
  
  if (!dir.exists("plots")) dir.create("plots")
  save_plot(p_val_2020_2024,  "plots/valoracion_2020_2024.png")
  save_plot(p_prob_2020_2024, "plots/problemas_2020_2024.png")
  save_plot(p_val_2018_2019,  "plots/valoracion_2018_2019.png")
  if (!is.null(p_prob_2018_2019)) save_plot(p_prob_2018_2019, "plots/problemas_2018_2019.png")
  
  # ---------- SAV: solo limpieza (modelos opcionales) ----------
  df_2023     <- pipeline_post_2023("data/raw/3420.sav")
  df_2019_nov <- pipeline_post_2019_nov("data/raw/3269.sav")
  df_2019_apr <- pipeline_post_2019_apr("data/raw/3248.sav")
  
  source("src/modeling.R")
  models <- fit_all_models(list(
    postelectoral_2023     = df_2023,
    postelectoral_2019_nov = df_2019_nov,
    postelectoral_2019_apr = df_2019_apr
    ))
  
  invisible(list(
    csv  = list(intro_2020_2024 = intro_2020_2024, intro_2018_2019 = intro_2018_2019),
    sav  = list(post_2023 = df_2023, post_2019_nov = df_2019_nov, post_2019_apr = df_2019_apr),
    plots = list(
      val_2020_2024 = p_val_2020_2024,
      prob_2020_2024 = p_prob_2020_2024,
      val_2018_2019 = p_val_2018_2019,
      prob_2018_2019 = p_prob_2018_2019
    )
    # , models = models
  ))
}

# Uso interactivo en RStudio: solo pulsa Source o ejecuta:
if (interactive()) {
  out <- run()
  # Vista rápida:
  str(out$csv, max.level = 1)
}

# Uso por terminal (opcional):
# Rscript src/main.R

