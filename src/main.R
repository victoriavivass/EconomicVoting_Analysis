suppressPackageStartupMessages({
  library(dplyr)
  source("src/data_cleaning.R")
  source("src/data_viz.R")
})

run <- function() {
  # --- 2020–2024 ---
  intro_2020_2024 <- pipeline_intro_2020_2024("data/raw/datosCIS_2020-2024.csv")
  p1 <- plot_intro_malvaloracion(intro_2020_2024, title = "2020–2024: Valoración economía")
  p2 <- plot_intro_problemas(intro_2020_2024, title = "2020–2024: Problemas económicos")
  
  # --- 2018–2019 ---
  intro_2018_2019 <- pipeline_intro_2018_2019("data/raw/datosCIS_2018_2019.csv")
  p3 <- plot_intro_malvaloracion(intro_2018_2019, title = "2018–2019: Valoración economía")
  
  if (!dir.exists("plots")) dir.create("plots")
  save_plot(p1, "plots/valoracion_2020_2024.png")
  save_plot(p2, "plots/problemas_2020_2024.png")
  save_plot(p3, "plots/valoracion_2018_2019.png")
  
  invisible(list(
    plots = list(
      val_2020_2024 = p1,
      prob_2020_2024 = p2,
      val_2018_2019 = p3
    )
  ))
}

if (interactive()) {
  out <- run()
  print(out$plots$val_2020_2024)
  print(out$plots$prob_2020_2024)
  print(out$plots$val_2018_2019)
}
