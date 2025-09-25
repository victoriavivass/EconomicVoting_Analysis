suppressPackageStartupMessages({
  library(dplyr)
  source("src/data_cleaning.R")
  source("src/modeling.R")   # ⬅️ añadimos modeling
  # source("src/data_viz.R") # después
})

run <- function() {
  # Carga + limpieza (usa tus ficheros locales en data/raw/)
  df_2023     <- pipeline_post_2023("data/raw/3420.sav")
  df_2019_nov <- pipeline_post_2019_nov("data/raw/3269.sav")
  df_2019_apr <- pipeline_post_2019_apr("data/raw/3248.sav")
  
  dfs <- list(
    postelectoral_2023     = df_2023,
    postelectoral_2019_nov = df_2019_nov,
    postelectoral_2019_apr = df_2019_apr
  )
  
  # Ajuste de modelos
  models <- fit_all_models(dfs)
  
  # (Opcional) guarda a disco
  # save_models(models, dir_out = "models")
  
  invisible(list(dfs = dfs, models = models))
}

if (interactive()) {
  out <- run()
  # Echa un ojo rápido:
  lapply(out$models, function(mm) lapply(mm, function(m) {
    c(class = class(m)[1], AIC = tryCatch(AIC(m), error = function(e) NA))
  }))
}
