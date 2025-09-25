suppressPackageStartupMessages({
  library(MASS)    # polr (ordinal)
  library(stats)   # glm
  library(dplyr)
  library(purrr)
})

# Asegura tipos antes de modelar
._coerce_for_models <- function(df) {
  df <- df %>% mutate(
    # Sociotropico debe ser factor ordenado para polr
    Sociotropico = if (!is.ordered(Sociotropico)) factor(Sociotropico, ordered = TRUE) else Sociotropico,
    # voto es binaria con niveles (0,1) -> factor está bien para reportes y glm acepta numérico/0-1
    voto = if (is.factor(voto)) voto else factor(voto, levels = c(0,1),
                                                 labels = c("No apoyo a la coalición gobernante", "Apoyo a la coalición gobernante"))
  )
  df
}

# Ajusta los cuatro modelos que usas en cada base
fit_models <- function(df) {
  df <- ._coerce_for_models(df)
  
  # Fórmulas (quitamos Recuvoto como en tu script)
  f_polr_full    <- as.formula("Sociotropico ~ . - Recuvoto")
  f_polr_simple  <- as.formula("Sociotropico ~ voto")
  f_logit_full   <- as.formula("voto ~ . - Recuvoto")
  f_logit_simple <- as.formula("voto ~ Sociotropico")
  
  # Ajustes
  m_polr_full    <- polr(f_polr_full,    data = df, Hess = TRUE)
  m_polr_simple  <- polr(f_polr_simple,  data = df, Hess = TRUE)
  # Para glm, convierto voto a num 0/1 si venía factor (evita warnings raros)
  df_glm <- df %>% mutate(voto = as.numeric(voto) - 1)
  m_logit_full   <- glm(f_logit_full,   data = df_glm, family = binomial())
  m_logit_simple <- glm(f_logit_simple, data = df_glm, family = binomial())
  
  list(
    polr_full     = m_polr_full,
    polr_simple   = m_polr_simple,
    logit_full    = m_logit_full,
    logit_simple  = m_logit_simple
  )
}

# Ajusta modelos para todas las bases (tu lista de dfs)
fit_all_models <- function(dfs_named_list) {
  map(dfs_named_list, fit_models)
}

# (Opcional) Guardar modelos a disco
save_models <- function(models_named_list, dir_out = "models") {
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  purrr::iwalk(models_named_list, function(mset, name) {
    saveRDS(mset, file = file.path(dir_out, paste0("models_", name, ".rds")))
  })
}

# (Opcional) Exportar comparativas con stargazer (POLR)
export_stargazer_polr <- function(models_named_list, outfile = "model_comparison_polr.html") {
  requireNamespace("stargazer", quietly = TRUE)
  ms <- c(
    models_named_list$postelectoral_2019_apr$polr_simple,
    models_named_list$postelectoral_2019_apr$polr_full,
    models_named_list$postelectoral_2019_nov$polr_simple,
    models_named_list$postelectoral_2019_nov$polr_full,
    models_named_list$postelectoral_2023$polr_simple,
    models_named_list$postelectoral_2023$polr_full
  )
  stargazer::stargazer(ms, type = "html", out = outfile)
}

# (Opcional) Exportar comparativas con stargazer (GLM)
export_stargazer_logit <- function(models_named_list, outfile = "model_comparison_logit.html") {
  requireNamespace("stargazer", quietly = TRUE)
  ms <- c(
    models_named_list$postelectoral_2019_apr$logit_simple,
    models_named_list$postelectoral_2019_apr$logit_full,
    models_named_list$postelectoral_2019_nov$logit_simple,
    models_named_list$postelectoral_2019_nov$logit_full,
    models_named_list$postelectoral_2023$logit_simple,
    models_named_list$postelectoral_2023$logit_full
  )
  stargazer::stargazer(ms, type = "html", out = outfile)
}

