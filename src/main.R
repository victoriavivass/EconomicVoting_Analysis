suppressPackageStartupMessages({
  library(dplyr)
  source("src/data_cleaning.R")
  # source("src/data_viz.R")
  # source("src/modeling.R")
})

run <- function() {
  df_2023     <- pipeline_post_2023("data/raw/3420.sav")
  df_2019_nov <- pipeline_post_2019_nov("data/raw/3269.sav")
  df_2019_apr <- pipeline_post_2019_apr("data/raw/3248.sav")
  
  invisible(list(
    postelectoral_2023 = df_2023,
    postelectoral_2019_nov = df_2019_nov,
    postelectoral_2019_apr = df_2019_apr
  ))
}