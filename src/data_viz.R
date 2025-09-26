suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# -----------------------------
# Helpers
# -----------------------------
.label_month_es <- function(d) format(d, "%Y - %B")

save_plot <- function(plot, file, w = 10, h = 6, dpi = 300) {
  ggsave(filename = file, plot = plot, width = w, height = h, dpi = dpi)
}

# -----------------------------
# Gráfico 1: % que valora "Mal/Muy mal"
# Espera columnas: fecha (Date), malavaloración (num)
# -----------------------------
plot_intro_malvaloracion <- function(df, title = NULL) {
  df <- df %>% dplyr::filter(!is.na(fecha), !is.na(malavaloracion))
  ggplot(df, aes(x = fecha, y = malavaloracion)) +
    geom_line() +
    geom_point(shape = 15, size = 1) +
    scale_x_date(labels = .label_month_es, breaks = unique(df$fecha)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      plot.title   = element_text(hjust = 0.5, size = 16)
    ) +
    labs(title = title, x = "Fecha",
         y = "(%) Porcentaje que valora la economía 'Mal' o 'Muy mal'")
}

# -----------------------------
# Gráfico 2: % que señalan problema económico (total + 1er y 2º problema)
# Espera columnas: fecha, Porcentaje.total, principalproblema, segundoproblema
# -----------------------------

plot_intro_problemas <- function(df, title = NULL) {
  df <- df %>% dplyr::filter(!is.na(fecha))
  ggplot(df, aes(x = fecha, y = porcentaje_total)) +
    geom_line() +
    geom_point(shape = 15, size = 1) +
    geom_line(aes(y = principalproblema), colour = "red") +
    geom_line(aes(y = segundoproblema), linetype = "dashed") +
    scale_x_date(labels = .label_month_es, breaks = unique(df$fecha)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      plot.title   = element_text(hjust = 0.5, size = 16)
    ) +
    labs(title = title, x = "Fecha",
         y = "(%) Económica como 1º/2º problema principal")
}

