#' Plot DALYs per 100,000 by infection type
#' @param dataset Data frame of hai_type and dalys_per100k (germany_burden or eu_eea_burden)
#' @param country Title label ("Germany" or "EU/EEA")
#' @export
plot_tradeoff <- function(dataset = germany_burden, country = "Germany") {
  stopifnot(all(c("hai_type","dalys_per100k") %in% names(dataset)))
  ggplot2::ggplot(dataset, ggplot2::aes(hai_type, dalys_per100k)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = paste0(country, ": DALYs per 100,000 (2011–2012)"),
      x = "Infection type", y = "DALYs per 100,000"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

