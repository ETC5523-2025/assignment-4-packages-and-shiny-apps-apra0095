#' Plot DALYs per 100,000 by infection type
#'
#' @param data Data frame with columns `hai_type` and `dalys_per100k`.
#' @param region Character string specifying region name (e.g. "Germany" or "EU/EEA").
#' @return A ggplot object showing DALYs by infection type.
#' @examples
#' plot_tradeoff(asg4::germany_burden, "Germany")
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export
plot_tradeoff <- function(data, region = "Region") {
  ggplot2::ggplot(data, ggplot2::aes(.data$hai_type, .data$dalys_per100k)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Infection type",
      y = "DALYs per 100,000",
      title = paste(region, ": DALYs per 100,000 (2011-2012)")
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

