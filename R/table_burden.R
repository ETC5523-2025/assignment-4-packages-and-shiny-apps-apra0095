#' Build a formatted burden table
#' @param data Tibble with hai_type, cases, deaths, dalys, dalys_per100k.
#' @return A gt table.
#' @export
table_burden <- function(data) {
  need <- c("hai_type","cases","deaths","dalys","dalys_per100k")
  for (nm in need) if (!nm %in% names(data)) data[[nm]] <- NA_real_
  data <- data[, need, drop = FALSE]

  gt::gt(data) |>
    gt::cols_label(
      hai_type = "Infection type",
      cases = "Cases",
      deaths = "Deaths",
      dalys = "DALYs",
      dalys_per100k = "DALYs per 100k"
    ) |>
    gt::fmt_number(columns = c("cases", "deaths", "dalys", "dalys_per100k"),
                   decimals = 1) |>
    gt::fmt_missing(gt::everything(), missing_text = "-")

}
