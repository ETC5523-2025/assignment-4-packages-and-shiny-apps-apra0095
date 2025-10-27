#' Present burden table (Germany by default)
#' @param dataset Data frame to display (default: germany_burden)
#' @export
table_burden <- function(dataset = germany_burden) {
  gt::gt(dataset)
}
