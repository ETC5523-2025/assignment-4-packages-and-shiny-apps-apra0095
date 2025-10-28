#' Germany HAI burden (representative PPS sample, medians)
#' @format A tibble with 5 rows and 5 variables:
#' \describe{
#'   \item{hai_type}{HAP, UTI, BSI, SSI, CDI}
#'   \item{cases}{Estimated cases (count)}
#'   \item{deaths}{Attributable deaths (count)}
#'   \item{dalys}{DALYs (count)}
#'   \item{dalys_per100k}{DALYs per 100,000 population}
#' }
#' @source Zacher et al., Euro Surveill 2019.
"germany_burden"

#' EU/EEA HAI burden (per 100,000 population, medians)
#' @format Same columns as \code{germany_burden}; values are per 100,000.
#' @source Zacher et al., Euro Surveill 2019.
"eu_eea_burden"

#' Metadata for BHAI-derived summaries
#' @format Two columns: \code{item}, \code{value}.
"metadata_bhai"
