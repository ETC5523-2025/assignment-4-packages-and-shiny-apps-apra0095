#' Germany HAI burden (2011–2012, PPS representative sample)
#'
#' Median cases, attributable deaths, DALYs, and DALYs per 100,000 by infection type.
#' Values mirror Zacher et al. (2019) using the ECDC PPS 2011–2012 and BHAI methodology.
#' @format A tibble with 5 rows x 5 columns:
#' \describe{
#'   \item{hai_type}{Infection type: HAP, UTI, BSI, SSI, CDI}
#'   \item{cases}{Estimated annual cases (median)}
#'   \item{deaths}{Attributable deaths (median)}
#'   \item{dalys}{DALYs (median)}
#'   \item{dalys_per100k}{DALYs per 100,000 population (median)}
#' }
"germany_burden"

#' EU/EEA burden (DALYs per 100,000) by infection type
"eu_eea_burden"

#' Metadata and key assumptions (provenance, method, McCabe proxy)
"metadata_bhai"
