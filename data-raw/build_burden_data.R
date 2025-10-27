library(dplyr)

# Germany (representative PPS sample, medians)
germany_burden <- tibble::tribble(
  ~hai_type, ~cases, ~deaths, ~dalys, ~dalys_per100k,
  "HAP", 106586, 3968, 69508, 86.1,
  "UTI", 214150, 3664, 66701, 82.6,
  "BSI", 26976, 3905, 58350, 72.2,
  "SSI", 93222, 2328, 28842, 35.7,
  "CDI", 36002, 1917, 20890, 25.9
)

#EU/EEA average (ECDC PPS 2011–2012) — DALYs per 100k
eu_eea_burden <- tibble::tribble(
  ~hai_type, ~cases, ~deaths, ~dalys, ~dalys_per100k,
  "HAP", 143.7, 5.3, 109.8, 109.8,
  "UTI", 174.7, 3.0,  57.1,  57.1,
  "BSI",  22.2, 3.3,  76.2,  76.2,
  "SSI", 111.3, 2.6,  35.1,  35.1,
  "CDI",  16.0, 0.9,  10.0,  10.0
)
# note:
# The EU/EEA data are per 100k population, so “cases/deaths/dalys” are expressed per 100k not absolute counts like Germany.

# Metadata
metadata_bhai <- tibble::tibble(
  item  = c("Source","Survey period","Method","Assumption"),
  value = c(
    "Zacher et al., Euro Surveill 2019 (doi:10.2807/1560-7917.ES.2019.24.46.1900135)",
    "ECDC Point Prevalence Survey 2011–2012",
    "BHAI R package; incidence-based DALY estimation",
    "Applied EU/EEA McCabe distribution as proxy for Germany"
  )
)

usethis::use_data(germany_burden, eu_eea_burden, metadata_bhai, overwrite = TRUE)

