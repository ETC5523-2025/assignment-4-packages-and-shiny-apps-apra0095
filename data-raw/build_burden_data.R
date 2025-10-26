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
  ~hai_type, ~dalys_per100k,
  "HAP", 109.8,
  "UTI", 57.1,
  "BSI", 76.2,
  "SSI", 35.1,
  "CDI", 10.0
)

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

