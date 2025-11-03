# asg4 — Germany vs EU/EEA HAI Burden (2011–2012)

This package contains tidy, documented datasets and a Shiny app to explore the DALY burden of five healthcare-associated infections for Germany vs the EU/EEA (2011–2012). Data are medians from Zacher et al. (2019) computed with the BHAI R package.

## Documentation

**Site:** <https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-apra0095/>

[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-apra0095/)

## Install Package

``` r
# Install from GitHub (requires remotes)
install.packages("remotes")
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-apra0095")

# Load and run the Shiny app
library(asg4)
launch_app()
```

## Example of Usage
``` r
library(asg4)

combined <- asg4::germany_burden |>
  select(hai_type, dalys_per100k) |>
  mutate(country = "Germany") |>
  bind_rows(asg4::eu_eea_burden %>%
              mutate(country = "EU/EEA"))

ggplot(combined, aes(x = hai_type, y = dalys_per100k, fill = country)) +
  geom_col(position = "dodge") +
  labs(
    x = "Infection type",
    y = "DALYs per 100,000",
    title = "Comparison of Germany vs EU/EEA DALY burden (2011–2012)",
    fill = "Region"
  ) +
  theme_minimal(base_size = 13)

```

## Interactive Shiny Apps

<https://apra0095.shinyapps.io/asg4-bhai-germany/>
