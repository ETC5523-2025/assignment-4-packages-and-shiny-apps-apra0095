# asg4 — Germany vs EU/EEA HAI Burden (2011–2012)
This package ships tidy, documented datasets and a Shiny app to explore the DALY burden of five healthcare-associated infections for Germany vs the EU/EEA (2011–2012).
Data are medians from Zacher et al. (2019) computed with the BHAI R package.

## Documentation

**Site:** https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-apra0095/

[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-apra0095/)


## Install Package

```r
# from GitHub (remotes required)
install.packages("remotes")
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-apra0095")
library(asg4)
launch_app()
