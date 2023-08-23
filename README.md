# 4DModeller <img src="man/figures/logo.png" align="right" alt="" width="400" />

[![R-CMD-check](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml)

4D-Modeller is a spatio-temporal modeling library that can be applied to problems at any scale from micro to processes that operate at a global scale. It includes data visualization tools, finite element mesh building tools, Bayesian hierarchical modeling based on Bayesian inference packages INLA and inlabru, and model evaluation and assessment tools.

## Quickstart

To get the 4DModeller R package `fdmr` installed first you need to make sure you have a recent version of R installed.
The easiest way to do this is to [install RStudio](https://posit.co/downloads/).

Next start an R session and run

```R
install.packages("devtools")
library(devtools)
devtools::install_github("4DModeller/fdmr")
```

Next we need to install INLA which is not available on CRAN.

```R
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

You should now have `fdmr` and all its dependencies installed and you can continue on [one of our tutorials](https://4dmodeller.github.io/fdmr/articles/).
