# 4D Modeller - Spatio-temporal modeling tools for Bayesian Hierarchical Models

4D-Modeller is a spatio-temporal modeling library that can be applied to problems from micro to mesoscale. It includes data visualization tools, finite element mesh building tools, bayesian hierarchical modeling through INLA and inlabru, and model evaluation and assessment tools.

## Quickstart

To get 4DModeller R package `fdmr` installed first you need to make sure you have a recent version of R installed.
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
