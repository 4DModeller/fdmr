# 4DModeller <img src="man/figures/logo.png" align="right" alt="" width="400" />

[![R-CMD-check](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml)

4DModeller is a spatio-temporal modeling library that can be applied to problems at any scale from micro to processes that operate at a global scale. It includes data visualization tools, finite element mesh building tools, Bayesian hierarchical modeling based on Bayesian inference packages INLA and inlabru, and model evaluation and assessment tools.

## Why should I use 4DModeller?

4DModeller is an R toolbox that has been designed to make it easy to design spatially distributed, temporally dependent statistical models. Typically, 4DModeller expects tabular data sets with spatial coordinates, time indices, and the values that change or remain constant over those times. It is designed to be used in the modeling process once data has been sufficiently organized from wherever it was gathered from.

4DModeller has a stack of tools that include shiny apps, vignettes in R-markdown notebooks, and the library itself. These tools are designed to help you easily build finite element meshes that models can be calculated on:

https://github.com/4DModeller/fdmr/assets/8915182/b1dc1fc7-3340-4915-8de6-319a29ca7a89

tools for how to specify priors for the model to pick the best model hyperparameters:

https://github.com/4DModeller/fdmr/assets/8915182/678fdb23-83c0-4609-bc54-0d0c874264fa

and tools for evaluating the fully trained model output:

https://github.com/4DModeller/fdmr/assets/8915182/8e2bf77e-064b-421c-ac96-e5c45ad9cf5e

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
