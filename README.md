# 4DModeller <img src="man/figures/logo.png" align="right" alt="" width="400" />

[![R-CMD-check](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/4DModeller/fdmr/actions/workflows/check-standard.yaml)

4DModeller is a spatio-temporal modelling package that can be applied to problems at any scale from micro to processes that operate at a global scale. It includes data visualization tools, finite element mesh building tools, Bayesian hierarchical modelling based on Bayesian inference packages [INLA](https://www.r-inla.org/home) and [inlabru](https://github.com/inlabru-org/inlabru), and model evaluation and assessment tools.

## Why should I use 4DModeller?

4DModeller has been designed to make it easy to design spatially distributed, temporally dependent statistical models. Typically, 4DModeller expects tabular data sets with spatial coordinates, time indices, and the values that change or remain constant over those times. It is designed to be used in the modelling process once data has been sufficiently organized from wherever it was gathered from.

4DModeller has a stack of tools that include [Shiny apps](https://4dmodeller.github.io/fdmr/reference/index.html#shiny-apps), [tutorials as vignettes](https://4dmodeller.github.io/fdmr/articles/) in R Markdown notebooks, and the [package itself](https://github.com/4DModeller/fdmr). These tools are designed to help you:

### - easily build finite element meshes that models can be calculated on

![](https://github.com/4DModeller/fdmr/assets/8915182/4c1d188d-3feb-471f-8831-97aa5fab4765)

### - specify priors for the model to pick the best model hyperparameters

![](https://github.com/4DModeller/fdmr/assets/8915182/30bfea5e-80d8-42d8-96e7-ff0bce22029a)

### - evaluate the fully trained model output

![](https://github.com/4DModeller/fdmr/assets/8915182/fe791f74-c9b4-4db0-a52e-ffa018b12b41)

## Quickstart

To get the 4DModeller R package `fdmr` installed first you need to make sure you have a recent version of R installed.
The easiest way to do this is to [install RStudio](https://posit.co/downloads/).

Next start an R session and run

```R
install.packages("devtools")
library(devtools)
devtools::install_github("4DModeller/fdmr")
```

You should now have `fdmr` and all its dependencies installed and you can continue on [one of our tutorials](https://4dmodeller.github.io/fdmr/articles/).

## Installation

On most systems the commands above should get you up and running. On some Linux systems we've found the need to 
install some additional libraries before `fdmr`'s dependencies can be installed.

### Ubuntu 20.04

Using a fresh Ubuntu 20.04 install we found we needed to install the C and C++ compilers and some additional libraries.
To install GCC, the GNU Compiler Collection and related tools run

```console
sudo apt-get install build-essential
```

Then install the libraries required by our dependencies

```console
sudo apt-get install libharfbuzz-dev libfribidi-dev libfreetype6-dev \
                        libpng-dev libtiff5-dev libjpeg-dev libudunits2-dev libgdal-dev
```

Note that on other Linux distributions the names of these packages may differ.