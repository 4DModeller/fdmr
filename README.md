# 4D Modeller - Spatio-temporal modeling tools for the 21st Century

This repository contains the code for the 4DModeller project.

## Quickstart

The R package is currently called `fdmr` as it's easy to write and there's currently no package with the same name that I could find.

To get 4D Modeller installed first you need to make sure you have a recent version of R installed.
The easiest way to do this is to [install RStudio](https://posit.co/downloads/).

Next clone this repository and move into the directory  

```bash
# git clone https://github.com/4DModeller/fdmr.git
# cd fdmr
```

Next, start an R session and use `renv` to install the packages required by `fdmr`.

```R
# renv::restore()
```

Next we need to install INLA, which won't be installed by default when you install the `fdmr` package.

```R
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

Next you can install the `fdmr` package from the local directory as source package.

```R
# install.packages(".", repos = NULL, type = "source")
```

This will install the `fdmr` package and its dependencies.
