# Changelog

All notable changes to `fdmr` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- This changelog - [PR #99](https://github.com/4DModeller/fdmr/pull/99)
- Added an INLA FAQ vignette that covers INLA crashes we have encountered in the past - [PR #116](https://github.com/4DModeller/fdmr/pull/116)
- Building a mesh with the Tibetan lakes data and the `mesh_builder` Shiny app to the [mesh building tutorial](https://4dmodeller.github.io/fdmr/articles/meshbuilder.html) - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Clearer documentation on types expected by the `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Checks on the data types being passed into the `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Ability to plot either polygon or point data on Leaflet map of `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)

### Fixed

- Read of `mean` twice from INLA model output instead of reading `sd` - [PR #121](https://github.com/4DModeller/fdmr/pull/121)

### Changed

- Use of `rgdal` functions migrated to `sf` equivalents due to [`rgdal` retirement](https://r-spatial.org/r/2022/04/12/evolution.html) - [PR #145](https://github.com/4DModeller/fdmr/pull/145)
- Move to use `utils::untar` instead of `archive::extract_archive` due to issues on some Linux systems - [PR #101](https://github.com/4DModeller/fdmr/pull/130)

### Removed
