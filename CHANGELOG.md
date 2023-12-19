# Changelog

All notable changes to `fdmr` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased](https://github.com/openghg/openghg/compare/0.1.1...HEAD)

### Fixed

- Added a function to ensure correct polygon display across the dateline - [PR #285](https://github.com/4DModeller/fdmr/pull/285)

### Added

- Added a new tutorial on loading data from different sources - [PR #266](https://github.com/4DModeller/fdmr/pull/266/)
- Added new geophysical processes tutorial thanks to Alexander Minakov (4minakov) - [PR #257](https://github.com/4DModeller/fdmr/pull/257)
- Added mouse pointer coordinates header and standard measurement tool - [PR #260](https://github.com/4DModeller/fdmr/pull/260)
- New plotting function `plot_map_mapview` to use `mapview` to plot raster and spatial data - [#291](https://github.com/4DModeller/fdmr/pull/291)

### Changed

- Updated `plot_map` to allow use of both `leaflet` and `mapview` packages - [#291](https://github.com/4DModeller/fdmr/pull/291)

### Changed

- Moved from using `leaflet` to using (`mapview`)[https://r-spatial.github.io/mapview/index.html] for plotting the mesh and spatial data in the `mesh_builder` Shiny app. This enables use of UTM coordinates - [PR #288](https://github.com/4DModeller/fdmr/pull/288)

## [0.1.1] - 2023-11-01

### Added

- Added the ability to select a custom range for the colour map - [PR #218](https://github.com/4DModeller/fdmr/pull/218)

### Fixed

- Added check for `time_variable` being passed into model builder Shiny app - [PR #216](https://github.com/4DModeller/fdmr/pull/216)
- Clear button not clearing all checkboxes in model builder Shiny app - [PR #215](https://github.com/4DModeller/fdmr/pull/215)
- Corrected code in code pane of model builder Shiny app - [PR #208](https://github.com/4DModeller/fdmr/pull/208)
- Code displayed in the Code tab of the mesh builder Shiny app couldn't be copied and pasted and used - [PR #233](https://github.com/4DModeller/fdmr/pull/233)

### Changed

- Allowed reversal of colour palette and added raster plot legend in map plotter - [PR #229](https://github.com/4DModeller/fdmr/pull/229)
- Added markers option to mesh plotter - [PR #230](https://github.com/4DModeller/fdmr/pull/230)
- Updated the interfaces of the Shiny apps to the use [bslib](https://rstudio.github.io/bslib/index.html) theming - [PR #236](https://github.com/4DModeller/fdmr/pull/236)
- Code tab added to `fdmr::model_viewer` so the user can easily reproduce plots - [PR #237](https://github.com/4DModeller/fdmr/pull/237)

## [0.1.0] - 2023-10-17

### Added

- This changelog - [PR #99](https://github.com/4DModeller/fdmr/pull/99)
- Added an INLA FAQ vignette that covers INLA crashes we have encountered in the past - [PR #116](https://github.com/4DModeller/fdmr/pull/116)
- Building a mesh with the Tibetan lakes data and the `mesh_builder` Shiny app to the [mesh building tutorial](https://4dmodeller.github.io/fdmr/articles/meshbuilder.html) - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Clearer documentation on types expected by the `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Checks on the data types being passed into the `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- Ability to plot either polygon or point data on Leaflet map of `mesh_builder` tool - [PR #101](https://github.com/4DModeller/fdmr/pull/101)
- The ability to plot model predictions on a `leaflet` map in the our [Model builder Shiny app](https://4dmodeller.github.io/fdmr/articles/modelbuilder.html) - [PR #147](https://github.com/4DModeller/fdmr/pull/147)
- A new Shiny app to parse and plot INLA model output, letting users easily view model parameters and predictions on a map - [PR #158](https://github.com/4DModeller/fdmr/pull/158)

### Fixed

- Read of `mean` twice from INLA model output instead of reading `sd` - [PR #121](https://github.com/4DModeller/fdmr/pull/121)
- Issue with the exposure parameter being selectable when using Gaussian data, updated the model builder Shiny app to reflect this fix in its code output tab - [PR #183](https://github.com/4DModeller/fdmr/pull/183)
- Bug when trying to plot a map of model predictions without having run a model in the model builder Shiny app - [PR #181](https://github.com/4DModeller/fdmr/pull/181)

### Changed

- Use of `rgdal` functions migrated to `sf` equivalents due to [`rgdal` retirement](https://r-spatial.org/r/2022/04/12/evolution.html) - [PR #145](https://github.com/4DModeller/fdmr/pull/145)
- Move to use `utils::untar` instead of `archive::extract_archive` due to issues on some Linux systems - [PR #101](https://github.com/4DModeller/fdmr/pull/130)
- The `plot_mesh` function now uses an interactive [Leaflet](https://rstudio.github.io/leaflet/) map with the optional to plot spatial data overlaid - [PR #162](https://github.com/4DModeller/fdmr/pull/162)
- Use of `Intercept` updated due to deprecation in INLA - [PR #180](https://github.com/4DModeller/fdmr/pull/180)
- Replaced use of `INLA::inla.mesh.2d` with updated `fmesher::fm_mesh_2d_inla` which is faster - [PR #182](https://github.com/4DModeller/fdmr/pull/182)

### Removed

- `plot_interactive_map` Shiny app removed due to limited usefulness - [PR #166](https://github.com/4DModeller/fdmr/pull/166)

