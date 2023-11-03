# Spatial distribution of snow - exploratory tutorial

This tutorial explores whether 4Dmodeller can be used to model the spatial distribution of snow in the Bayelva area on Svalbard, based on once-a-year point-based manual snow depth measurements for several years and snow cover duration derived from timelapse camera imagery. This data has been used before to estimate snow depth and permafrost using data assimilation methods (see literature references). Additionally, we included topography and topographic parameters as these are known to correlate with snow depth distribution. The dataset used in this tutorial has been prepared by Kristoffer Aalstad based on the following publicly available data:

**XXX ADD dois for the datasets**

The dataset is originally projected (UTM zone 33X) and available in the data sub-folder in this repository. As UTM was not supported for all steps we anticipated while developing this tutorial (see below), there is also a lat/lon version of the same data.

### Literature references

Aalstad, K. et al. (2018). <doi:10.5194/tc-12-247-2018>. TC.

Aalstad, K. et al. (2020). doi: 10.1016/j.rse.2019.111618. RSE.

Boike, J. et al. (2018). <doi:10.5194/essd-10-355-2018>. ESSD.

Zweigel, R. et al. (2021). <doi:10.1029/2020JF005673>.

## Import and load the data

```{r modules}
# load modules used in this tutorial
library(ncdf4)
#library(tidyverse)

# Specify where your data is stored locally
ncpath <- "N:/courses_conferences/2023_4DM_hackathon/fdmr/vignettes/data/" 
ncname <- "Bayelva_snow" 
ncfname <- paste(ncpath, ncname, ".nc", sep="")

#open data 
ncin <- nc_open(ncfname) 
head(ncin)
```

## Get relevant features
The netcdf file has both point measurements of snow depth (ds, or translated into SWE: Ds) for several years, and continuous raster data with snow depth duration for several years and topographic features. We start with the snow depth points only:

```{r modules}
# specify the longitude and latitude columns. Note that the coordinates are utm!
longitude <- ncvar_get(ncin,"xs") 
latitude <-ncvar_get(ncin,"ys")

# Get the first year of snow depth (ds)
ds <- ncvar_get(ncin,"ds")[,1]

# Create a data frame
df <- data.frame(LONG = longitude,LAT = latitude,ds) 
head(df)
```

## Build the mesh
The shiny app doesn't work with UTM, so we need to do this non-interactively.

```{r modules}
# This doesn't work with UTM:
# fdmr::mesh_builder(spatial_data = df, crs="+proj=utm +zone=33")

# We have to define an UTM
crs <- "+proj=utm +zone=33" 
mesh <- fmesher::fm_mesh_2d_inla(loc =
df[,c(1,2)], max.edge = c(20,40),crs = crs ) 

plot(mesh)
```

## Visualise the mesh: also plot our data points
This doesn't seem to work with the built-in plot_mesh command in UTM.
```{r modules}
fdmr::plot_mesh(mesh=mesh, spatial_data = df[,c(1,2)])
```
