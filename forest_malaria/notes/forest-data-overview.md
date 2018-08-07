## Forest Data Overview

#### Files
_Forest cover_
* `"/homes/georgoff/forest_data/forest_points.RDS"`
* `"/homes/georgoff/forest_data/cambodia_hansen_any_forest_0pc_native.tif"`
* `"/homes/georgoff/georgoff.github.io/forest_malaria/oxford/output-all-forest-plots.R"`
  - Generates plots for all forest cover rasters

_Village locations_
* `"/homes/georgoff/forest_data/gis_osm_places_free_1.csv"`
  - Original `.csv` from Katie Battle
* `"/homes/georgoff/georgoff.github.io/forest_malaria/data/village_points.csv"`
  - Narrowed down to just locations with `village` type

_Friction surface_
* `"/homes/georgoff/forest_data/friction_surface_2015_v1.tif"`

#### Scripts
`R_generic_accessibility_mapping_script.R`
* Dan Weiss' script for utilizing the friction surface
* Produces a raster of travel times from the nearest reference point
  - Reference points are input via a `.csv` with lat/long coordinates

`individual_rasters.R`
* Similar to first script, but doesn't aggregate across all reference points
* Creates an indiviudal raster for each reference point

`forest-prop.R`
* Calculates proportion of forest cover within a polygon defined around a given village point
* Method:
  - Define polygon around village point
    - Example: Square with sides of size 0.001&deg;, centered around village location
  - Sum pixels within polygon that contain forest
  - Divide sum by total number of pixels to obtain proportion of forest within polygon
* Outputs `forest-prop-map.pdf`
