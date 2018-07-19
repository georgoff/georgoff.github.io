## TaR Matrix Development Strategies

### Objective
Determine how to use available data to fill out the Time-at-Risk matrix for the forest model.

### Data
_Forest Cover_: A raster of forest coverage is available [here](https://georgoff.github.io/forest_malaria/data/cambodia_hansen_any_forest_0pc_native.tif). This file contains latitude/longitude pairs for the greater Cambodia region, with a binary indicator of forest coverage in each pixel.

_Village Locations_: A dataset of village locations is available [here](https://georgoff.github.io/forest_malaria/data/gis_osm_places_free_1.csv). This file has latitude/longitude of each location, as well as the type of location (village, town, etc.) and the population.

### Options

#### Option #1: "Proximity Within Polygon"
Method:
* Define polygon around village point
    * Example: Square with sides of size 0.001&deg;, centered around village location
* Sum pixels within polygon that contain forest
* Divide sum by total number of pixels to obtain proportion of forest within polygon

Results:
![forest-prop-map](https://georgoff.github.io/forest_malaria/data/forest-prop-map.png)