## TaR Matrix Development Strategies

### Objective
Determine how to use available data to fill out the Time-at-Risk matrix for the forest model.

### Data
_Forest Cover_: A raster of forest coverage is available [here](https://georgoff.github.io/forest_malaria/data/cambodia_hansen_any_forest_0pc_native.tif). This file contains latitude/longitude pairs for the greater Cambodia region, with a binary indicator of forest coverage in each pixel.

_Village Locations_: A dataset of village locations is available [here](https://georgoff.github.io/forest_malaria/data/gis_osm_places_free_1.csv). This file has latitude/longitude of each location, as well as the type of location (village, town, etc.) and the population.

_Friction Surface_: A friction surface is available [here](https://georgoff.github.io/forest_malaria/data/friction_surface_2015_v1.tif). This file quantifies the travel time between any two geographical points.

### Defining Forests
The forest cover raster is a binary indicator of forest within each pixel. In reality, it is very difficult to determine which parts/how much of a forest that humans are using; therefore, there are multiple ways to classify a location as "forest". A few examples:
* Each pixel containing forest is its own "forest"
* A "forest" consists of a grouping of forested pixels wherein each forested pixel borders at least one other forested pixel

### Options

##### Option #1: Forest Proportion Within Polygon
Method:
* Define polygon around village point
    * Example: Square with sides of size 0.001&deg;, centered around village location
* Sum pixels within polygon that contain forest
* Divide sum by total number of pixels to obtain proportion of forest within polygon

Results:

<img src="https://georgoff.github.io/forest_malaria/data/forest-prop-map.png" alt="forest-prop-map" width="600px"/>

##### Option #2: Travel Time to Nearest Forested Pixel
Method:
* Define polygon around village point
    * Example: Square with sides of size 0.001&deg;, centered around village location
* Isolate pixels within polygon that contain forest
* Use friction surface to calculate travel time to closest forested pixel