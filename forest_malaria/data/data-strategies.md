## TaR Matrix Development Strategies

### Objective
Determine how to use available data to fill out the Time-at-Risk matrix for the forest model.

### Data
_Forest Cover_: A raster of forest coverage is available [here](https://georgoff.github.io/forest_malaria/data/cambodia_hansen_any_forest_0pc_native.tif). This file contains latitude/longitude pairs for the greater Cambodia region, with a binary indicator of forest coverage in each pixel.

_Village Locations_: A dataset of village locations is available [here](https://georgoff.github.io/forest_malaria/data/gis_osm_places_free_1.csv). This file has latitude/longitude of each location, as well as the type of location (village, town, etc.) and the population.

_Friction Surface_: A friction surface is available [here](https://georgoff.github.io/forest_malaria/data/friction_surface_2015_v1.tif). This file quantifies the travel time between any two geographical points.

### Defining Forests
The forest cover raster is a binary indicator of forest within each pixel. In reality, it is very difficult to determine which parts/how much of a forest that humans are using; therefore, there are multiple ways to classify a location as "forest". The following are a few examples:

##### Option #1: Each pixel containing forest is its own "forest"
* This leads to the maximum number of "forests", and assumes that there is no human activity between forested pixels.

##### Option #2: A "forest" consists of a grouping of forested pixels wherein each forested pixel borders at least one other forested pixel
* This leads to very large "forests" that are assumed to be one environment for transmission (i.e. there are many interactions amongst all the humans and mosquitoes in the entire forest).

##### Option #3: "Forests" are defined as groups of forested pixels within a certain proximity of a village
* This definition takes into account the fact that large portions of the forested areas are unused (because no one lives near them), while certain portions are used heavily and by many people (because they live near them).

### Determining Time-at-Risk
There is no feasible way to measure how much time villagers spend in the forest, so some assumptions will have to be made. Since the locations of both the villages and forests are known, it is a reasonable first assumption to say that people who live closer to forests (by either travel time or distance) will spend more of their time in the forest. It is therefore necessary to develop a metric for each village that describes how much of its residents' time is spent in the forest. The following are several possible methods for achieving this:

##### Option #1: Forest Proportion Within Polygon
_Method_:
* Define polygon around village point
    * Example: Square with sides of size 0.001&deg;, centered around village location
* Sum pixels within polygon that contain forest
* Divide sum by total number of pixels to obtain proportion of forest within polygon

_Example Results_:

<img src="https://georgoff.github.io/forest_malaria/data/forest-prop-map.png" alt="forest-prop-map" width="600px"/>

##### Option #2: Travel Time to Nearest Forested Pixel
_Method_:
* Define polygon around village point
    * Example: Square with sides of size 0.001&deg;, centered around village location
* Isolate pixels within polygon that contain forest
* Use friction surface to calculate travel time to closest forested pixel