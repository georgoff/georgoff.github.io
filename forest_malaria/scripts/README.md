---
title: Forest Model Code Overview
tags: [Notebooks/Quick Notes]
---

# Forest Model Code Overview

## Overview
There are a few scripts that can be used to run the TaR forest model:
- `TaR-malaria.R`
- `TaR-malaria-simple.R`
- `TaR-malaria-functions.R`

Each of these scripts are slightly different but, for the most part, accomplish the same thing: they accept model parameters and output malaria prevalence in every location in the model.

## `TaR-malaria.R`
_This script is the most versatile, allowing the user to run any size of model. It requires `.csv` files as inputs that specify the model parameters._

This script requires the following parameters:
- `params`
   - The number of humans in each unique human population
- `psi`
   - The complete &Psi; matrix for the given system, which describes time-at-risk
- `r_values`
   - The range of `R` values in each location to calculate prevalence over. Each location must have an `R_min`, `R_max`, and `R_step`

### User's Guide
1) Create the necessary `.csv` files for the model parameters:
   - `params.csv` takes the following form:
   
| id | H |   |
|:---:|:---:|:---:|
| V1-V | 47 | v only |
| V1-F | 11 | v &harr; f |
| V2-V | 43 | v only |
| V2-F | 13 | v &harr; f |

   - `psi.csv` takes the following form:
   
| id | V1 | V2 | F |
|:---:|:---:|:---:|:---:|
| V1-V | 1 | 0 | 0 |
| V1-F | 0.3 | 0 | 0.7 |
| V2-V | 0 | 1 | 0 |
| V2-F | 0 | 0.6 | 0.4 |

   - `r_values.csv` takes the following form:
   
| id | R_min | R_max | R_step |
|:--:|:---:|:---:|:---:|
| V1 | 0 | 5 | 0.1 |
| V2 | 0 | 5 | 0.1 |
| F | 3 | 3 | 1 |

   - **Note: The order of the `id` variables must be the same in `params` and `psi` in order for the values to be scaled correctly!**

2) Specify the filepaths for the `.csv` files in the script:
```r
params_path <- "/path_to_file/params.csv"
psi_path <- "/path_to_file/psi.csv"
r_values_path <- "/path_to_file/r_values.csv"
```

3) Choose output settings in the script and set filepaths:
```r
output_csv <- TRUE
output_bar_PDF <- TRUE
output_line_PDF <- TRUE
csv_filepath <- "/path_to_file/results.csv"
pdf_bar_filepath <- "/path_to_file/bar.pdf"
pdf_line_filepath <- "/path_to_file/lines.pdf"
```
   - **Note: the `output_line_PDF` functionality was not fully developed so it is commented out. It was only used once for a specific case, so it was not built to adapt to different sets of model parameters.**

4) **Run the script**
   - Note that the main determinant for runtime of the code is the range of `R` values that have to be cycled through. The model solves for every possible combination of `R` values, so the computational cost rises very quickly.

### Outputs
- `results.csv`
   - This is a `.csv` file containing the prevalence in every location for every possible combination of `R` values
- `bar.pdf`
   - This is a visual representation of `results.csv`, with a bar chart for each combination of `R` values

## `TaR-malaria-simple.R`
_This script runs the simplest possible forest model: one village and one forest. This simplicity allows for more interesting visual representations of the results, which are included in this script._

This script does not read in external files to set parameters; rather, the parameters are defined within the script itself. Because the model consists of one village and one forest, the number of parameters to set is fairly small:
- `R` value ranges for the village and forest
- Human population sizes in the village and forest (the number of stationary "villagers" and number of "forest-goers" that travel to the forest)
- One `p` value for the &Psi; matrix, which will be filled out in the following way:
|   | `V` | `F` |
|:---:|:---:|:---:|
| `H_V` | 1 | 0 |
| `H_F` | 1-p | p |
   - The `p` value describes what proportion of time the forest-goers spend in the forest

### User's Guide
1) Set `p` value:
```r
p <- 0.4
```

2) Set `R` values:
```r
R_v_min <- 0
R_v_max <- 3
R_v_step_size <- 0.05

R_f_min <- 0
R_f_max <- 3
R_f_step_size <- 0.05
```

3) Choose output settings:
```r
make_surface <- T
make_binary_heatmap <- T
make_continuous_heatmap <- T
```

4) Set human popuation sizes:
```r
# set number of villagers and forest-goers, respectively:
H <- as.vector(c(5000,2000))
```

5) **Run the script**
- **Note: This script does not write any outputs to files; it is meant to be run in an interactive RStudio session**

## `TaR-malaria-functions.R`
_This script is an adaptation of `TaR-malaria.R`, meant to be used with a Shiny app._

This script is sourced by the Shiny app that is meant to present an interactive verion of the TaR Forest Model; it is not meant to be run on its own.
