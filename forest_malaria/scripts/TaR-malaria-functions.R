########################
# TaR Malaria
#
# Author: Alec Georgoff
#
# Purpose: Solve for equilibrium prevalence values given R values in a complex system
########################

list.of.packages <- c("rootSolve", "data.table", "plotly", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(rootSolve)
require(data.table)
require(plotly)
require(ggplot2)

###################################
#
# Set parameters
#
###################################

# specify filepaths for parameter .csv files:

# params_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/params.csv"
# psi_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/psi.csv"

# set R values to cycle over:

# R_min <- 0
# R_max <- 2
# R_step <- 0.5

# PDF output settings:
# output_PDF <- TRUE
# pdf_filepath <- "/homes/georgoff/georgoff.github.io/forest_malaria/test2.pdf"

# this script assumes that the following parameters are the same for every
# location; in reality this may not be accurate. an update may be made that
# allows for custom parameters in every location

# a <- 0.88   # human blood feeding rate
# b <- 0.55   # proportion of bites by infectious mosquitoes that cause an infection
# c <- 0.15   # proportion of mosquitoes infected after biting infectious human
# g <- 0.1    # per capita death rate of mosquitoes
# r <- 1/200  # rate that humans recover from an infection
# n <- 12     # time for sporogonic cycle
# S <- a/g    # stability index

###################################
#
# Establish matrices of variables
#
###################################

# read in village and forest parameters from .csv file:

initialize_variables <- function(inFile_params, inFile_psi) {
  params <- as.data.table(read.csv(inFile_params$datapath))

  H <- params$H
  X <- vector(mode = "numeric", length = length(H))

  Psi <- as.data.table(read.csv(inFile_psi$datapath))
  Psi[, id := NULL]
  Psi <- as.matrix(Psi)
  Psi_dt <- as.data.table(Psi)

  locs <- names(Psi_dt)

  return(list("H" = H,
              "X" = X,
              "Psi" = Psi,
              "locs" = locs))
}

initialize_variables_2 <- function(inFile_params, inFile_psi) {
  params <- as.data.table(read.csv(inFile_params))

  H <- params$H
  X <- vector(mode = "numeric", length = length(H))

  Psi <- as.data.table(read.csv(inFile_psi))
  Psi[, id := NULL]
  Psi <- as.matrix(Psi)
  Psi_dt <- as.data.table(Psi)

  locs <- names(Psi_dt)

  return(list("H" = H,
              "X" = X,
              "Psi" = Psi,
              "locs" = locs))
}

###################################
#
# Set up the equations as a function
#
###################################

model <- function(X, Psi, R, c_val, S_val, H) {

  theta_psi <- (t(Psi) %*% X) / (t(Psi) %*% H)

  equation_vector <- (Psi %*% (R * (theta_psi/(c_val*S_val*theta_psi + 1)))) *
    (H-X) - X

  return(equation_vector)

}

###################################
#
# Solve for roots
#
###################################

find_roots <- function(R, Psi., H., S., c_val) {
  
  # choose starting point for root solver:
  theta_start <- vector(mode = "numeric", length = length(H.))
  theta_start[1:length(theta_start)] <- 0.9
  
  # convert to number of humans:
  X_start <- theta_start * H.

  # use multiroot solver to find roots:
  ss <- multiroot(f = model, start = X_start,
                  positive = TRUE, maxiter = 1000,
                  ctol = 1e-20,
                  Psi = Psi.,
                  R = R,
                  c_val = c_val,
                  S_val = S.,
                  H = H.)

  return(ss)
}

###################################
#
# Set up results table
#
###################################

create_results_table <- function(R_min, R_max, R_step, locs) {
  all_R_values <- seq(R_min, R_max, R_step)

  list_of_R_values <- list(NULL)

  for (i in 1:length(locs)) {
    list_of_R_values[[i]] <- all_R_values

    names(list_of_R_values)[i] <- locs[i]
  }

  results <- as.data.table(expand.grid(list_of_R_values))

  theta_holder <- as.data.table(matrix(data = 0, nrow = nrow(results),
                                       ncol = length(locs)))

  for (k in 1:length(locs)) {
    names(theta_holder)[k] <- paste0("theta_", locs[k])
  }

  results <- cbind(results, theta_holder)

  return(results)
}

###################################
#
# Cycle through R values
#
###################################

solve_all_R_values <- function(results, locs,
                               Psi, H, S, c) {
  for (i in 1:nrow(results)) {
    # cat("Working on ", i, " of ", nrow(results), "\n")
    
    these_R_values <- unlist(results[i, 1:length(locs)], use.names = FALSE)
    
    X_solutions <- find_roots(R = these_R_values,
                              Psi. = Psi, H. = H, S. = S, c_val = c)$root
    
    theta_solutions <- (t(Psi) %*% X_solutions) / (t(Psi) %*% H)
    
    for (j in (1 + length(locs)):ncol(results)) {
      results[i, j] <- theta_solutions[j - length(locs)]
    }
    
    incProgress(1/nrow(results), detail = paste("Working on", i, "of", nrow(results)))
  }
  
  return(results)
}
