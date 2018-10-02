########################
# TaR Malaria
#
# Author: Alec Georgoff
#
# Purpose: Solve for equilibrium prevalence values given R values in a complex system
########################

rm(list = ls())

# require(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
# require(data.table)
# require(plotly, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

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

params_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/params.csv"
psi_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/psi.csv"

# set R values to cycle over:

R_min <- 0
R_max <- 2
R_step <- 0.5

# this script assumes that the following parameters are the same for every
# location; in reality this may not be accurate. an update may be made that
# allows for custom parameters in every location

a <- 0.88 # human blood feeding rate
b <- 0.55 # proportion of bites by infectious mosquitoes that cause an infection
c <- 0.15 # proportion of mosquitoes infected after biting infectious human
g <- 0.1 # per capita death rate of mosquitoes
r <- 1/200 # rate that humans recover from an infection
n <- 12 # time for sporogonic cycle
S <- a/g # stability index

###################################
#
# Establish matrices of variables
#
###################################

# read in village and forest parameters from .csv file:

params <- as.data.table(read.csv(params_path))

n_villages <- nrow(params)

H <- params$H
X <- vector(mode = "numeric", length = length(H))

Psi <- as.data.table(read.csv(psi_path))
Psi[, id := NULL]
Psi <- as.matrix(Psi)
Psi_dt <- as.data.table(Psi)

locs <- names(Psi_dt)

H_psi <- t(Psi) %*% H
X_psi <- t(Psi) %*% X

# choose starting point for root solver:
theta_start <- vector(mode = "numeric", length = length(H))
theta_start[1:length(theta_start)] <- 0.9

# convert to number of humans:
X_start <- theta_start * H

###################################
#
# Set up the equations as a function
#
###################################

model <- function(X, Psi, R, c_val, S_val, H) {
  
  theta_psi <- (t(Psi) %*% X) / (t(Psi) %*% H)
  
  equation_matrix <- (Psi %*% (R * (theta_psi/(c_val*S_val*theta_psi + 1)))) * (H-X) - X
  
  return(equation_matrix)
  
}

###################################
#
# Solve for roots
#
###################################

find_roots <- function(R,
                       Psi. = Psi,
                       H. = H,
                       S. = S,
                       c_val = c,
                       X_start. = X_start) {
  
  # use multiroot solver to find roots:
  ss <- multiroot(f = model, start = X_start.,
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

all_R_values <- seq(R_min, R_max, R_step)

list_of_R_values <- list(NULL)

for (i in 1:length(locs)) {
  list_of_R_values[[i]] <- all_R_values
  
  names(list_of_R_values)[i] <- locs[i]
}

# fill results table with every possible combination of
# R values:

results <- as.data.table(expand.grid(list_of_R_values))

# put in placeholder for theta values:

theta_holder <- as.data.table(matrix(data = 0, nrow = nrow(results), ncol = length(locs)))

for (k in 1:length(locs)) {
  names(theta_holder)[k] <- paste0("theta_", locs[k])
}

results <- cbind(results, theta_holder)

###################################
#
# Cycle through R values
#
###################################

for (i in 1:nrow(results)) {
  cat("Working on ", i, " of ", nrow(results), "\n")
  
  these_R_values <- unlist(results[i, 1:length(locs)], use.names = FALSE)
  
  X_solutions <- find_roots(these_R_values)$root
  
  theta_solutions <- (t(Psi) %*% X_solutions) / H_psi
  
  for (j in (1 + length(locs)):ncol(results)) {
    results[i, j] <- theta_solutions[j - length(locs)]
  }
}

###################################
#
# Create PDF of results
#
###################################

pdf("/homes/georgoff/georgoff.github.io/forest_malaria/test.pdf")

for (j in 1:500) {
  this_row <- data.table(loc = c(paste0("v1\n",
                                        "R = ", results[j, v1]),
                                 paste0("v2\n",
                                        "R = ", results[j, v2]),
                                 paste0("v3\n",
                                        "R = ", results[j, v3]),
                                 paste0("f1\n",
                                        "R = ", results[j, f1]),
                                 paste0("f2\n",
                                        "R = ", results[j, f2])),
                         theta = unlist(results[j, 6:10], use.names = F))
  
  bar <- ggplot(data = this_row,
                aes(x = loc, y = theta)) +
    geom_col() +
    geom_text(aes(label = round(theta, 3), y = theta + 0.02)) +
    coord_cartesian(ylim = c(0,0.31))
  
  print(bar)
}

dev.off()