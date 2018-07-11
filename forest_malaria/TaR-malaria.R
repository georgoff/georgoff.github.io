########################
# TaR Malaria
#
# Author: Alec Georgoff
#
# Purpose: Solve for equilibrium prevalence values given R_0 values in a complex system
########################

rm(list = ls())

###################################
#
# Set parameters
#
###################################

a <- 0.88 # human blood feeding rate
b <- 0.55 # proportion of bites by infectious mosquitoes that cause an infection
c <- 0.15 # proportion of mosquitoes infected after biting infectious human
g <- 0.1 # per capita death rate of mosquitoes
r <- 1/200 # rate that humans recover from an infection
n <- 12 # time for sporogonic cycle
S <- a/g # stability index

p <- 0.3

###################################
#
# Establish matrices of variables
#
###################################

## TO-DO: ask for number of villages/forests ##
# readinteger <- function() {
#   n <- readline(prompt = "Enter an integer: ")
#   return(as.integer(n))
# }


n_villages <- 1
n_forests <- 1
n_total <- n_villages + n_forests

# H <- vector(mode = "numeric", length = n_total)
# V <- vector(mode = "numeric", length = n_total)
# X <- vector(mode = "numeric", length = n_total)
# Y <- vector(mode = "numeric", length = n_total)
H <- as.vector(c(5000,2000))
V <- as.vector(c(100,500))
X <- as.vector(c(0,0))
Y <- as.vector(c(0,0))

# set up function to calculate R values:
calculate_R <- function(V, a, b, c, g, n, H, r) {
  R = (V * a^2 * b * c * exp(-g * n)) / (H * g * r)
  return(R)
}

# calculate R values:
R <- calculate_R(V, a, b, c, g, n, H, r)

# Psi <- matrix(data = 0, nrow = n_total, ncol = n_total)
Psi <- matrix(c(1,1-p,0,p), nrow=2)

H_psi <- t(Psi) %*% H
X_psi <- t(Psi) %*% X

# choose starting point for root solver:
# chi_start <- vector(mode = "numeric", length = n_total)
chi_start <- c(0.9, 0.1)

# convert to number of humans:
X_start <- chi_start * H

###################################
#
# Set up the equations as a function
#
###################################

model <- function(X, Psi, R, c_val, S_val, H) {
  equation_matrix <- (Psi %*% (R * ((t(Psi) %*% X)/c_val*S_val*t(Psi) %*% X + t(Psi) %*% H))) * (H-X) - X
  
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
                       p_val = p,
                       X_start. = X_start) {
  
  # use multiroot solver to find roots:
  ss <- multiroot(f = model, start = X_start.,
                  Psi = Psi.,
                  R = R,
                  c_val = c_val,
                  S_val = S.,
                  H = H.)
  
  # convert results to prevalence:
  # chi_v_SS <- ss$root[1] / H_v
  # chi_f_SS <- ss$root[2] / H_f
  
  return(ss)
}