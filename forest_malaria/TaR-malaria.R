########################
# TaR Malaria
#
# Author: Alec Georgoff
#
# Purpose: Solve for equilibrium prevalence values given R_0 values in a complex system
########################

rm(list = ls())

require(rootSolve, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
require(data.table)
require(plotly, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

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
  # equation_matrix <- (Psi %*% (R * ((t(Psi) %*% X)/(c_val*S_val*t(Psi) %*% X + t(Psi) %*% H)))) * (H-X) - X
  # equation_matrix <- (Psi %*% ((t(1/Psi) %*% R) * ((t(Psi) %*% X)/(c_val*S_val*t(Psi) %*% X + t(Psi) %*% H)))) * (H-X) - X
  
  chi_psi <- (t(Psi) %*% X) / (t(Psi) %*% H)
  
  equation_matrix <- (Psi %*% (R * (H / (t(Psi) %*% H)) * (chi_psi/(c_val*S_val*chi_psi + 1)))) * (H-X) - X
  # equation_matrix <- (Psi %*% (R * (chi_psi/(c_val*S_val*chi_psi + 1)))) * (H-X) - X
  
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
  chi_SS <- ss$root / H
  # chi_v_SS <- ss$root[1] / H_v
  # chi_f_SS <- ss$root[2] / H_f
  
  return(chi_SS)
}




# set R values to cycle through:
R_0_v_values <- seq(0, 10, 0.1)
R_0_f_values <- seq(0, 10, 0.1)

R_values <- rbind(R_0_f_values, R_0_v_values)

# create data table to store results:
results <- data.table(R_0_v = rep(0, times = length(R_0_f_values) * length(R_0_v_values)),
                      R_0_f = 0, chi_v = 0, chi_f = 0)

i <- 1

for (v in R_0_v_values) {
  for (f in R_0_f_values) {
    # record current R values:
    results[i, R_0_v := v]
    results[i, R_0_f := f]
    # solve for roots at those R values:
    results[i, chi_v := find_roots(c(v, f))[1]]
    results[i, chi_f := find_roots(c(v, f))[2]]
    
    # print progress:
    cat("R_0_v =", v, ", R_0_f =", f, " \r", file = "", sep = " ")
    flush.console()
    
    i <- i + 1
  }
}

p <- plot_ly(x = results$R_0_v,
             y = results$R_0_f,
             z = results$chi_v,
             type = "heatmap",
             height = 800, width = 960) %>%
  layout(title = "Equilibrium Prevalence in Village as a Function of R_0 in Village and Forest",
         titlefont = list(size = 16),
         xaxis = list(title = "R_0 Value, Village",
                      titlefont = list(size = 20)),
         yaxis = list(title = "R_0 Value, Forest",
                      titlefont = list(size = 20)))

p