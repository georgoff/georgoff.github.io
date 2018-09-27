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

village_params_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/village_params.csv"
forest_params_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/forest_params.csv"
params_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/params.csv"
psi_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/psi.csv"

###################################
#
# Set parameters
#
###################################

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

# read in village and forest parameters from .csv file:

params <- as.data.table(read.csv(params_path))

n_villages <- nrow(params)

H <- params$H
X <- vector(mode = "numeric", length = length(H))

# village_params <- as.data.table(read.csv(village_params_path))
# forest_params <- as.data.table(read.csv(forest_params_path))
# all_params <- rbindlist(list(village_params, forest_params))
# 
# H_v <- village_params$H
# V_v <- village_params$V
# X_v <- village_params$X
# Y_v <- village_params$Y
# 
# H_f <- forest_params$H
# V_f <- forest_params$V
# X_f <- forest_params$X
# Y_f <- forest_params$Y
# 
# H <- c(H_v, H_f)
# V <- c(V_v, V_f)
# X <- c(X_v, X_f)
# Y <- c(Y_v, Y_f)
# 
# n_villages <- nrow(village_params)
# n_forests <- nrow(forest_params)
# n_total <- n_villages + n_forests

# # set up function to calculate R values:
# calculate_R <- function(V, a, b, c, g, n, H, r) {
#   R = (V * a^2 * b * c * exp(-g * n)) / (H * g * r)
#   return(R)
# }
# 
# # calculate R values:
# R <- calculate_R(V, a, b, c, g, n, H, r)

# Psi <- matrix(data = 0, nrow = n_total, ncol = n_total)
# Psi <- matrix(c(1,1-p,0,p), nrow=2)
Psi <- as.data.table(read.csv(psi_path))
Psi[, id := NULL]
Psi <- as.matrix(Psi)
Psi_dt <- as.data.table(Psi)

H_psi <- t(Psi) %*% H
X_psi <- t(Psi) %*% X

# choose starting point for root solver:
theta_start <- vector(mode = "numeric", length = length(H))
theta_start[1:length(theta_start)] <- 0.9
# theta_start <- c(0.9, 0.9)

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
  
  # convert results to prevalence:
  # theta_SS <- ss$root / (t(Psi) %*% H)
  
  X_psi_SS <- t(Psi) %*% ss$root
  theta_SS <- X_psi_SS / H_psi
  # theta_v_SS <- ss$root[1] / H_v
  # theta_f_SS <- ss$root[2] / H_f
  
  return(ss)
}


# create table of all possible R values and cycle thru each row of that

R_min <- 0
R_max <- 2
R_step <- 0.5

all_R_values <- seq(R_min, R_max, R_step)

results <- as.data.table(expand.grid(v1 = all_R_values,
                                     v2 = all_R_values,
                                     v3 = all_R_values,
                                     f1 = all_R_values,
                                     f2 = all_R_values))

results[, theta_v1 := 0]
results[, theta_v2 := 0]
results[, theta_v3 := 0]
results[, theta_f1 := 0]
results[, theta_f2 := 0]

for (i in 1:nrow(results)) {
  cat("Working on ", i, " of ", nrow(results), "\n")
  
  these_R_values <- unlist(results[i, 1:5], use.names = FALSE)
  
  X_solutions <- find_roots(these_R_values)$root
  
  theta_solutions <- (t(Psi) %*% X_solutions) / H_psi
  
  results[i, 6:10 := list(theta_solutions[1],
                          theta_solutions[2],
                          theta_solutions[3],
                          theta_solutions[4],
                          theta_solutions[5])]
}


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

# results2 <- as.data.table(expand.grid(names(Psi_dt) = all_R_values))

# create data table to store results:
# results <- data.table(R_0_v = rep(0, times = length(R_0_f_values) * length(R_0_v_values)), R_0_f = 0,
#                       theta_v = 0, theta_f = 0,
#                       X_v = 0, X_f = 0,
#                       X_psi_v = 0, X_psi_f = 0,
#                       root_f_value_v = 0, root_f_value_f = 0,
#                       iter = 0, estim.precis = 0)

i <- 1

for (v in R_0_v_values) {
  for (f in R_0_f_values) {
    # record current R values:
    results[i, R_0_v := as.numeric(v)]
    results[i, R_0_f := as.numeric(f)]
    # solve for roots at those R values:
    these_roots <- find_roots(R = c(v,f))
    
    # add X values to results:
    results[i, X_v := these_roots$root[1]]
    results[i, X_f := these_roots$root[2]]
    
    # add scaled X values to results:
    results[i, X_psi_v := X_v + (1-p)*X_f]
    results[i, X_psi_f := p*X_f]
    
    # add prevalence values to results:
    results[i, theta_v := X_psi_v / H_psi[1]]
    results[i, theta_f := X_f / H[2]]
    
    # add value of equations at root to results:
    results[i, root_f_value_v := these_roots$f.root[1,]]
    results[i, root_f_value_f := these_roots$f.root[2,]]
    
    # add # of iterations and estimated precision to results:
    results[i, iter := these_roots$iter]
    results[i, estim.precis := these_roots$estim.precis]
    
    # print progress:
    cat("R_0_v =", v, ", R_0_f =", f, " \r", file = "", sep = " ")
    flush.console()
    
    i <- i + 1
  }
}

heatmap <- plot_ly(x = results$R_0_v,
             y = results$R_0_f,
             z = results$theta_v,
             type = "heatmap",
             height = 800, width = 960) %>%
  layout(title = "Equilibrium Prevalence in Village as a Function of R_0 in Village and Forest",
         titlefont = list(size = 16),
         xaxis = list(title = "R_0 Value, Village",
                      titlefont = list(size = 20)),
         yaxis = list(title = "R_0 Value, Forest",
                      titlefont = list(size = 20)))

heatmap

# 
# my_plot <- ggplot(data = results) +
#   geom_raster(aes(x = R_0_v, y = R_0_f, fill = theta_v)) +
#   scale_fill_gradientn(colours = c("blue", "red")) +
#   
#   # geom_point(data = results[theta_v > 0.01 & theta_v < 0.1],
#   #            aes(x = R_0_v, y = R_0_f)) +
#   
#   geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1, color = "yellow")) +
#   
#   labs(title = paste0("Equilibrium Village Prevalence of Malaria \n", "p = ", p),
#        x = "R_0 Value in Village",
#        y = "R_0 Value in Forest")
# 
# my_plot