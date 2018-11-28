########################
# TaR Malaria (Simple)
#
# Author: Alec Georgoff
#
# Purpose: Solve for equilibrium prevalence values given R values in a system with one village and
#          one forest
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
# Choose options
#
###################################

R_v_min <- 0
R_v_max <- 5
R_v_step_size <- 0.1

R_f_min <- 0
R_f_max <- 5
R_f_step_size <- 0.1

make_surface <- T
make_heatmap <- T

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

p <- 0.4

###################################
#
# Establish matrices of variables
#
###################################

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

Psi <- matrix(c(1,1-p,0,p), nrow=2)

H_psi <- t(Psi) %*% H
X_psi <- t(Psi) %*% X

# choose starting point for root solver:
theta_start <- c(0.9, 0.9)

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
                       p_val = p,
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




# set R values to cycle through:
R_0_v_values <- seq(R_v_min, R_v_max, R_v_step_size)
R_0_f_values <- seq(R_f_min, R_f_max, R_f_step_size)

# create data table to store results:
results <- data.table(R_0_v = rep(0, times = length(R_0_f_values) * length(R_0_v_values)), R_0_f = 0,
                      theta_v = 0, theta_f = 0,
                      X_v = 0, X_f = 0,
                      X_psi_v = 0, X_psi_f = 0,
                      root_f_value_v = 0, root_f_value_f = 0,
                      iter = 0, estim.precis = 0)

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

if (make_heatmap) {
  heatmap <- plot_ly(x = results$R_0_v,
                     y = results$R_0_f,
                     z = results$theta_v,
                     type = "heatmap",
                     # colors = colorRamp(c("green", "red")),
                     height = 800, width = 960) %>%
    layout(title = paste0("Equilibrium Prevalence in Village as a Function of R in Village and Forest      p = ", p),
           titlefont = list(size = 16),
           xaxis = list(title = "R_0 Value, Village",
                        titlefont = list(size = 20)),
           yaxis = list(title = "R_0 Value, Forest",
                        titlefont = list(size = 20)))
  
  heatmap
}

if (make_surface) {
  results$thresh <- "Malaria in Village"
  results$thresh[which(results$theta_v < 0.00001)] <- "No Malaria in Village"
  results$thresh[which(results$R_0_v < 1 & results$R_0_f < 1)] <- "Both R Values Below 0"
  
  surface <- plot_ly(data = results,
                x = ~R_0_v,
                y = ~R_0_f,
                z = ~theta_v,
                color = ~thresh,
                colors = c("purple", "red", "blue"),
                type = "scatter3d") %>%
    add_markers() %>%
    layout(
      title = paste0("Malaria Prevalence in the Village as a Function of R\np = ", p),
      scene = list(
        xaxis = list(title = "R, Village"),
        yaxis = list(title = "R, Forest"),
        zaxis = list(title = "Village Malaria Prevalence")
      )
    )
  
  surface
}

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