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

a <- 0 # human blood feeding rate
b <- 0 # proportion of bites by infectious mosquitoes that cause an infection
c <- 0 # proportion of mosquitoes infected after biting infectious human
g <- 0 # per capita death rate of mosquitoes
r <- 0 # rate that humans recover from an infection
n <- 0 # time for sporogonic cycle

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


n_villages <- 3
n_forests <- 1
n_total <- n_villages + n_forests

H <- vector(mode = "numeric", length = n_total)
V <- vector(mode = "numeric", length = n_total)