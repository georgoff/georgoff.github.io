####################################################
#
# Superspreading Figure Generation
#
# Date: 3/28/2019
#
# Authors: Laura Cooper, Dave Smith, Alec Georgoff
#
####################################################

rm(list = ls())

#####
# REQUIRED USER INPUTS
#####

# change this variable to the location where you have saved the
# "figure_generation_files" folder:
directory <- "/homes/georgoff/georgoff.github.io/superspreading/figure_generation_files/"

#####
# Check for and install necessary packages
#####

list.of.packages <- c("foreign", "MASS", "RColorBrewer", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(foreign)
library(MASS)
library(RColorBrewer)
library(scales)

#####
# Source scripts
#####

source(paste0(directory, "scripts/Data_prep_LC.R"))
source(paste0(directory, "scripts/HBR.R"))
source(paste0(directory, "scripts/HBR-and-EIR.R"))
