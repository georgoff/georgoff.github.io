rm(list = ls())

require(stats, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
require(data.table, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

n_f <- 7
forest_max <- 0.8
n_v <- 5

# set.seed(5)

distribute_travel <- function(n_f, forest_max) {
  results <- vector("numeric", length = n_f)
  
  for (i in 1:n_f) {
    if (i == 1) {
      max = forest_max
    }
    else {
      max = forest_max - sum(results[1:i-1])
    }
    
    # cat("max = ", max, "\n")
    
    if (i < n_f) {
      results[i] <- runif(1, min = 0, max = max)
    }
    if (i == n_f) {
      results[i] <- forest_max - sum(results[1:i-1])
    }
    # cat("result = ", results[i], "\n")
  }
  
  return(results)
}

# test <- distribute_travel(n_f, forest_max)
# 
# test_sum <- vector("numeric", length = length(test))
# 
# for (i in 1:n_f) {
#   test_sum[i] <- sum(test[1:i])
# }
# 
# plot(1:n_f, test_sum)

generate_psi <- function(n_f, n_v, forest_max) {
  psi <- as.data.table(matrix(data = 0, nrow = 2*n_v, ncol = n_v + n_f + 1))
  
  psi[,1] <- vector("character", length = nrow(psi))
  names(psi)[1] <- "id"
  
  for (i in 2:(n_v + n_f + 1)) {
    if (i - 1 <= n_v) {
      names(psi)[i] <- paste0("V", as.character(i - 1))
    }
    if (i - 1 > n_v) {
      names(psi)[i] <- paste0("F", as.character(i - 1 - n_v))
    }
  }
  
  for (i in 1:nrow(psi)) {
    if (i %% 2) {
      # odd
      psi$id[i] <- paste0("V", as.character((i+1)/2), "-V")
      psi[i, ((i+1)/2)+1] <- 1
    }
    
    if (!(i %% 2)) {
      # even
      psi$id[i] <- paste0("V", as.character(i/2), "-F")
      psi[i, (i/2)+1] <- 1 - forest_max
      psi[i, (2 + n_v):ncol(psi) := as.list(distribute_travel(n_f, forest_max))]
    }
  }
  
  return(psi)
}

write.csv(psi, file = "/homes/georgoff/georgoff.github.io/forest_malaria/psi_files/psi_1.csv",
          row.names = FALSE)

# create "answer key"
# all humans are randomly assigned to a population until they're all filled
# requires: total # humans, # humans in each population
population_path <- "/homes/georgoff/georgoff.github.io/forest_malaria/example1/params.csv"

n_v <- 5

pops <- as.data.table(read.csv(population_path))
pops[, X := NULL]

answers <- as.data.table(matrix(data = 0, nrow = sum(pops$H), ncol = 1))
names(answers) <- "human"
answers$human <- seq(1, sum(pops$H))

answers$group <- vector("character", length = nrow(answers))

for (i in 1:nrow(pops)) {
  random_indices <- sample(which(answers$group == ""), pops$H[i], replace = FALSE)
  
  answers$group[random_indices] <- as.character(pops$id[i])
}

# create giant psi matrix
# rows: one for each human
# columns: one for each village and forest
psi <- as.data.table(matrix(data = 0, nrow = nrow(answers), ncol = n_v + n_f + 1))
psi[,1] <- seq(1, nrow(psi))
names(psi)[1] <- "human"

for (i in 1:n_v) {
  names(psi)[i+1] <- paste0("V", as.character(i))
}

for (i in 1:n_f) {
  names(psi)[i + 1 + n_v] <- paste0("F", as.character(i))
}
# fill out psi matrix
# look up which village a human belongs to
# assign a random (very high) p value to that village
# randomly assign forest p values
