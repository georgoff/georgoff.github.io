new_model(X = c(689.1342, 1543.3797), Psi = Psi, R = c(2,3), c_val = c, S_val = S, H = H)

new_model(X = c(5000,2000), Psi = Psi, R = c(2,3), c_val = c, S_val = S, H = H)

# new model:
new_model <- function(X, Psi, R, c_val, S_val, H) {
  # equation_matrix <- (Psi %*% (R * ((t(Psi) %*% X)/(c_val*S_val*t(Psi) %*% X + t(Psi) %*% H)))) * (H-X) - X
  # equation_matrix <- (Psi %*% (R * (H / (t(Psi) %*% H)) * ((t(Psi) %*% X)/(c_val*S_val*t(Psi) %*% X + t(Psi) %*% H)))) * (H-X) - X
  
  chi_psi <- (t(Psi) %*% X) / (t(Psi) %*% H)
  
  equation_matrix <- (Psi %*% (R * (H / (t(Psi) %*% H)) * (chi_psi/(c_val*S_val*chi_psi + 1)))) * (H-X) - X
  
  return(equation_matrix)
}

# old model:
old_model <- function(X, R_0_v, R_0_f, H_v, H_f, S_v, S_f, c_val, p_val) {
  # X[1] = X_f
  # X[2] = X_v
  
  # convert to prevalence:
  chi_f = X[1] / H_f
  chi_v = ((1 - p_val) * X[1] + X[2]) / ((1 - p_val) * H_f + H_v)
  
  # equation_village <- (R_0_v * (1 - p_val) * (chi_v / (1 + S_v * c_val * chi_v))) * (H_v - X[2]) - X[2]
  equation_village <- (R_0_v * (chi_v / (1 + S_v * c_val * chi_v))) * (H_v - X[2]) - X[2]
  
  equation_forest <- (R_0_v * (1 - p_val) * chi_v / (1 + S_v * c_val * chi_v) + R_0_f * p_val * chi_f / 
                        (1 + S_f * c_val * chi_f)) * (H_f - X[1]) - X[1]
  
  return(c(equation_village, equation_forest))
}

old_model(X = c(689.1342, 1543.3797), R_0_v = 2, R_0_f = 3, H_v = H[1], H_f = H[2], S_v = S, S_f = S, c_val = c, p_val = p)

old_model(X = c(5000, 2000), R_0_v = 2, R_0_f = 3, H_v = H[1], H_f = H[2], S_v = S, S_f = S, c_val = c, p_val = p)

calculate_R <- function(V, a, b, c, g, n, H, r) {
  R = (V * a^2 * b * c * exp(-g * n)) / (H * g * r)
  return(R)
}

calculate_R_psi <- function(Psi, V, a, b, c, g, n, H, r) {
  R = (V * a^2 * b * c * exp(-g * n)) / ((t(Psi) %*% H) * g * r)
  return(R)
}