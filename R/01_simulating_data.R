
args <- commandArgs(trailingOnly = TRUE)
setwd(args[1])

source("./load_packages.R")

MODEL_NUM <- as.numeric(args[2])

N <- as.numeric(args[3])
NUM_TASKS <- as.numeric(args[5])

DEPENDENCE_IN_SPACE <- args[6]
DEPENDENCE_IN_TIME <- args[7]
DEPENDENCE_IN_VARIABLE <- args[8]
DEPENDENCE_IN_ADVECTION <- args[9]

if(MODEL_NUM == 1){

  MODEL_NAME = 'univariate_matern_spatial'
  p = 1
  locs_in_degrees = F
  advection_model = F
  three_dimensional_space = F

  VARIANCE <- 1
  RANGE <- 0.2
  NU <- 1
  PARAM <- c(VARIANCE, RANGE, NU)
  PARAM_NAMES <- c('variance', 'range', 'smoothness')

}else if(MODEL_NUM == 2){

  MODEL_NAME = 'univariate_matern_schlather_spacetime'
  p = 1
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  VARIANCE <- 1
  RANGE <- 0.2
  NU <- 1
  VEL_MEAN <- c(0.1, 0.1)
  VEL_VARIANCE_CHOL <- c(0.1, 0, 0.1)
  PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}else if(MODEL_NUM == 3){
  
  MODEL_NAME = 'bivariate_matern_parsimonious_spatial'
  p = 2
  locs_in_degrees = F
  advection_model = F
  three_dimensional_space = F
  
  VARIANCE1 <- 1
  VARIANCE2 <- 1
  RANGE <- 0.2
  NU1 <- 0.5
  NU2 <- 1
  RHO <- 0.6
  PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO)
  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation')

}else if(MODEL_NUM == 4){

  MODEL_NAME = 'bivariate_matern_single_advection_spacetime'
  p = 2
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  VARIANCE1 <- 1
  VARIANCE2 <- 1
  if(DEPENDENCE_IN_SPACE == 'WEAK'){
    RANGE <- 0.03
  }else if(DEPENDENCE_IN_SPACE == 'MODERATE'){
    RANGE <- 0.1
  }else if(DEPENDENCE_IN_SPACE == 'STRONG'){
    RANGE <- 0.2
  }
  NU1 <- 0.5
  NU2 <- 1
  RHO <- DEPENDENCE_IN_VARIABLE
  VEL_MEAN <- c(0.1, 0.1)
  if(DEPENDENCE_IN_TIME == 'WEAK'){
    VEL_VARIANCE_CHOL <- c(1, 0, 1)
  }else if(DEPENDENCE_IN_TIME == 'MODERATE'){
    VEL_VARIANCE_CHOL <- c(0.5, 0, 0.5)
  }else if(DEPENDENCE_IN_TIME == 'STRONG'){
    VEL_VARIANCE_CHOL <- c(0.1, 0, 0.1)
  }
  PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)
  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}else if(MODEL_NUM == 5){
 
  MODEL_NAME = 'bivariate_matern_multiple_advection_spacetime'
  p = 2
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  VARIANCE1 <- 0.05
  VARIANCE2 <- 0.05
  if(DEPENDENCE_IN_SPACE == 'WEAK'){
    RANGE <- 0.03
  }else if(DEPENDENCE_IN_SPACE == 'MODERATE'){
    RANGE <- 0.1
  }else if(DEPENDENCE_IN_SPACE == 'STRONG'){
    RANGE <- 0.2
  }
  NU1 <- 0.5
  NU2 <- 1
  RHO <- DEPENDENCE_IN_VARIABLE
  VEL_MEAN <- c(0.1, 0.1, -0.2, -0.2)
  VEL_VARIANCE <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 4, 4)

  if(DEPENDENCE_IN_ADVECTION == 'NEGATIVE'){
    VEL_VARIANCE[1, 3] <- VEL_VARIANCE[2, 4] <- VEL_VARIANCE[3, 1] <- VEL_VARIANCE[4, 2] <- -0.9
  }else if(DEPENDENCE_IN_ADVECTION == 'INDEPENDENT'){
    VEL_VARIANCE[1, 3] <- VEL_VARIANCE[2, 4] <- VEL_VARIANCE[3, 1] <- VEL_VARIANCE[4, 2] <- 0
  }else if(DEPENDENCE_IN_ADVECTION == 'POSITIVE'){
    VEL_VARIANCE[1, 3] <- VEL_VARIANCE[2, 4] <- VEL_VARIANCE[3, 1] <- VEL_VARIANCE[4, 2] <- 0.9
  }

  if(DEPENDENCE_IN_TIME == 'WEAK'){
    VEL_VARIANCE <- 200 * VEL_VARIANCE
  }else if(DEPENDENCE_IN_TIME == 'MODERATE'){
    VEL_VARIANCE <- 0.25 * VEL_VARIANCE
  }else if(DEPENDENCE_IN_TIME == 'STRONG'){
    VEL_VARIANCE <- 0.01 * VEL_VARIANCE
  }

  VEL_VARIANCE_CHOL_TEMP <- t(chol(VEL_VARIANCE))
  VEL_VARIANCE_CHOL <- VEL_VARIANCE_CHOL_TEMP[lower.tri(VEL_VARIANCE_CHOL_TEMP, diag = T)]
  PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)
  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation', 'vel1_mean_x', 'vel1_mean_y', 'vel2_mean_x', 'vel2_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_13', 'vel_variance_chol_14', 'vel_variance_chol_22', 'vel_variance_chol_23', 'vel_variance_chol_24', 'vel_variance_chol_33', 'vel_variance_chol_34', 'vel_variance_chol_44')

}else if(MODEL_NUM == 6){

  MODEL_NAME = 'bivariate_lmc_spatial'
  p = 2
  locs_in_degrees = F
  advection_model = F
  three_dimensional_space = F

  RANGE1 <- 0.1
  RANGE2 <- 0.2
  NU1 <- 0.5
  NU2 <- 1
  A11 <- 0.9
  A12 <- sqrt(1 - A11^2) 
  A21 <- 0.6
  A22 <- sqrt(1 - A21^2) 
  PARAM <- c(RANGE1, RANGE2, NU1, NU2, A11, A12, A21, A22)
  PARAM_NAMES <- c('range1', 'range2', 'smoothness1', 'smoothness2', 'A11', 'A12', 'A21', 'A22')

}else if(MODEL_NUM == 7){
  
  MODEL_NAME = 'univariate_deformation_matern_frozen_spacetime'
  p = 1
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  VARIANCE <- 1
  RANGE <- 0.2
  NU <- 1
  VEL_MEAN <- c(0.1, 0.1)
  DEFORM_PARAM <- c(0.5, 0.5, 1, -1, 1.5, 1, -1, 1.5)
  PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, DEFORM_PARAM)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'deform_source_x', 'deform_source_y', 'deform_coef_x_1', 'deform_coef_x_2', 'deform_coef_x_3', 'deform_coef_y_1', 'deform_coef_y_2', 'deform_coef_y_3')

}else if(MODEL_NUM == 8){

  MODEL_NAME = 'bivariate_lmc_spacetime'
  p = 2
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  RANGE1 <- 0.1
  RANGE2 <- 0.2
  NU1 <- 0.5
  NU2 <- 1
  VEL1_MEAN <- c(0.1, 0.1)
  VEL1_VARIANCE_CHOL <- c(0.1, 0, 0.1)
  VEL2_MEAN <- c(-0.3, -0.3)
  VEL2_VARIANCE_CHOL <- c(0.5, 0, 0.5)
  A11 <- 0.9
  A12 <- sqrt(1 - A11^2)
  A21 <- 0.6
  A22 <- sqrt(1 - A21^2)
  PARAM <- c(RANGE1, RANGE2, NU1, NU2, VEL1_MEAN, VEL2_MEAN, VEL1_VARIANCE_CHOL, VEL2_VARIANCE_CHOL, A11, A12, A21, A22)
  PARAM_NAMES <- c('range1', 'range2', 'smoothness1', 'smoothness2', 'vel1_mean_x', 'vel1_mean_y', 'vel2_mean_x', 'vel2_mean_y', 'vel1_variance_chol_11', 'vel1_variance_chol_12', 'vel1_variance_chol_22', 'vel2_variance_chol_11', 'vel2_variance_chol_12', 'vel2_variance_chol_22', 'A11', 'A12', 'A21', 'A22')

}else if(MODEL_NUM == 9){
  
  MODEL_NAME = 'univariate_matern_gneiting_spacetime'
  p = 1
  locs_in_degrees = F
  advection_model = F
  three_dimensional_space = F
  
  VARIANCE <- 1
  RANGE_SPACE <- 0.2
  NU <- 1
  RANGE_TIME <- 5
  ALPHA <- 1
  BETA <- 0.8
  DELTA <- 0.5
  PARAM <- c(VARIANCE, RANGE_SPACE, NU, RANGE_TIME, ALPHA, BETA, DELTA)
  PARAM_NAMES <- c('variance', 'range_space', 'smoothness', 'range_time', 'alpha', 'beta', 'delta')

}else if(MODEL_NUM == 10){

  MODEL_NAME = 'bivariate_matern_bourotte_spacetime'
  p = 2
  locs_in_degrees = F
  advection_model = F
  three_dimensional_space = F

  VARIANCE1 <- 1
  VARIANCE2 <- 1
  if(DEPENDENCE_IN_SPACE == 'WEAK'){
    RANGE_SPACE <- 0.03
  }else if(DEPENDENCE_IN_SPACE == 'MODERATE'){
    RANGE_SPACE <- 0.1
  }else if(DEPENDENCE_IN_SPACE == 'STRONG'){
    RANGE_SPACE <- 0.2
  }
  NU1 <- 0.5
  NU2 <- 1
  RHO <- 0.6
  RANGE_TIME <- 5
  ALPHA <- 1
  BETA <- 0.8
  DELTA <- 0.5
  PARAM <- c(VARIANCE1, VARIANCE2, RANGE_SPACE, NU1, NU2, RHO, RANGE_TIME, ALPHA, BETA, DELTA)
  PARAM_NAMES <- c('variance1', 'variance2', 'range_space', 'smoothness1', 'smoothness2', 'colocated_correlation', 'range_time', 'alpha', 'beta', 'delta')

}else if(MODEL_NUM == 11){

  MODEL_NAME = 'univariate_matern_numerical_lagrangian_spacetime'
  p = 1
  locs_in_degrees = F
  advection_model = T
  three_dimensional_space = F

  VARIANCE <- 1
  RANGE <- 0.2
  NU <- 1
  VEL_MEAN <- c(0.1, 0.1)
  VEL_VARIANCE_CHOL <- c(0.1, 0, 0.1)
  PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}else if(MODEL_NUM == 12){

  MODEL_NAME = 'bivariate_differential_operator_spatial'
  p = 2
  locs_in_degrees = T
  advection_model = F
  three_dimensional_space = T

  VARIANCE1 <- 1
  VARIANCE2 <- 1
  SCALE_HORIZONTAL <- 5
  SCALE_VERTICAL <- 0.5
  NU1 <- 1.5
  NU2 <- 3
  RHO <- 0.6
  A1 <- 0
  B1 <- 0
  C1 <- 0.1
  D1 <- 1
  A2 <- 0
  B2 <- 0
  C2 <- 0.1
  D2 <- 1
  PARAM <- c(VARIANCE1, VARIANCE2, SCALE_HORIZONTAL, SCALE_VERTICAL, NU1, NU2, RHO, A1, B1, C1, D1, A2, B2, C2, D2)
  PARAM_NAMES <- c('variance1', 'variance2', 'scale_horizontal', 'scale_vertical', 'smoothness1', 'smoothness2', 'colocated_correlation', 'a1', 'b1', 'c1', 'd1', 'a2', 'b2', 'c2', 'd2')

}

save_covariance = F

if(locs_in_degrees){

  lons <- seq(-180, 180, length.out = sqrt(N))
  lats <- seq(-90, 90, length.out = sqrt(N))
  sim_grid_locations <- as.matrix(expand.grid(lons, lats))

}else{
  x <- seq(0, 1, length.out = sqrt(N))
  sim_grid_locations <- as.matrix(expand.grid(x, x))
}

if(REAL_DATA){
  DATASET = 'pm_20190107'
  FILE_NAME <- paste('../MERRA2/data/', DATASET, '.RData', sep = "")
  load(FILE_NAME)
  comm.print("Done loading data file . . .")

  N = 550
  sim_grid_locations <- DAT@locs[1:N, 1:2]
}

locs <- cbind(sim_grid_locations, rep(0, N))

if(three_dimensional_space){
  pdims <- as.numeric(args[4])

  if(pdims > 1){
    for(pres in 1:(pdims - 1)){
      locs <- rbind(locs, cbind(sim_grid_locations, rep(pres, N)))
    }
  }

}else{
  TT <- as.numeric(args[4])
 
  if(TT > 1){

    if(REAL_DATA){
      for(tt in 1:(TT - 1)){
        locs <- rbind(locs, cbind(sim_grid_locations, rep(3 * tt, N)))
      }
    }else{
      for(tt in 1:(TT - 1)){
        locs <- rbind(locs, cbind(sim_grid_locations, rep(tt, N)))
      }
    }
  }
}


n <- nrow(locs)

if(comm.rank() == 0){
  cat("MODEL: ", MODEL_NAME, '\n')
}

init.grid(NPROW = sqrt(NUM_TASKS), NPCOL = sqrt(NUM_TASKS))
bldim <- c(sqrt(NUM_TASKS), sqrt(NUM_TASKS))

start_time = Sys.time()

ICTXT <- 0
dim <- c(n * p, n * p)
ldim <- base.numroc(dim = dim, bldim = bldim, ICTXT = ICTXT)
descx <- base.descinit(dim = dim, bldim = bldim, ldim = ldim, ICTXT = ICTXT)

locs_new <- NULL
for(variable_label in 1:p){
  locs_new <- rbind(locs_new, cbind(locs, rep(variable_label, n)))
}

Sigma_sub <- base.crosscovsubmat(model = MODEL_NUM, param = PARAM, x = locs_new, descx = descx)
Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim, ldim = ldim, bldim = bldim, ICTXT = ICTXT)
Sigma_chol_dd <- chol(Sigma_dd)

if(comm.rank() == 0){

  num_replicates = 100
  set.seed(1234)

  Z <- matrix(rnorm(n = n * p * num_replicates, mean = 0, sd = 1), ncol = num_replicates, nrow = n * p)
 
  covariates <- cbind(rep(1, n), (locs[, 1] - mean(locs[, 1])) / sd(locs[, 1]), (locs[, 2] - mean(locs[, 2])) / sd(locs[, 2]))
  
  X <- array(, dim = c(nrow(covariates), ncol(covariates), p))
  X[, , 1] <- covariates
  X[, , 2] <- covariates

  covariates_full <- as.matrix(bdiag(X[, , 1], X[, , 2]))

  beta <- matrix(, nrow = ncol(covariates), ncol = p)

  Y_mean_temp <- NULL
  for(variable in 1:p){
    #beta[, variable] <- c(0, 0, 0)
    beta[, variable] <- c(0.5, 0.5, 0.5)
    Y_mean_temp <- c(Y_mean_temp, c(covariates %*% beta[, variable]))
  }
  Y_mean <- matrix(rep(Y_mean_temp, num_replicates), byrow = F, ncol = num_replicates)

}else{
 
  Z <- NULL
  Y_mean <- NULL

}

Z_dd <- as.ddmatrix(x = Z, bldim = bldim)
Y_mean_dd <- as.ddmatrix(x = Y_mean, bldim = bldim)

Y_dd <- t(Sigma_chol_dd) %*% Z_dd + Y_mean_dd
Y <- as.matrix(Y_dd, proc.dest = 0)

end_time = Sys.time()

TOTAL_TIME <- as.numeric(end_time - start_time, units = "secs")

comm.print(TOTAL_TIME)

if(save_covariance){
  Sigma <- as.matrix(Sigma_dd, proc.dest = 0)
  comm.print(isSymmetric(Sigma))
}

if(comm.rank() == 0){

  print(range(Y))

  if(three_dimensional_space){
    setClass("SyntheticData", representation(measurements = "matrix", covariates = "matrix", locs = "matrix", num_locs = "numeric", num_pressure = "numeric", parameter_values = "vector", parameter_names = "vector"))

    if(p == 1){
      DAT <- new("SyntheticData", measurements = Y, covariates = covariates_full, locs = locs, num_locs = N, num_pressure = pdims, parameter_values = c(c(beta), PARAM), parameter_names = c("beta01", "beta11", "beta21", PARAM_NAMES))
    }else if(p == 2){
      DAT <- new("SyntheticData", measurements = Y, covariates = covariates_full, locs = locs, num_locs = N, num_pressure = pdims, parameter_values = c(c(beta), PARAM), parameter_names = c("beta01", "beta11", "beta21", "beta02", "beta12", "beta22", PARAM_NAMES))
    }
  }else{
    setClass("SyntheticData", representation(measurements = "matrix", covariates = "matrix", locs = "matrix", num_locs = "numeric", num_time = "numeric", parameter_values = "vector", parameter_names = "vector"))

    if(p == 1){
      DAT <- new("SyntheticData", measurements = Y, covariates = covariates_full, locs = locs, num_locs = N, num_time = TT, parameter_values = c(c(beta), PARAM), parameter_names = c("beta01", "beta11", "beta21", PARAM_NAMES))
    }else if(p == 2){
      DAT <- new("SyntheticData", measurements = Y, covariates = covariates_full, locs = locs, num_locs = N, num_time = TT, parameter_values = c(c(beta), PARAM), parameter_names = c("beta01", "beta11", "beta21", "beta02", "beta12", "beta22", PARAM_NAMES))
    }
  }

  if(advection_model){
    save(DAT, file = paste('../data/simulated_data_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME_', DEPENDENCE_IN_VARIABLE, '_DEPENDENCE_IN_VARIABLE_', DEPENDENCE_IN_ADVECTION, '_DEPENDENCE_IN_ADVECTION', '.RData', sep = ""))
    print(paste('Finished generating simulated_data_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME_', DEPENDENCE_IN_VARIABLE, '_DEPENDENCE_IN_VARIABLE_', DEPENDENCE_IN_ADVECTION, '_DEPENDENCE_IN_ADVECTION', '.RData', sep = ""))
  }else if(three_dimensional_space){
    save(DAT, file = paste('../data/simulated_data_', MODEL_NAME, '_N_', N, '_pressure_', pdims, '.RData', sep = ""))
    print(paste('Finished generating simulated_data_', MODEL_NAME, '_N_', N, '_pressure_', pdims, '.RData', sep = ""))
  }

  if(save_covariance){

    setClass("SyntheticCovariance", representation(cov = "matrix", locs = "matrix", num_locs = "numeric", num_time = "numeric", parameter_values = "vector", parameter_names = "vector"))
    DAT <- new("SyntheticCovariance", cov = Sigma[c(1, 880, N + 1, N + 880), ], locs = locs, num_locs = N, num_time = TT, parameter_values = PARAM, parameter_names = PARAM_NAMES)
    if(advection_model){
      save(DAT, file = paste('../data/simulated_covariance_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME', DEPENDENCE_IN_ADVECTION, '_DEPENDENCE_IN_ADVECTION', '.RData', sep = ""))
    print(paste('Finished generating simulated_covariance_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME', DEPENDENCE_IN_ADVECTION, '_DEPENDENCE_IN_ADVECTION', '.RData', sep = ""))
    }else if(three_dimensional_space){
      save(DAT, file = paste('../data/simulated_covariance_', MODEL_NAME, '_N_', N, '.RData', sep = ""))
    print(paste('Finished generating simulated_covariance_', MODEL_NAME, '_N_', N, '.RData', sep = ""))
    }
  }
}

comm.print("DONE . . . ")

finalize()

