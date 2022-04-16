
args <- commandArgs(trailingOnly = TRUE)

setwd(args[1])
DATASET <- args[2]
NUM_TASKS <- as.numeric(args[3])
ESTIMATION_METHOD = args[4]
FIT_MODEL_NUM <- as.numeric(args[5])

source("../../codes/load_packages.R")
comm.print("Done loading packages . . .")

init.grid(NPROW = sqrt(NUM_TASKS), NPCOL = sqrt(NUM_TASKS))
bldim <- c(sqrt(NUM_TASKS), sqrt(NUM_TASKS))


if(FIT_MODEL_NUM == 1){

  FIT_MODEL_NAME = 'univariate_spatial_matern'
  p = 1

  NONSTATIONARY = F
  MULTISTEP = F

  init <- c(log(0.5), log(0.5), log(0.5))
  PARAM_NAMES <- c('variance', 'range', 'smoothness')

}else if(FIT_MODEL_NUM == 2){

  FIT_MODEL_NAME = 'univariate_matern_schlather_spacetime'
  p = 1

  NONSTATIONARY = F
  MULTISTEP = T

  init_space <- c(log(0.5), log(0.5), log(0.5), 0, 0, 0.5, 0, 0.5)
  init_frozen <- c(log(0.5), log(50000), log(1.7), 0, 0)
  init_velocity_variance_chol <- c(6, 0, 6)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}else if(FIT_MODEL_NUM == 3){

  FIT_MODEL_NAME = 'bivariate_matern_parsimonious_spatial'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = F

  init <- c(log(0.5), log(0.5), log(66), log(0.7), log(0.7), 0)
  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation')

}else if(FIT_MODEL_NUM == 4){

  FIT_MODEL_NAME = 'bivariate_matern_single_advection_spacetime'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = T

  init <- c(log(0.5), log(0.5), log(30), log(0.7), log(0.7), 0, 0, 0, 0.5, 0, 0.5)
  init_frozen <- c(log(0.5), log(0.5), log(50000), log(0.7), log(0.7), 0.4, 0, 0)
  init_velocity_variance_chol <- c(6, 0, 6)
  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}else if(FIT_MODEL_NUM == 5){

  FIT_MODEL_NAME = 'bivariate_matern_multiple_advection_spacetime'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = T
 
  init_space <- c(log(0.5), log(0.5), log(66), log(0.7), log(0.7), 0, 0.1, 0.1, -0.2, -0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  init_frozen <- c(log(0.5), log(0.5), log(50000), log(0.7), log(0.7), 0.4, 0, 0, 0, 0)
  init_velocity_variance_chol <- c(6, 6, 6, 6)
  init_velocity_variance_chol_off_diagonal <- c(0, 0, 0, 0, 0, 0)

  PARAM_NAMES <- c('variance1', 'variance2', 'range', 'smoothness1', 'smoothness2', 'colocated_correlation', 'vel1_mean_x', 'vel1_mean_y', 'vel2_mean_x', 'vel2_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_13', 'vel_variance_chol_14', 'vel_variance_chol_22', 'vel_variance_chol_23', 'vel_variance_chol_24', 'vel_variance_chol_33', 'vel_variance_chol_34', 'vel_variance_chol_44')

}else if(FIT_MODEL_NUM == 6){

  FIT_MODEL_NAME = 'bivariate_lmc_spatial'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = F

  init_space <- c(log(0.1), log(0.1), log(0.7), log(0.7), 1, 0, 0, 1)
  PARAM_NAMES <- c('range1', 'range2', 'smoothness1', 'smoothness2', 'A11', 'A12', 'A21', 'A22')

}else if(FIT_MODEL_NUM == 7){
  
  FIT_MODEL_NAME = 'univariate_deformation_matern_frozen_spacetime'
  p = 1

  NONSTATIONARY = T
  MULTISTEP = T

  jWarp = 1:10

  init_space <- c(log(0.5), log(0.5), log(0.5), 0, 0, 0.5, 0, 0.5)
  init_frozen <- c(log(0.5), log(0.5), log(0.5), 0.05, 0.05, rep(0, max(jWarp) * 2))
  init_velocity_variance_chol <- c(2, 0, 2)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'deform_source_x', 'deform_source_y', 'deform_coef_x_1', 'deform_coef_x_2', 'deform_coef_x_3', 'deform_coef_y_1', 'deform_coef_y_2', 'deform_coef_y_3')

}else if(FIT_MODEL_NUM == 8){

  FIT_MODEL_NAME = 'bivariate_lmc_spacetime'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = T

  init_frozen <- c(log(50000), log(50000), log(0.7), log(0.7), 0, 0, 0, 0, 1, 0, 0, 1)
  init_velocity_variance_chol <- c(6, 0, 6, 6, 0, 6)
  PARAM_NAMES <- c('range1', 'range2', 'smoothness1', 'smoothness2', 'vel1_mean_x', 'vel1_mean_y', 'vel2_mean_x', 'vel2_mean_y', 'vel1_variance_chol_11', 'vel1_variance_chol_12', 'vel1_variance_chol_22', 'vel2_variance_chol_11', 'vel2_variance_chol_12', 'vel2_variance_chol_22', 'A11', 'A12', 'A21', 'A22')

}else if(FIT_MODEL_NUM == 10){

  FIT_MODEL_NAME = 'bivariate_matern_bourotte_spacetime'
  p = 2

  NONSTATIONARY = F
  MULTISTEP = F

  init <- c(log(0.5), log(0.5), log(50000), log(0.7), log(0.7), 0.4, log(3), log(0.5), 0.5, log(0.1))
  PARAM_NAMES <- c('variance1', 'variance2', 'range_space', 'smoothness1', 'smoothness2', 'colocated_correlation', 'range_time', 'alpha', 'beta', 'delta')

}else if(FIT_MODEL_NUM == 11){

  FIT_MODEL_NAME = 'univariate_matern_numerical_lagrangian_spacetime'
  p = 1

  NONSTATIONARY = F
  MULTISTEP = T

  init_space <- c(log(0.5), log(0.5), log(0.5), 0, 0, 0.5, 0, 0.5)
  init_frozen <- c(log(0.5), log(0.5), log(0.5), 0, 0)
  init_velocity_variance_chol <- c(2, 0, 2)
  PARAM_NAMES <- c('variance', 'range', 'smoothness', 'vel_mean_x', 'vel_mean_y', 'vel_variance_chol_11', 'vel_variance_chol_12', 'vel_variance_chol_22')

}

FILE_NAME <- paste('../data/', DATASET, '.RData', sep = "")
load(FILE_NAME)
comm.print("Done loading data file . . .")

p_data <- 2

Y <- DAT@measurements
covariates <- DAT@covariates

N <- DAT@num_locs
TT <- DAT@num_time

n <- N * TT
TT_insample <- TT - 1
n_insample <- N * TT_insample

lon.lat_new <- DAT@original_locs[1:N, 1:2]


ICTXT <- 0
dim <- c(n_insample * p, n_insample * p)
ldim <- base.numroc(dim = dim, bldim = bldim, ICTXT = ICTXT)
descx <- base.descinit(dim = dim, bldim = bldim, ldim = ldim, ICTXT = ICTXT)


dim_out <- c(n * p, n * p)
ldim_out <- base.numroc(dim = dim_out, bldim = bldim, ICTXT = ICTXT)
descx_out <- base.descinit(dim = dim_out, bldim = bldim, ldim = ldim_out, ICTXT = ICTXT)

locs <- DAT@locs
locs_insample <- locs[which(locs[, 3] < max(locs[, 3])), ]
locs_outsample <- locs[-which(locs[, 3] < max(locs[, 3])), ]

locs_full_new <- locs_new <- NULL

for(variable in 1:p){
  locs_new <- rbind(locs_new, cbind(locs_insample, rep(variable, n_insample)))
  locs_full_new <- rbind(locs_full_new, cbind(locs, rep(variable, n)))
}

index_in <- NULL
for(variable in 1:p){
  index_in <- c(index_in, n * (variable - 1) + 1:n_insample)
}
index_out_temp <- seq(1, n * p, 1) %in% index_in
index_out <- seq(1, n * p, 1)[!index_out_temp]


if(NONSTATIONARY){

  NN <- 50
  grid_x <- seq(from = -1, to = 2, length.out = NN)
  grid_y <- seq(from = -1, to = 2, length.out = NN)
  locs0 <- expand.grid(grid_x, grid_y) %>% as.matrix()

  nn <- nrow(locs0)

  dim0 <- c(nn, nn)
  ldim0 <- base.numroc(dim = dim0, bldim = bldim, ICTXT = ICTXT)
  descx0 <- base.descinit(dim = dim0, bldim = bldim, ldim = ldim0, ICTXT = ICTXT)

  ## calculating bending energy matrix B;
  # See the texts by Bookstein (1991) or Dryden and Mardia (1998)

  K_sub <- base.crosscovsubmat(model = 0, param = rep(0, 3), x = locs0, descx = descx0)
  K_dd <- new("ddmatrix", Data = K_sub, dim = dim0, ldim = ldim0, bldim = bldim, ICTXT = ICTXT)
  K <- as.matrix(K_dd)

  one <- rep(1, nn)
  Gamma <- cbind(K, one, locs0)
  Gamma <- rbind(Gamma, c(one, 0, 0, 0))
  Gamma <- rbind(Gamma, cbind(t(locs0), matrix(0, 2, 3)))
  Gamma_dd <- as.ddmatrix(x = Gamma, bldim = bldim)
  Ginv <- as.matrix(solve(Gamma_dd))
  Ginv <- (Ginv + t(Ginv))/2  # make exactly symmetric prior to eigen

  B <- Ginv[1:nn, 1:nn]
  Beig <- eigen(B)
  g <- Beig$vectors
  l <- Beig$values
  g <- g[,order(l)]
  l <- l[order(l)]

  dim0_targ <- c(nn + n_insample, nn + n_insample)
  ldim0_targ <- base.numroc(dim = dim0_targ, bldim = bldim, ICTXT = ICTXT)
  descx0_targ <- base.descinit(dim = dim0_targ, bldim = bldim, ldim = ldim0_targ, ICTXT = ICTXT)


}

NEGLOGLIK <- function(theta, X, Y_truth, OLS = T){
  
  param_vals_flag = F

  if(FIT_MODEL_NUM == 1){

    VARIANCE <- exp(theta[1])
    RANGE <- exp(theta[2])
    NU <- exp(theta[3])
    PARAM <- c(VARIANCE, RANGE, NU)

  }else if(FIT_MODEL_NUM == 2){

    VARIANCE <- exp(theta[1])
    RANGE <- exp(theta[2])
    NU <- exp(theta[3])
    VEL_MEAN <- theta[4:5]
    VEL_VARIANCE_CHOL <- theta[6:8]
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)
 
  }else if(FIT_MODEL_NUM == 3){
  
    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO)

  }else if(FIT_MODEL_NUM == 4){

    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    VEL_MEAN <- theta[7:8]
    VEL_VARIANCE_CHOL <- theta[9:11]
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 5){

    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    VEL_MEAN <- theta[7:10]
    VEL_VARIANCE_CHOL <- theta[11:20]
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 6){

    RANGE1 <- exp(theta[1])
    RANGE2 <- exp(theta[2])
    NU1 <- exp(theta[3])
    NU2 <- exp(theta[4])
    A11 <- theta[5]
    A12 <- theta[6]
    A21 <- theta[7]
    A22 <- theta[8]
    PARAM <- c(RANGE1, RANGE2, NU1, NU2, A11, A12, A21, A22)

  }else if(FIT_MODEL_NUM == 10){

    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE_SPACE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    RANGE_TIME <- exp(theta[7])
    ALPHA <- exp(theta[8])
    BETA <- theta[9]
    DELTA <- exp(theta[10])
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE_SPACE, NU1, NU2, RHO, RANGE_TIME, ALPHA, BETA, DELTA)

    if(NU1 > 3 | NU2 > 3 | NU1 < 0.1 | NU2 < 0.1 | abs(RHO) >= 1 | ALPHA == 0 | BETA < 0 | BETA > 1){
      param_vals_flag = T
    }

  }

  comm.print(PARAM)

  if(param_vals_flag){
    return(Inf)
  }else{
  
    Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = PARAM, x = locs_new, descx = descx)
  
    Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim, ldim = ldim, bldim = bldim, ICTXT = ICTXT)
    Sigma_chol_dd <- tryCatch(chol(Sigma_dd), error = function(a) numeric(0))

    if(is.na(min(diag(Sigma_chol_dd)))){
      return(Inf)
    }else if(length(Sigma_chol_dd) == 0 | min(diag(Sigma_chol_dd)) <= 0){
      return(Inf)
    }else{
   
      Sigma_inv_dd <- chol2inv(Sigma_chol_dd)
      Sigma_inv <- as.matrix(Sigma_inv_dd)

      if(ESTIMATION_METHOD == 'REML'){
        if(OLS){
          beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
        }else{
          beta <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% Y_truth
        }
      }else{
        beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
      }

      res <- Y_truth - as.numeric(X %*% beta)

      Data <- as.ddmatrix(x = matrix(res, ncol = 1), bldim = bldim)

      z2 <- as.matrix(t(Data) %*% Sigma_inv_dd %*% Data)
      logsig  <- 2 * sum(log(diag(Sigma_chol_dd)))
      out <- 1/2 * n_insample * p * log(2 * pi) + 1/2 * logsig + 1/2 * as.numeric(z2)

      if(ESTIMATION_METHOD == 'REML'){
        temp1 <- as.matrix(t(covariates_full_dd) %*% Sigma_inv_dd %*% covariates_full_dd)
        out <- out - 1/2 * M * p * log(2 * pi) - 1/2 * covariates_log_det + 1/2 * log(det(temp1))
      }

      return(out)
    }
  }
}

NEGLOGLIK_BIVARIATE_FROZEN <- function(theta, theta_velocity_variance, theta_velocity_variance_off_diagonal = NULL, X, Y_truth, OLS = T){
          
  param_vals_flag = F

  if(FIT_MODEL_NUM %in% c(2, 11)){

    VARIANCE <- exp(theta[1])
    RANGE <- exp(theta[2])
    NU <- exp(theta[3])
    VEL_MEAN <- theta[4:5]
    VEL_VARIANCE_CHOL <- theta_velocity_variance
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)

    if(NU > 3 | NU < 0.1 | RANGE > (200 * 1000)){
      param_vals_flag = T
    }

  }else if(FIT_MODEL_NUM == 4){

    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    VEL_MEAN <- theta[7:8]
    VEL_VARIANCE_CHOL <- theta_velocity_variance
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

    if(VARIANCE1 > 2 | VARIANCE2 > 2 | NU1 > 3 | NU2 > 3 | NU1 < 0.1 | NU2 < 0.1 | abs(RHO) >= 1){
      param_vals_flag = T
    }

  }else if(FIT_MODEL_NUM == 5){

    VARIANCE1 <- exp(theta[1])
    VARIANCE2 <- exp(theta[2])
    RANGE <- exp(theta[3])
    NU1 <- exp(theta[4])
    NU2 <- exp(theta[5])
    RHO <- theta[6]
    VEL_MEAN <- theta[7:10]
    VEL_VARIANCE_CHOL <- c(theta_velocity_variance[1], theta_velocity_variance_off_diagonal[1:3], theta_velocity_variance[2], theta_velocity_variance_off_diagonal[4:5], theta_velocity_variance[3], theta_velocity_variance_off_diagonal[6], theta_velocity_variance[4]) #theta_velocity_variance
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

    if(VARIANCE1 > 2 | VARIANCE2 > 2 | NU1 > 3 | NU2 > 3 | NU1 < 0.1 | NU2 < 0.1 | abs(RHO) >= 1){
      param_vals_flag = T
    }

  }else if(FIT_MODEL_NUM == 7){

    VARIANCE <- exp(theta[1])
    RANGE <- exp(theta[2])
    NU <- exp(theta[3])
    VEL_MEAN <- theta[4:5]
    VEL_VARIANCE_CHOL <- theta_velocity_variance
    DEFORM_PARAM_X <- theta[5 + 1:length(jWarp)]
    DEFORM_PARAM_Y <- theta[5 + length(jWarp) + 1:length(jWarp)]
    PARAM <- c(VARIANCE, RANGE, NU)

    locs_targ <- cbind(locs_insample[, 1] - VEL_MEAN[1] * locs_insample[, 3], locs_insample[, 2] - VEL_MEAN[2] * locs_insample[, 3])
    locs_full_targ <- rbind(locs0, locs_targ)
    K_targ_sub <- base.crosscovsubmat(model = 0, param = rep(0, 3), x = locs_full_targ, descx = descx0_targ)
    K_targ_dd <- new("ddmatrix", Data = K_targ_sub, dim = dim0_targ, ldim = ldim0_targ, bldim = bldim, ICTXT = ICTXT)
    K_targ <- as.matrix(K_targ_dd)
    K_targ <- K_targ[1:nn, nn + 1:n_insample]

    parWarpsSum_x <- rowSums(g[, 3 + jWarp] * matrix(DEFORM_PARAM_X, ncol = length(DEFORM_PARAM_X), nrow = nn, byrow = T))
    parWarpsSum_y <- rowSums(g[, 3 + jWarp] * matrix(DEFORM_PARAM_Y, ncol = length(DEFORM_PARAM_Y), nrow = nn, byrow = T))
    parWarpsSum <- cbind(parWarpsSum_x, parWarpsSum_y)

    locs_deformed <- locs_new[, 1:2] + t(K_targ) %*% parWarpsSum
    locs_deformed <- cbind(locs_deformed, 1)
   
  }else if(FIT_MODEL_NUM == 8){

    RANGE1 <- exp(theta[1])
    RANGE2 <- exp(theta[2])
    NU1 <- exp(theta[3])
    NU2 <- exp(theta[4])
    VEL1_MEAN <- theta[5:6]
    VEL1_VARIANCE_CHOL <- theta_velocity_variance[1:3]
    VEL2_MEAN <- theta[7:8]
    VEL2_VARIANCE_CHOL <- theta_velocity_variance[4:6]
    A11 <- theta[9]
    A12 <- theta[10]
    A21 <- theta[11]
    A22 <- theta[12]
    PARAM <- c(RANGE1, RANGE2, NU1, NU2, VEL1_MEAN, VEL2_MEAN, VEL1_VARIANCE_CHOL, VEL2_VARIANCE_CHOL, A11, A12, A21, A22)

    if(NU1 > 3 | NU2 > 3 | NU1 < 0.1 | NU2 < 0.1){
      param_vals_flag = T
    }

  }

  comm.print(PARAM)

  if(param_vals_flag){
    return(Inf)
  }else{

    if(FIT_MODEL_NUM == 7){
      Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = PARAM, x = locs_deformed, descx = descx)
    }else{
      Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = PARAM, x = locs_new, descx = descx)
    }
    
    Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim, ldim = ldim, bldim = bldim, ICTXT = ICTXT)
    Sigma_chol_dd <- tryCatch(chol(Sigma_dd), error = function(a) numeric(0))

    if(is.na(min(diag(Sigma_chol_dd)))){
      return(Inf)
    }else if(length(Sigma_chol_dd) == 0 | min(diag(Sigma_chol_dd)) <= 0){
      return(Inf)
    }else{
   
      Sigma_inv_dd <- chol2inv(Sigma_chol_dd)
      Sigma_inv <- as.matrix(Sigma_inv_dd)

      if(ESTIMATION_METHOD == 'REML'){
        if(OLS){
          beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
        }else{
          beta <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% Y_truth
        }
      }else{
        beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
      }

      res <- Y_truth - as.numeric(X %*% beta)

      Data <- as.ddmatrix(x = matrix(res, ncol = 1), bldim = bldim)

      z2 <- as.matrix(t(Data) %*% Sigma_inv_dd %*% Data)
      logsig  <- 2 * sum(log(diag(Sigma_chol_dd)))
      out <- 1/2 * n_insample * p * log(2 * pi) + 1/2 * logsig + 1/2 * as.numeric(z2)

      if(ESTIMATION_METHOD == 'REML'){
        temp1 <- as.matrix(t(covariates_full_dd) %*% Sigma_inv_dd %*% covariates_full_dd)
        out <- out - 1/2 * M * p * log(2 * pi) - 1/2 * covariates_log_det + 1/2 * log(det(temp1))
      }

      return(out)
    }
  }
}

NEGLOGLIK_BIVARIATE_NONFROZEN <- function(theta, theta0, theta_velocity_variance_off_diagonal = NULL, X, Y_truth, OLS = T){

  param_vals_flag = F

  if(FIT_MODEL_NUM == 2){

    VARIANCE <- exp(theta0[1])
    RANGE <- exp(theta0[2])
    NU <- exp(theta0[3])
    VEL_MEAN <- theta0[4:5]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 11){

    VARIANCE <- exp(theta0[1])
    RANGE <- exp(theta0[2])
    NU <- exp(theta0[3])
    VEL_MEAN <- theta0[4:5]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 4){

    VARIANCE1 <- exp(theta0[1])
    VARIANCE2 <- exp(theta0[2])
    RANGE <- exp(theta0[3])
    NU1 <- exp(theta0[4])
    NU2 <- exp(theta0[5])
    RHO <- theta0[6]
    VEL_MEAN <- theta0[7:8]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 5){

    VARIANCE1 <- exp(theta0[1])
    VARIANCE2 <- exp(theta0[2])
    RANGE <- exp(theta0[3])
    NU1 <- exp(theta0[4])
    NU2 <- exp(theta0[5])
    RHO <- theta0[6]
    VEL_MEAN <- theta0[7:10]
    VEL_VARIANCE_CHOL <- c(theta[1], theta_velocity_variance_off_diagonal[1:3], theta[2], theta_velocity_variance_off_diagonal[4:5], theta[3], theta_velocity_variance_off_diagonal[6], theta[4])
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 8){

    RANGE1 <- exp(theta0[1])
    RANGE2 <- exp(theta0[2])
    NU1 <- exp(theta0[3])
    NU2 <- exp(theta0[4])
    VEL1_MEAN <- theta0[5:6]
    VEL1_VARIANCE_CHOL <- theta[1:3]
    VEL2_MEAN <- theta0[7:8]
    VEL2_VARIANCE_CHOL <- theta[4:6]
    A11 <- theta0[9]
    A12 <- theta0[10]
    A21 <- theta0[11]
    A22 <- theta0[12]
    PARAM <- c(RANGE1, RANGE2, NU1, NU2, VEL1_MEAN, VEL2_MEAN, VEL1_VARIANCE_CHOL, VEL2_VARIANCE_CHOL, A11, A12, A21, A22)

  }

  comm.print(PARAM)

  if(param_vals_flag){
    return(Inf)
  }else{

    Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = PARAM, x = locs_new, descx = descx)
    Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim, ldim = ldim, bldim = bldim, ICTXT = ICTXT)
    Sigma_chol_dd <- tryCatch(chol(Sigma_dd), error = function(a) numeric(0))
   
    if(is.na(min(diag(Sigma_chol_dd)))){
      return(Inf)
    }else if(length(Sigma_chol_dd) == 0 | min(diag(Sigma_chol_dd)) <= 0){
      return(Inf)
    }else{

      Sigma_inv_dd <- chol2inv(Sigma_chol_dd)
      Sigma_inv <- as.matrix(Sigma_inv_dd)

      if(ESTIMATION_METHOD == 'REML'){
        if(OLS){
          beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
        }else{
          beta <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% Y_truth
        }
      }else{
        beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
      }

      res <- Y_truth - as.numeric(X %*% beta)

      Data <- as.ddmatrix(x = matrix(res, ncol = 1), bldim = bldim)

      z2 <- as.matrix(t(Data) %*% Sigma_inv_dd %*% Data)
      logsig  <- 2 * sum(log(diag(Sigma_chol_dd)))
      out <- 1/2 * n_insample * p * log(2 * pi) + 1/2 * logsig + 1/2 * as.numeric(z2)

      if(ESTIMATION_METHOD == 'REML'){
        temp1 <- as.matrix(t(covariates_full_dd) %*% Sigma_inv_dd %*% covariates_full_dd)
        out <- out - 1/2 * M * p * log(2 * pi) - 1/2 * covariates_log_det + 1/2 * log(det(temp1))
      }

      return(out)
    }
  }
}

NEGLOGLIK_BIVARIATE_NONFROZEN_OFF_DIAGONAL <- function(theta, theta0, theta_velocity_variance, X, Y_truth, OLS = T){

  param_vals_flag = F

  if(FIT_MODEL_NUM == 2){

    VARIANCE <- exp(theta0[1])
    RANGE <- exp(theta0[2])
    NU <- exp(theta0[3])
    VEL_MEAN <- theta0[4:5]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)
   
  }else if(FIT_MODEL_NUM == 11){

    VARIANCE <- exp(theta0[1])
    RANGE <- exp(theta0[2])
    NU <- exp(theta0[3])
    VEL_MEAN <- theta0[4:5]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 4){

    VARIANCE1 <- exp(theta0[1])
    VARIANCE2 <- exp(theta0[2])
    RANGE <- exp(theta0[3])
    NU1 <- exp(theta0[4])
    NU2 <- exp(theta0[5])
    RHO <- theta0[6]
    VEL_MEAN <- theta0[7:8]
    VEL_VARIANCE_CHOL <- theta
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 5){

    VARIANCE1 <- exp(theta0[1])
    VARIANCE2 <- exp(theta0[2])
    RANGE <- exp(theta0[3])
    NU1 <- exp(theta0[4])
    NU2 <- exp(theta0[5])
    RHO <- theta0[6]
    VEL_MEAN <- theta0[7:10]
    VEL_VARIANCE_CHOL <- c(theta_velocity_variance[1], theta[1:3], theta_velocity_variance[2], theta[4:5], theta_velocity_variance[3], theta[6], theta_velocity_variance[4])
    PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)

  }else if(FIT_MODEL_NUM == 8){

    RANGE1 <- exp(theta0[1])
    RANGE2 <- exp(theta0[2])
    NU1 <- exp(theta0[3])
    NU2 <- exp(theta0[4])
    VEL1_MEAN <- theta0[5:6]
    VEL1_VARIANCE_CHOL <- theta[1:3]
    VEL2_MEAN <- theta0[7:8]
    VEL2_VARIANCE_CHOL <- theta[4:6]
    A11 <- theta0[9]
    A12 <- theta0[10]
    A21 <- theta0[11]
    A22 <- theta0[12]
    PARAM <- c(RANGE1, RANGE2, NU1, NU2, VEL1_MEAN, VEL2_MEAN, VEL1_VARIANCE_CHOL, VEL2_VARIANCE_CHOL, A11, A12, A21, A22)

  }

  comm.print(PARAM)

  if(param_vals_flag){
    return(Inf)
  }else{

    Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = PARAM, x = locs_new, descx = descx)
    Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim, ldim = ldim, bldim = bldim, ICTXT = ICTXT)
    Sigma_chol_dd <- tryCatch(chol(Sigma_dd), error = function(a) numeric(0))
   
    if(is.na(min(diag(Sigma_chol_dd)))){
      return(Inf)
    }else if(length(Sigma_chol_dd) == 0 | min(diag(Sigma_chol_dd)) <= 0){
      return(Inf)
    }else{

      Sigma_inv_dd <- chol2inv(Sigma_chol_dd)
      Sigma_inv <- as.matrix(Sigma_inv_dd)

      if(ESTIMATION_METHOD == 'REML'){
        if(OLS){
          beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
        }else{
          beta <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% Y_truth
        }
      }else{
        beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
      }

      res <- Y_truth - as.numeric(X %*% beta)

      Data <- as.ddmatrix(x = matrix(res, ncol = 1), bldim = bldim)

      z2 <- as.matrix(t(Data) %*% Sigma_inv_dd %*% Data)
      logsig  <- 2 * sum(log(diag(Sigma_chol_dd)))
      out <- 1/2 * n_insample * p * log(2 * pi) + 1/2 * logsig + 1/2 * as.numeric(z2)

      if(ESTIMATION_METHOD == 'REML'){
        temp1 <- as.matrix(t(covariates_full_dd) %*% Sigma_inv_dd %*% covariates_full_dd)
        out <- out - 1/2 * M * p * log(2 * pi) - 1/2 * covariates_log_det + 1/2 * log(det(temp1))
      }

      return(out)
    }
  }
}

start_time = Sys.time()

EST_FUNCTIONS <- function(Y_insample, Y_outsample, covariates_insample, covariates_outsample){

  if(MULTISTEP){
   
    if(FIT_MODEL_NUM == 5){ 
      fit1 <- optim(par = init_frozen, fn = NEGLOGLIK_BIVARIATE_FROZEN, theta_velocity_variance = init_velocity_variance_chol, theta_velocity_variance_off_diagonal = init_velocity_variance_chol_off_diagonal, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
      fit2 <- optim(par = init_velocity_variance_chol, fn = NEGLOGLIK_BIVARIATE_NONFROZEN, theta0 = fit1$par, theta_velocity_variance_off_diagonal = init_velocity_variance_chol_off_diagonal, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
      fit3 <- optim(par = init_velocity_variance_chol_off_diagonal, fn = NEGLOGLIK_BIVARIATE_NONFROZEN_OFF_DIAGONAL, theta0 = fit1$par, theta_velocity_variance = fit2$par, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
    }else{
      fit1 <- optim(par = init_frozen, fn = NEGLOGLIK_BIVARIATE_FROZEN, theta_velocity_variance = init_velocity_variance_chol, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
      fit2 <- optim(par = init_velocity_variance_chol, fn = NEGLOGLIK_BIVARIATE_NONFROZEN, theta0 = fit1$par, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
    }

    for(round in 1:10){
      
      if(FIT_MODEL_NUM == 5){ 
        fit1 <- optim(par = fit1$par, fn = NEGLOGLIK_BIVARIATE_FROZEN, theta_velocity_variance = fit2$par, theta_velocity_variance_off_diagonal = fit3$par, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
        fit2 <- optim(par = fit2$par, fn = NEGLOGLIK_BIVARIATE_NONFROZEN, theta0 = fit1$par, theta_velocity_variance_off_diagonal = fit3$par, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
        fit3 <- optim(par = fit3$par, fn = NEGLOGLIK_BIVARIATE_NONFROZEN_OFF_DIAGONAL, theta0 = fit1$par, theta_velocity_variance = fit2$par, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
      }else{
        fit1 <- optim(par = fit1$par, fn = NEGLOGLIK_BIVARIATE_FROZEN, theta_velocity_variance = fit2$par, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
        fit2 <- optim(par = fit2$par, fn = NEGLOGLIK_BIVARIATE_NONFROZEN, theta0 = fit1$par, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
      }
      
    }
    
    if(FIT_MODEL_NUM == 5){ 
      est_params <- c(fit1$par, fit2$par, fit3$par)
      loglikelihood_value <- -fit3$value    #take the negative because the value from the optim is the negative loglikelihood
    }else{
      est_params <- c(fit1$par, fit2$par)
      loglikelihood_value <- -fit2$value    #take the negative because the value from the optim is the negative loglikelihood
    }
    
  }else{
    fit1 <- optim(par = init, fn = NEGLOGLIK, X = covariates_insample, Y_truth = Y_insample, OLS = T, control = list(trace = 5, maxit = 500)) 
    for(round in 1:10){
      fit1 <- optim(par = fit1$par, fn = NEGLOGLIK, X = covariates_insample, Y_truth = Y_insample, OLS = F, control = list(trace = 5, maxit = 500)) 
    }

    est_params <- fit1$par
    loglikelihood_value <- -fit1$value    #take the negative because the value from the optim is the negative loglikelihood
  }
    
  if(FIT_MODEL_NUM %in% c(2, 11)){
      
    VARIANCE <- exp(est_params[1])
    RANGE <- exp(est_params[2])
    NU <- exp(est_params[3])
    VEL_MEAN <- est_params[4:5]
    VEL_VARIANCE_CHOL <- est_params[6:8]
    EST_PARAM <- c(VARIANCE, RANGE, NU, VEL_MEAN, VEL_VARIANCE_CHOL)
      
  }else if(FIT_MODEL_NUM == 4){
      
    VARIANCE1 <- exp(est_params[1])
    VARIANCE2 <- exp(est_params[2])
    RANGE <- exp(est_params[3])
    NU1 <- exp(est_params[4])
    NU2 <- exp(est_params[5])
    RHO <- est_params[6]
    VEL_MEAN <- est_params[7:8]
    VEL_VARIANCE_CHOL <- est_params[9:11]
    EST_PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)
      
  }else if(FIT_MODEL_NUM == 5){
      
    VARIANCE1 <- exp(est_params[1])
    VARIANCE2 <- exp(est_params[2])
    RANGE <- exp(est_params[3])
    NU1 <- exp(est_params[4])
    NU2 <- exp(est_params[5])
    RHO <- est_params[6]
    VEL_MEAN <- est_params[7:10]
    VEL_VARIANCE_CHOL <- c(est_params[11], est_params[15:17], est_params[12], est_params[18:19], est_params[13], est_params[20], est_params[14])
    EST_PARAM <- c(VARIANCE1, VARIANCE2, RANGE, NU1, NU2, RHO, VEL_MEAN, VEL_VARIANCE_CHOL)
      
  }else if(FIT_MODEL_NUM == 8){
      
    RANGE1 <- exp(est_params[1])
    RANGE2 <- exp(est_params[2])
    NU1 <- exp(est_params[3])
    NU2 <- exp(est_params[4])
    VEL1_MEAN <- est_params[5:6]
    VEL1_VARIANCE_CHOL <- est_params[13:15]
    VEL2_MEAN <- est_params[7:8]
    VEL2_VARIANCE_CHOL <- est_params[16:18]
    A11 <- est_params[9]
    A12 <- est_params[10]
    A21 <- est_params[11]
    A22 <- est_params[12]
    EST_PARAM <- c(RANGE1, RANGE2, NU1, NU2, VEL1_MEAN, VEL2_MEAN, VEL1_VARIANCE_CHOL, VEL2_VARIANCE_CHOL, A11, A12, A21, A22)
      
  }else if(FIT_MODEL_NUM == 10){
    
    VARIANCE1 <- exp(est_params[1])
    VARIANCE2 <- exp(est_params[2])
    RANGE_SPACE <- exp(est_params[3])
    NU1 <- exp(est_params[4])
    NU2 <- exp(est_params[5])
    RHO <- est_params[6]
    RANGE_TIME <- exp(est_params[7])
    ALPHA <- exp(est_params[8])
    BETA <- est_params[9]
    DELTA <- exp(est_params[10])
    EST_PARAM <- c(VARIANCE1, VARIANCE2, RANGE_SPACE, NU1, NU2, RHO, RANGE_TIME, ALPHA, BETA, DELTA)
    
  }
    
  Sigma_sub <- base.crosscovsubmat(model = FIT_MODEL_NUM, param = EST_PARAM, x = locs_full_new, descx = descx_out)
  Sigma_dd <- new("ddmatrix", Data = Sigma_sub, dim = dim_out, ldim = ldim_out, bldim = bldim, ICTXT = ICTXT)

  Sigma_insample_dd <- Sigma_dd[index_in, index_in]
  Sigma_outsample_dd <- Sigma_dd[index_out, index_out]
  Sigma_inoutsample_dd <- Sigma_dd[index_in, index_out]

  Sigma_inv <- as.matrix(solve(Sigma_insample_dd))

  X <- covariates_insample
  Y_truth <- Y_insample
    
  if(ESTIMATION_METHOD == 'REML'){
    beta <- solve(t(X) %*% Sigma_inv %*% X) %*% t(X) %*% Sigma_inv %*% Y_truth
  }else{
    beta <- solve(t(X) %*% X) %*% t(X) %*% Y_truth
  }
  res <- Y_truth - as.numeric(X %*% beta)

  X <- covariates_outsample
  Y_truth <- Y_outsample
  res_out <- Y_truth - as.numeric(X %*% beta)

  Z_insample_dd <- as.ddmatrix(x = matrix(res, ncol = 1), bldim = bldim)
  Z_outsample_dd <- as.ddmatrix(x = matrix(res_out, ncol = 1), bldim = bldim)
    
  pred_dd <- t(Sigma_inoutsample_dd) %*% solve(Sigma_insample_dd) %*% Z_insample_dd
  error <- pred_dd - Z_outsample_dd
  MSE <- as.numeric(as.matrix(t(error) %*% error)) / length(index_out)
    
  AIC <- -2 * loglikelihood_value + 2 * length(EST_PARAM)
  BIC <- -2 * loglikelihood_value + log(n_insample * p) * length(EST_PARAM)

  setClass("EstimationResults", representation(est_params = "vector", loglikelihood_value = "numeric", MSE = "numeric", AIC = "numeric", BIC = "numeric"))
  
  DAT <- new("EstimationResults", est_params = EST_PARAM, loglikelihood_value = loglikelihood_value, MSE = MSE, AIC = AIC, BIC = BIC)

  return(DAT)
}

Y_insample <- matrix(Y[index_in, ], ncol = 1)
if(p == 1){
  covariates_insample <- covariates[index_in, 1:2]
}else{
  covariates_insample <- covariates[index_in, ]
}

Y_outsample <- matrix(Y[index_out, ], ncol = 1)
if(p == 1){
  covariates_outsample <- covariates[index_out, 1:2]
}else{
  covariates_outsample <- covariates[index_out, ]
}

if(ESTIMATION_METHOD == 'REML'){
  covariates_log_det <- log(det(t(covariates_insample) %*% covariates_insample))
  covariates_full_dd <- as.ddmatrix(x = covariates_insample, bldim = bldim)
  M <- ncol(covariates_full_dd)
}

est_run <- EST_FUNCTIONS(Y_insample, Y_outsample, covariates_insample, covariates_outsample)

est_params <- est_run@est_params
loglikelihood_value <- est_run@loglikelihood_value
MSE <- est_run@MSE
AIC <- est_run@AIC
BIC <- est_run@BIC

if(p_data > p){

  Y_insample <- matrix(Y[n + index_in, ], ncol = 1)
  if(p == 1){
    covariates_insample <- covariates[n + index_in, 3:4]
  }else{
    covariates_insample <- covariates[n + index_in, ]
  }

  Y_outsample <- matrix(Y[n + index_out, ], ncol = 1)
  if(p == 1){
    covariates_outsample <- covariates[n + index_out, 3:4]
  }else{
   covariates_outsample <- covariates[n + index_out, ]
  }

  est_run <- EST_FUNCTIONS(Y_insample, Y_outsample, covariates_insample, covariates_outsample)

  est_params <- c(est_params, est_run@est_params)
  loglikelihood_value <- loglikelihood_value + est_run@loglikelihood_value
  MSE <- MSE + est_run@MSE
  AIC <- AIC + est_run@AIC
  BIC <- BIC + est_run@BIC


}

end_time = Sys.time()

TOTAL_TIME <- as.numeric(end_time - start_time, units = "secs")

comm.print(TOTAL_TIME)

if(comm.rank() == 0){
  
  file_name = paste('../results/est_params_real_data_fitted_model_', FIT_MODEL_NAME, '_', ESTIMATION_METHOD, sep = "")

  RESULT <- matrix(c(MSE, AIC, BIC, loglikelihood_value, TOTAL_TIME, est_params, DATASET), nrow = 1) 

  if(p_data > p){
    colnames(RESULT) <- c('MSE', 'AIC', 'BIC', 'loglikelihood_value', 'total_time', paste("est_", PARAM_NAMES, sep = ''), paste("est_", PARAM_NAMES, sep = ''), 'DATASET')
  }else if(p_data == p & p == 1){
    colnames(RESULT) <- c('MSE', 'AIC', 'BIC', 'loglikelihood_value', 'total_time', paste("est_", PARAM_NAMES, sep = ''), 'DATASET')
  }else if(p_data == p & p == 2){
    colnames(RESULT) <- c('MSE', 'AIC', 'BIC', 'loglikelihood_value', 'total_time', paste("est_", PARAM_NAMES, sep = ''), 'DATASET')
  }

  write.table(RESULT, file = file_name, sep = " ", row.names = FALSE, col.names = !file.exists(file_name), quote = FALSE, append = T)

}

comm.print("DONE . . . ")

finalize()

