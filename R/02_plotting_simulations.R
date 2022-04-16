
args <- commandArgs(trailingOnly = TRUE)
setwd(args[1])

source("./load_packages-IBEX.R")
source("./auxiliary_functions.R")

MODEL_NUM <- as.numeric(args[2])

if(MODEL_NUM == 1){

  MODEL_NAME = 'univariate_matern_spatial'
  locs_in_degrees = F

}else if(MODEL_NUM == 2){

  MODEL_NAME = 'univariate_matern_schlather_spacetime'
  locs_in_degrees = F

}else if(MODEL_NUM == 3){
  
  MODEL_NAME = 'bivariate_matern_parsimonious_spatial'
  locs_in_degrees = F

}else if(MODEL_NUM == 4){
  
  MODEL_NAME = 'bivariate_matern_single_advection_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 5){
  
  MODEL_NAME = 'bivariate_matern_multiple_advection_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 6){
  
  MODEL_NAME = 'bivariate_lmc_spatial'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 7){
  
  MODEL_NAME = 'univariate_deformation_matern_frozen_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 8){
  
  MODEL_NAME = 'bivariate_lmc_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 9){
  
  MODEL_NAME = 'univariate_matern_gneiting_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 10){
  
  MODEL_NAME = 'bivariate_matern_bourotte_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 11){
  
  MODEL_NAME = 'univariate_matern_numerical_lagrangian_spacetime'
  locs_in_degrees = F
  
}else if(MODEL_NUM == 12){
  
  MODEL_NAME = 'bivariate_differential_operator_spatial'
  locs_in_degrees = T
}

N <- 550 #2500

if(MODEL_NUM == 12){
  pdims <- 10
  FILE_NAME <- paste('../data/simulated_data_', MODEL_NAME, '_N_', N, '_pressure_', pdims, '.RData', sep = "")
}else{
  TT <- 5
  DEPENDENCE_IN_SPACE <- 'STRONG'
  DEPENDENCE_IN_TIME <- 'STRONG'
  DEPENDENCE_IN_VARIABLE <- 0.6
  DEPENDENCE_IN_ADVECTION <- 'POSITIVE'
  FILE_NAME <- paste('../data/simulated_data_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME_', DEPENDENCE_IN_VARIABLE, '_DEPENDENCE_IN_VARIABLE_', DEPENDENCE_IN_ADVECTION, '_DEPENDENCE_IN_ADVECTION', '.RData', sep = "")
}

load(FILE_NAME)

Y <- DAT@simulations[, 1]
LOCS <- DAT@locs

zlim_range <- range(Y)

PDF_NAME <- paste('../figures/simulated_data_', MODEL_NAME, '_N_', N, '_T_', TT, '_', DEPENDENCE_IN_SPACE, '_DEPENDENCE_IN_SPACE_', DEPENDENCE_IN_TIME, '_DEPENDENCE_IN_TIME', '.pdf', sep = "")
#PDF_NAME <- paste('../figures/simulated_data_', MODEL_NAME, '.pdf', sep = "")

if(MODEL_NUM == 1){
  
  quilt.plot(LOCS[, 1], LOCS[, 2], Y)
  
}else if(MODEL_NUM %in% c(2, 7, 9, 11)){
  
  pdf(PDF_NAME, width = 15, height = 4)
  
  split.screen( rbind(c(0.05,0.96,0.05,0.97), c(0.94,0.99,0.05,0.97)))
  split.screen( figs = c( 1, 5 ), screen = 1 )
  
  for(tt in 1:5){
    
    screen(2 + tt)
    par(pty = 's')
    par(mai=c(0.2,0.2,0.2,0.2))
    
    quilt.plot(LOCS[(tt - 1) * N + 1:N, 1], LOCS[(tt - 1) * N + 1:N, 2], Y[(tt - 1) * N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F, zlim = zlim_range)
    axis(1, at = seq(0, 1, length.out = 5), labels = as.character(seq(0, 1, length.out = 5)), cex.axis = 1)
    
    if(tt == 1){
      axis(2, at = seq(0, 1, length.out = 5), labels = as.character(seq(0, 1, length.out = 5)), cex.axis = 1)
      mtext(expression(s[y]), side = 2, line = 2.5, adj = 0.5, cex = 1.5)
    }
    
    mtext(paste("t = ", tt, sep = ""), side = 3, line = 1, adj = 0.5, cex = 2, font = 2)
    mtext(expression(s[x]), side = 1, line = 2.5, adj = 0.5, cex = 1.5)
    
  }
  
  screen(2)
  x1 <- c(0.15,0.2,0.2,0.15) + 0.1
  y1 <- c(0.25,0.25,0.75,0.75)
  legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(zlim_range[1], zlim_range[2], length.out = 5), 1), CEX = 0.8)
  
  close.screen( all=TRUE)
  dev.off()
    
}else if(MODEL_NUM %in% c(3, 6)){
 
  pdf(PDF_NAME, width = 10, height = 5)
  
  split.screen( rbind(c(0.05,0.93,0.1,0.94), c(0.94,0.99,0.1,0.94)))
  split.screen( figs = c( 1, 2 ), screen = 1 )
  
  for(variable_label in 1:2){
    
    screen(variable_label + 2)
    par(pty = 's')
    par(mai=c(0.2,0.2,0.2,0.2))
    
    quilt.plot(LOCS[, 1], LOCS[, 2], Y[(variable_label - 1) * N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F, zlim = zlim_range)
    axis(1, at = seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5), labels = as.character(seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5)), cex.axis = 1)
    
    mtext(paste("Variable ", variable_label, sep = ""), side = 3, line = 1, adj = 0.5, cex = 2, font = 2)
    
    if(locs_in_degrees){
      mtext('Longitude', side = 1, line = 2.5, adj = 0.5, cex = 1.5)
    }else{
      mtext(expression(s[x]), side = 1, line = 2.5, adj = 0.5, cex = 1.5)
    }
   
    if(variable_label == 1){
      axis(2, at = seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5), labels = as.character(seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5)), cex.axis = 1)
      if(locs_in_degrees){
        mtext('Latitude', side = 2, line = 2.5, adj = 0.5, cex = 1.5)
      }else{
        mtext(expression(s[y]), side = 2, line = 2.5, adj = 0.5, cex = 1.5)
      }
    }
  }
  
  screen(2)
  x1 <- c(0.01,0.06,0.06,0.01)
  y1 <- c(0.25,0.25,0.75,0.75)
  legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(zlim_range[1], zlim_range[2], length.out = 5), 1), CEX = 0.8)
  
  close.screen( all=TRUE)
  dev.off()
   
}else if(MODEL_NUM %in% c(4, 5, 8, 10)){
  
  pdf(PDF_NAME, width = 16, height = 7)
  
  split.screen( rbind(c(0.08,0.96,0.05,0.97), c(0.94,0.99,0.05,0.97)))
  split.screen( figs = c( 2, 5 ), screen = 1 )
  
  for(variable_label in 1:2){
  
    for(tt in 1:5){
      
      screen(2 + (variable_label - 1) * 5 + tt)
      par(pty = 's')
      par(mai=c(0.2,0.2,0.2,0.2))
      
      quilt.plot(LOCS[(tt - 1) * N + 1:N, 1], LOCS[(tt - 1) * N + 1:N, 2], Y[(variable_label - 1) * N * TT + (tt - 1) * N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F, zlim = zlim_range)
      
      if(tt == 1){
        axis(2, at = seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5), labels = as.character(seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5)), cex.axis = 1)
        mtext(expression(s[y]), side = 2, line = 2.5, adj = 0.5, cex = 1.5)
        mtext(paste("Variable ", variable_label, sep = ""), side = 2, line = 4.5, adj = 0.5, cex = 2, font = 2)
      }
      
      if(variable_label == 1){
        mtext(paste("t = ", tt, sep = ""), side = 3, line = 1, adj = 0.5, cex = 2, font = 2)
      }
      if(variable_label == 2){
        axis(1, at = seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5), labels = as.character(seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5)), cex.axis = 1)
        mtext(expression(s[x]), side = 1, line = 2.5, adj = 0.5, cex = 1.5)
      }
    }
    
  }
  
  screen(2)
  x1 <- c(0.15,0.2,0.2,0.15) + 0.1
  y1 <- c(0.25,0.25,0.75,0.75)
  legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(zlim_range[1], zlim_range[2], length.out = 5), 1), CEX = 0.8)
  
  close.screen( all=TRUE)
  dev.off()
  
}else if(MODEL_NUM == 12){
  
  library(rgl)
  
  xdims <- sqrt(N)
  ydims <- sqrt(N)
  
  cov_val <- cbind(LOCS, Y[1:(N * pdims)], Y[N * pdims + 1:(N * pdims)])
  
  open3d(windowRect = c(1587, 497, 2743, 1002))
  #par3d("windowRect") to check dimensions of the rectangle
  
  rgl.clear()
  
  mfrow3d(1, 2)
  
  ng = 5
  vertex  <- c(0, 0, 0)
  
  ref_lat <- -1.836735 #the one nearest 0 degrees
  ref_long <- 84.49 #the one nearest to 90 degrees
  ref_long2 <- -180
  
  long <- matrix(seq(-180, 180, len = 50 * ng) * pi / 180, 50 * ng, 50 * ng, byrow = F)
  lat <- matrix(seq(-90, 90, len = 50 * ng) * pi / 180, 50 * ng, 50 * ng, byrow = T)
  
  x <- vertex[1] + 10 * cos(lat) * cos(long)
  y <- vertex[2] + 10 * cos(lat) * sin(long)
  z <- vertex[3] + 10 * sin(lat)
  
  ## Y1
  
  M <- matrix(cov_val[which(cov_val[, 3] == 0), 4], xdims, ydims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, ydims * ng)
  
  x_sub <- x[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  y_sub <- y[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  z_sub <- z[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  cols_mat_sub <- cols_mat[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  
  persp3d(x_sub, y_sub, z_sub, specular = "white", add = T, color = cols_mat_sub, lit = FALSE, xlab = 'x', ylab = 'y', zlab = 'z')
  
  x_sub <- x[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  y_sub <- y[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  z_sub <- z[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  cols_mat_sub <- cols_mat[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  
  persp3d(x_sub, y_sub, z_sub, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  cov_val_ordered_inner_to_outer <- matrix(, ncol = ncol(cov_val), nrow = nrow(cov_val))
  for(pres in 1:pdims){
    cov_val_ordered_inner_to_outer[(pres - 1) * (xdims * ydims) + 1:(xdims * ydims), ] <- cov_val[which(cov_val[, 3] == max(cov_val[, 3]) - pres + 1), ]
  }
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 2], 2) == -1.84), 4], byrow = F, nrow = xdims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  radius <- matrix(seq(0.01, pdims, len = pdims * ng), xdims * ng, pdims * ng, byrow = T)
  
  x_new <- vertex[1] + radius[which(long[, 1] >= ref_long * pi / 180), ] * cos(ref_lat * pi / 180) * cos(long[which(long[, 1] >= ref_long * pi / 180), 1])
  y_new <- vertex[2] + radius[which(long[, 1] >= ref_long * pi / 180), ] * cos(ref_lat * pi / 180) * sin(long[which(long[, 1] >= ref_long * pi / 180), 1])
  z_new <- vertex[3] + radius[which(long[, 1] >= ref_long * pi / 180), ] * sin(ref_lat * pi / 180)
  
  cols_mat_sub <- cols_mat[which(long[, 1] >= ref_long * pi / 180), ]
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 1], 2) == 84.49), 4], byrow = F, nrow = ydims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  cols_mat_sub <- cols_mat[which(lat[1, ] >= ref_lat * pi / 180), ]
  
  x_new <- vertex[1] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * cos(ref_long * pi / 180)
  y_new <- vertex[2] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * sin(ref_long * pi / 180)
  z_new <- vertex[3] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * sin(lat[1, which(lat[1, ] >= ref_lat * pi / 180)])
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 1], 2) == -180.00), 4], byrow = F, nrow = ydims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  cols_mat_sub <- cols_mat[which(lat[1, ] >= ref_lat * pi / 180), ]
  
  x_new <- vertex[1] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * cos(ref_long2 * pi / 180)
  y_new <- vertex[2] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * sin(ref_long2 * pi / 180)
  z_new <- vertex[3] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * sin(lat[1, which(lat[1, ] >= ref_lat * pi / 180)])
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  view3d(userMatrix=rotationMatrix(2*pi * 54 / 90, 0.3, -0.8, -2), zoom = 0.9)
  
  bgplot3d(gradient_legend_3d(c(cov_val[, 4:5]), pnts = c(0.87, 0.15, 0.88, 0.4), plot_title = expression(Y[1])))
  
  ## Y2
  
  next3d(reuse=FALSE)
  
  M <- matrix(cov_val[which(cov_val[, 3] == 0), 5], xdims, ydims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, ydims * ng)
  
  x_sub <- x[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  y_sub <- y[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  z_sub <- z[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  cols_mat_sub <- cols_mat[which(long[, 1] <= ceil(ref_long) * pi / 180), ]
  
  persp3d(x_sub, y_sub, z_sub, specular = "white", add = T, color = cols_mat_sub, lit = FALSE, xlab = 'x', ylab = 'y', zlab = 'z')
  
  x_sub <- x[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  y_sub <- y[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  z_sub <- z[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  cols_mat_sub <- cols_mat[which(long[, 1] >= floor(ref_long) * pi / 180), which(lat[1, ] <= 0)]
  
  persp3d(x_sub, y_sub, z_sub, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  cov_val_ordered_inner_to_outer <- matrix(, ncol = ncol(cov_val), nrow = nrow(cov_val))
  for(pres in 1:pdims){
    cov_val_ordered_inner_to_outer[(pres - 1) * (xdims * ydims) + 1:(xdims * ydims), ] <- cov_val[which(cov_val[, 3] == max(cov_val[, 3]) - pres + 1), ]
  }
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 2], 2) == -1.84), 5], byrow = F, nrow = xdims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  radius <- matrix(seq(0.01, pdims, len = pdims * ng), xdims * ng, pdims * ng, byrow = T)
  
  x_new <- vertex[1] + radius[which(long[, 1] >= ref_long * pi / 180), ] * cos(ref_lat * pi / 180) * cos(long[which(long[, 1] >= ref_long * pi / 180), 1])
  y_new <- vertex[2] + radius[which(long[, 1] >= ref_long * pi / 180), ] * cos(ref_lat * pi / 180) * sin(long[which(long[, 1] >= ref_long * pi / 180), 1])
  z_new <- vertex[3] + radius[which(long[, 1] >= ref_long * pi / 180), ] * sin(ref_lat * pi / 180)
  
  cols_mat_sub <- cols_mat[which(long[, 1] >= ref_long * pi / 180), ]
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 1], 2) == 84.49), 5], byrow = F, nrow = ydims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  cols_mat_sub <- cols_mat[which(lat[1, ] >= ref_lat * pi / 180), ]
  
  x_new <- vertex[1] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * cos(ref_long * pi / 180)
  y_new <- vertex[2] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * sin(ref_long * pi / 180)
  z_new <- vertex[3] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * sin(lat[1, which(lat[1, ] >= ref_lat * pi / 180)])
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  M <- matrix(cov_val_ordered_inner_to_outer[which(round(cov_val_ordered_inner_to_outer[, 1], 2) == -180.00), 5], byrow = F, nrow = ydims, ncol = pdims)
  cols_vals <- kronecker(M, matrix(1, ng, ng))
  cols_mat <- matrix(color.gradient(cols_vals, xlim = range(cov_val[, 4:5])), xdims * ng, pdims * ng)
  
  cols_mat_sub <- cols_mat[which(lat[1, ] >= ref_lat * pi / 180), ]
  
  x_new <- vertex[1] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * cos(ref_long2 * pi / 180)
  y_new <- vertex[2] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * cos(lat[1, which(lat[1, ] >= ref_lat * pi / 180)]) * sin(ref_long2 * pi / 180)
  z_new <- vertex[3] + radius[which(lat[1, ] >= ref_lat * pi / 180), ] * sin(lat[1, which(lat[1, ] >= ref_lat * pi / 180)])
  
  persp3d(x_new, y_new, z_new, specular = "white", add = T, color = cols_mat_sub, lit = FALSE)
  
  view3d(userMatrix=rotationMatrix(2*pi * 54 / 90, 0.3, -0.8, -2), zoom = 0.9)
  
  bgplot3d(gradient_legend_3d(c(cov_val[, 4:5]), pnts = c(0.87, 0.15, 0.88, 0.4), plot_title = expression(Y[2])))
  
  snapshot3d("../figures/simulated_data_ex6.png")
}


##############################   PLOTTING COVARIANCE OF MODEL 12   #############################

N = 3600
FILE_NAME <- paste('../data/simulated_covariance_', MODEL_NAME, '_N_', N, '.RData', sep = "")
load(FILE_NAME)

Y <- DAT@cov
LOCS <- DAT@locs

PDF_NAME <- paste('../figures/simulated_covariance_', MODEL_NAME, '.pdf', sep = "")
pdf(PDF_NAME, width = 12, height = 8)

split.screen( rbind(c(0.06,0.98,0.05,0.97), c(0.98,0.99,0.05,0.97)))
split.screen( figs = c( 2, 3 ), screen = 1 )

screen(3)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[1, 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
axis(2, at = seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5), labels = as.character(seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5)), cex.axis = 1)
mtext('Latitude', side = 2, line = 2.5, adj = 0.5, cex = 1)
mtext('C11', side = 3, line = 1, adj = 0.5, cex = 1.5, font = 2)
mtext('Reference Location: (-90, 90)', side = 2, line = 4, adj = 0.5, cex = 1, col = 'blue', font = 2)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[1, 1:N]), max(Y[1, 1:N]), length.out = 5), 1), CEX = 0.8)


screen(4)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[3, N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
mtext('C22', side = 3, line = 1, adj = 0.5, cex = 1.5, font = 2)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[3, N + 1:N]), max(Y[3, N + 1:N]), length.out = 5), 1), CEX = 0.8)


screen(5)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[1, N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
mtext('C12', side = 3, line = 1, adj = 0.5, cex = 1.5, font = 2)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[1, N + 1:N]), max(Y[1, N + 1:N]), length.out = 5), 1), CEX = 0.8)

screen(6)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[2, 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
axis(1, at = seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5), labels = as.character(seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5)), cex.axis = 1)
axis(2, at = seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5), labels = as.character(seq(min(LOCS[, 2]), max(LOCS[, 2]), length.out = 5)), cex.axis = 1)
mtext('Longitude', side = 1, line = 2.5, adj = 0.5, cex = 1)
mtext('Latitude', side = 2, line = 2.5, adj = 0.5, cex = 1)
mtext('Reference Location: (29, 47)', side = 2, line = 4, adj = 0.5, cex = 1, col = 'blue', font = 2)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[2, 1:N]), max(Y[2, 1:N]), length.out = 5), 1), CEX = 0.8)

screen(7)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[4, N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
axis(1, at = seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5), labels = as.character(seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5)), cex.axis = 1)
mtext('Longitude', side = 1, line = 2.5, adj = 0.5, cex = 1)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[4, N + 1:N]), max(Y[4, N + 1:N]), length.out = 5), 1), CEX = 0.8)

screen(8)
par(pty = 's')
par(mai=c(0.5, 0.5, 0.5, 0.5))
quilt.plot(LOCS[, 1], LOCS[, 2], Y[2, N + 1:N], nx = sqrt(N), ny = sqrt(N), ylab = '', xlab = '', axes = F, add.legend = F)
axis(1, at = seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5), labels = as.character(seq(min(LOCS[, 1]), max(LOCS[, 1]), length.out = 5)), cex.axis = 1)
mtext('Longitude', side = 1, line = 2.5, adj = 0.5, cex = 1)

par(xpd=TRUE)

x1 <- c(95,98,98,95)
y1 <- c(-50,-50,50,50)
legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(min(Y[2, N + 1:N]), max(Y[2, N + 1:N]), length.out = 5), 1), CEX = 0.8)

close.screen( all=TRUE)
dev.off()
