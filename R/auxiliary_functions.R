simulate_locations <- function(N, grid = T){

	grid_x <- seq(from = 0, to = 1, length.out = N)
	grid_y <- seq(from = 0, to = 1, length.out = N)
	sim_grid_locations <- expand.grid(grid_x, grid_y) %>% as.matrix()

	return(sim_grid_locations)
}

write_to_txt <- function(data, file_name){

	#comma-separated if you save the bivariate realizations data.
	#space-separated if you save the cross-covariance matrix.	
	if(!is.matrix(data)){
		write.table(data, file = file_name, sep = ",", row.names = FALSE, col.names = FALSE)
	}else{	
		if(ncol(data) > 2 ){
			write.table(data, file = file_name, sep = " ", row.names = FALSE, col.names = FALSE)
		}else{	
			write.table(data, file = file_name, sep = ",", row.names = FALSE, col.names = FALSE)
		}
	}
}

colors=c("blue","yellow","red")
colsteps=100

legend.gradient2 = function(pnts, cols=tim.colors(64),limits=c(0,1), title='Legend', CEX = 1, ROUND = 2, ...){
  	pnts = try(as.matrix(pnts),silent=T)
  	if(!is.matrix(pnts)) stop("you must have a 4x2 matrix")
  	if(dim(pnts)[1]!=4 || dim (pnts)[2]!=2) stop ("Matrix must have dimensions of 4 rows and 2 columms")
  	if(length(cols)<2) stop("You must have 2 or more colors")
  	#break up the min and max into a number of values == length(cols)
  	yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length=length(cols)+1)
  	#cycle through each of the yvals and create polygons
  	for (i in 1:length(cols)){  #create the polygon for that color
    		polygon(x=pnts[,1],y=c(yvals[i],yvals[i],yvals[i+1],yvals[i+1]),col=cols[i],border=F)
  	}
  	#add the text
	if(length(limits) == 5){
  		locationn <- seq(min(pnts[,2]),max(pnts[,2]),length.out = 5)
  		text(max(pnts[,1]),locationn[1],labels=round(limits[1],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[2],labels=round(limits[2],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[3],labels=round(limits[3],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[4],labels=round(limits[4],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[5],labels=round(limits[5],ROUND),pos=4, cex = CEX)
	}else if(length(limits) == 3){
  		locationn <- seq(min(pnts[,2]),max(pnts[,2]),length.out = 3)
  		text(max(pnts[,1]),locationn[1],labels=round(limits[1],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[2],labels=round(limits[2],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[3],labels=round(limits[3],ROUND),pos=4, cex = CEX)
	}
}


color.gradient <- function(x, xlim = NULL, colors = tim.colors(64), colsteps = 1000) {
  if(is.null(xlim)){
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x), max(x), length.out = colsteps)) ] )
  }else{
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(xlim[1], xlim[2], length.out = colsteps)) ] )
  }
}

gradient_legend_3d <- function(values, title = '', plot_title = '', pnts){
  min_val <- min(values)
  max_val <- max(values)
  x <- colorRamp(tim.colors(64))((20:0)/20)
  colors <- rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  legend_image <- as.raster(matrix(colors, ncol=1))
  plot(c(0,1),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '') #Generates a blank plot
  text(x = 0.5, y = 1, plot_title, cex = 2, font = 2)
  text(x = pnts[3] + 0.07, y = seq(pnts[2], pnts[4], l=5), labels = round(seq(min_val, max_val,l=5), 1), cex = 1) #Creates the numeric labels on the scale
  text(x = pnts[3] - 0.1, y = pnts[4], labels = title, adj = 1, srt = 90, cex = 1) #Determines where the title is placed
  rasterImage(legend_image, pnts[1], pnts[2], pnts[3], pnts[4]) #Values can be modified here to alter where and how wide/tall the gradient is drawn in the plotting area
}

title_3d <- function(title = '', pnts){
  plot(c(0,1),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '') 
  text(x = pnts[1], y = pnts[2], title, cex = 1.5, font = 2)
}

legend.gradient3 = function(pnts,cols=tim.colors(64),limits=c(0,1), title='Legend', CEX = 1, ...){
  pnts = try(as.matrix(pnts),silent=T)
  if(!is.matrix(pnts)) stop("you must have a 4x2 matrix")
  if(dim(pnts)[1]!=4 || dim (pnts)[2]!=2) stop ("Matrix must have dimensions of 4 rows and 2 columms")
  if(length(cols)<2) stop("You must have 2 or more colors")
  #break up the min and max into a number of values == length(cols)
  yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length=length(cols)+1)
  #cycle through each of the yvals and create polygons
  for (i in 1:length(cols)){  #create the polygon for that color
    polygon(x=pnts[,1],y=c(yvals[i],yvals[i],yvals[i+1],yvals[i+1]),col=cols[i],border=F)
  }
  #add the text
  locationn <- seq(min(pnts[,2]),max(pnts[,2]),length.out = 3)
  text(max(pnts[,1]) - 0.1,locationn[1],labels=round(limits[1],0),pos=4, cex = CEX)
  text(max(pnts[,1]) - 0.1,locationn[2],labels=0.5 * (round(limits[1],0) + round(limits[5],0)),pos=4, cex = CEX)
  text(max(pnts[,1]) - 0.1,locationn[3],labels=round(limits[5],0),pos=4, cex = CEX)

}

toeplitz_mat <- function(S_list){
	k <- min(unlist(lapply(S_list, dim)))
	n <- length(S_list)
	#
	# Create the "strip".
	#
	strip <- array(NA, dim=c(k,k,2*n-1))
	for (i in 1:n) strip[,,i] <- S_list[[n+1-i]]
	if (n > 1) for (i in 2:n) strip[,,n+i-1] <- t(S_list[[i]])
	#
	# Assemble into "block-Toeplitz" form.
	#
	X <- array(NA, dim=c(k,k,n,n))
	# Blast the strip across X.
	#
	for (i in 1:n) X[,,,i] <- strip[,,(n+1-i):(2*n-i)]
	X <- matrix(aperm(X, c(1,3,2,4)), n*k)
}


generate_h_matrix <- function(LOCS){
	
	n <- nrow(LOCS)	

	H_MAT <- matrix(, ncol = ncol(LOCS), nrow = n^2)

        for(rr in 1:n){
                for(ss in 1:n){
			for(cc in 1:ncol(LOCS)){
                        		H_MAT[ (rr - 1) * n + ss, cc] <- LOCS[rr, cc] - LOCS[ss, cc]
			}
                }
        }
	
	return(H_MAT)

}

mahalanobis_dist <- function(x, SIGS) {
	SIGS_INV <- solve(SIGS)
	u <- apply(x, 1, function(y) y %*% SIGS_INV %*% y)
        d <- outer(u, u, `+`) - 2 * x %*% SIGS_INV %*% t(x)
        return(d)
}

rdist.earth.vec2=function(x1, x2, miles = TRUE, R = NULL) {
  if (is.null(R)) {
    if (miles)
      R = 3963.34
    else R = 6378.388
  }
  x1 = x1 * (pi/180)
  x2 = x2 * (pi/180)
  lonDist2 = (x2[, 1] - x1[, 1]) * (1/2)
  latDist2 = (x2[, 2] - x1[, 2]) * (1/2)
  a = sin(latDist2) * sin(latDist2) + cos(x1[, 2]) * cos(x2[, 2]) * sin(lonDist2) * sin(lonDist2)
  error.check=tryCatch(sqrt(1 - a), error = function(x) {sqrt(1 - round(a, 15))})
  dist=2 * atan2(sqrt(a), error.check) * R
  return(2 * atan2(sqrt(a), error.check) * R)
}

find_UTM_zone <- function(longitude, latitude) {
  
  # Special zones for Svalbard and Norway
  if (latitude >= 72.0 && latitude < 84.0 ) 
    if (longitude >= 0.0  && longitude <  9.0) 
      return(31);
  if (longitude >= 9.0  && longitude < 21.0)
    return(33)
  if (longitude >= 21.0 && longitude < 33.0)
    return(35)
  if (longitude >= 33.0 && longitude < 42.0) 
    return(37)
  
  (floor((longitude + 180) / 6) %% 60) + 1
}


find_UTM_hemisphere <- function(latitude) {
  
  ifelse(latitude > 0, "north", "south")
}

# returns a DF containing the UTM values, the zone and the hemisphere
longlat_to_UTM <- function(long, lat, units = 'm') {
  
  df <- data.frame(
    id = seq_along(long), 
    x = long, 
    y = lat
  )
  sp::coordinates(df) <- c("x", "y")
  
  hemisphere <- find_UTM_hemisphere(lat)
  zone <- find_UTM_zone(long, lat)
  
  sp::proj4string(df) <- sp::CRS("+init=epsg:4326") 
  CRSstring <- paste0(
    "+proj=utm +zone=", zone,
    " +ellps=WGS84",
    " +", hemisphere,
    " +units=", units)
  if (dplyr::n_distinct(CRSstring) > 1L) 
    stop("multiple zone/hemisphere detected")
  
  res <- sp::spTransform(df, sp::CRS(CRSstring[1L])) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(
      zone = zone,
      hemisphere = hemisphere
    )
  
  res
}

UTM_to_longlat <- function(utm_df, zone, hemisphere) {
  
  CRSstring <- paste0("+proj=utm +zone=", zone, " +", hemisphere)
  utmcoor <- sp::SpatialPoints(utm_df, proj4string = sp::CRS(CRSstring))
  longlatcoor <- sp::spTransform(utmcoor, sp::CRS("+init=epsg:4326"))
  tibble::as_data_frame(longlatcoor)
}


LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

decimalplaces <- function(x) {
  temp <- strsplit(sub('0+$', '', as.character(x)), "e-", fixed = TRUE)[[1]][[2]]
  if(temp == '1'){
    as.numeric(paste(1, 'e-', temp, 0, sep = ''))
  }else{
    as.numeric(paste(1, 'e-', temp, sep = ''))
  }
  
  #nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
}

deg2rad <- function(deg){
  return(deg * pi / 180)
}

rad2deg <- function(rad){
  return(rad * 180 / pi)
}

distanceEarth <- function(lat1d, lon1d, lat2d, lon2d, earthRadiusKm = 1) {
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  u = sin((lat2r - lat1r)/2);
  v = sin((lon2r - lon1r)/2);
  #return(2.0 * earthRadiusKm * asin(sqrt(u * u + cos(lat1r) * cos(lat2r) * v * v)))
  return(2.0 * earthRadiusKm * sqrt(u^2 + cos(lat1r) * cos(lat2r) * v^2))
}

calculateDistance <- function(x1, y1, x2, y2, distance_metric) {
  if(distance_metric == 0){
    return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
  }else if(distance_metric == 1){
    return (distanceEarth(x1, y1, x2, y2))
  }
}

# Auxiliary functions for the bivariate_differential_operator_spatial

h <- function(scale_horizontal_space, scale_vertical_space,
              lat1d, lon1d, pres1, lat2d, lon2d, pres2){
  return(scale_horizontal_space^2 * calculateDistance(lat1d, lon1d, lat2d, lon2d, 1)^2 + 
           scale_vertical_space^2 * (pres1 - pres2)^2)
}

h1 <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  L = lat1r - lat2r
  l = lon1r - lon2r
  
  con = 4 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return (con * (sin(L / 2) * cos(L / 2) - sin(lat1r) * cos(lat2r) * (sin(l / 2))^2))
}

h3 <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  l = lon1r - lon2r
  
  con = 4 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return(con * sin(l / 2) * cos(l / 2))
}

h3_old <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  l = lon1r - lon2r
  
  con = 4 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return(con * cos(lat1r) * cos(lat2r) * sin(l / 2) * cos(l / 2))
}

h33 <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  l = lon1r - lon2r
  
  con = 2 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return(con * ((cos(l / 2))^2 - (sin(l / 2))^2))
}

h33_old <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  l = lon1r - lon2r
  
  con = 2 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return(con * cos(lat1r) * cos(lat2r) * ((cos(l / 2))^2 - (sin(l / 2))^2))
}

h12 <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  L = lat1r - lat2r
  l = lon1r - lon2r
  
  con = 4 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return (con * (-(cos(L / 2))^2 / 2 + (sin(L / 2))^2 / 2 + sin(lat1r) * sin(lat2r) * (sin(l / 2))^2))
}

h13 <- function(scale_horizontal_space, lat1d, lon1d, lat2d, lon2d){
  
  lat1r = deg2rad(lat1d)
  lon1r = deg2rad(lon1d)
  lat2r = deg2rad(lat2d)
  lon2r = deg2rad(lon2d)
  l = lon1r - lon2r
  
  con = 4 * scale_horizontal_space^2 * earthRadiusKm^2
  
  return (-con * sin(lat1r) * cos(lat2r) * sin(l / 2) * cos(l / 2))
}

h4 <- function(scale_vertical_space, pres1, pres2){
  return(2 * scale_vertical_space^2 * (pres1 - pres2))
}

h44 <- function(scale_vertical_space){
  return(2 * scale_vertical_space^2)
}


C1 <- function(PARAM, l1, l2){
  
  a1 = PARAM[3]
  b1 = PARAM[4]
  c1 = PARAM[5]
  d1 = PARAM[6]
  a2 = PARAM[7]
  b2 = PARAM[8]
  c2 = PARAM[9]
  d2 = PARAM[10]
  
  H = h(PARAM[1], PARAM[2], l1[2], l1[1], l1[3], l2[2], l2[1], l2[3]);
  H1 = h1(PARAM[1], l1[2], l1[1], l2[2], l2[1]);
  H2 = h1(PARAM[1], l2[2], l1[1], l1[2], l2[1]);
  H3 = h3(PARAM[1], l1[2], l1[1], l2[2], l2[1]);
  H4 = h4(PARAM[2], l1[3], l2[3]);
  
  return(0.25 * (a1 * a2 * H1 * H2 - b1 * b2 * H3^2 - c1 * c2 * H4^2 - a1 * b2 * H1 * H3
                 + a2 * b1 * H2 * H3 - a1 * c2 * H1 * H4 + a2 * c1 * H2 * H4
                 - b1 * c2 * H3 * H4 - b2 * c1 * H3 * H4) + H * d1 * d2)
}

C2 <- function(PARAM, l1, l2){
  
  a1 = PARAM[3]
  b1 = PARAM[4]
  c1 = PARAM[5]
  d1 = PARAM[6]
  a2 = PARAM[7]
  b2 = PARAM[8]
  c2 = PARAM[9]
  d2 = PARAM[10]
  
  nu = PARAM[11]
  
  H12 = h12(PARAM[1], l1[2], l1[1], l2[2], l2[1])
  H13 = h13(PARAM[1], l1[2], l1[1], l2[2], l2[1])
  H23 = h13(PARAM[1], l2[2], l1[1], l1[2], l2[1])
  H33 = h33(PARAM[1], l1[2], l1[1], l2[2], l2[1])
  H44 = h44(PARAM[2])
  
  return(-0.5 * (a1 * a2 * H12 - b1 * b2 * H33 - c1 * c2 * H44 - a1 * b2 * H13 + a2 * b1 * H23) + 2 * nu * d1 * d2)
}

uni_differential <- function(l1, l2, PARAM){
  
  sigma_square = PARAM[1]
  scale_horizontal_space = PARAM[2]
  scale_vertical_space = PARAM[3]
  smoothness = PARAM[4]
  
  a1 <- PARAM[5]
  b1 <- PARAM[6]
  c1 <- PARAM[7]
  d1 <- PARAM[8]
  
  a2 <- PARAM[9]
  b2 <- PARAM[10]
  c2 <- PARAM[11]
  d2 <- PARAM[12]
  
  PARAM_SUB <- c(scale_horizontal_space, scale_vertical_space, a1, b1, c1, d1, a2, b2, c2, d2, smoothness)
  
  con = 2^(smoothness - 1) * gamma(smoothness)
  con = 1.0 / con
  con = sigma_square * con
  
  expr <- sqrt(h(PARAM_SUB[1], PARAM_SUB[2], l1[2], l1[1], l1[3], l2[2], l2[1], l2[3]))
  
  if(round(expr, 3) == 0){
    val <- sigma_square * (C1(PARAM_SUB, l1, l2) + C2(PARAM_SUB, l1, l2))
    #val <- sigma_square * (C1(PARAM_SUB, l2, l2) + C2(PARAM_SUB, l2, l2))
  }else{
    f <- expr^smoothness * besselK(expr, smoothness)
    f_prime <- expr^(smoothness - 1) * besselK(expr, smoothness - 1)
    val <- con * (C1(PARAM_SUB, l1, l2) * f_prime + C2(PARAM_SUB, l1, l2) * f)
  }
  
  return(val)
}

plot_along_pressure <- function(data, data_type = 'residuals', ref_loc){
  
  long <- ref_loc[1]
  lat <- ref_loc[2]
  
  if(data_type == 'residuals'){
    
    for(type in c('temp', 'psal')){
      label <- c('Temperature', 'Salinity')[c('temp', 'psal') == type]
      label_units <- c('Temp (°C)', 'PSU')[c('temp', 'psal') == type]
      data$response <- list(data$temperature, data$salinity)[[which(c('temp', 'psal') == type)]]
      theme_set(theme_bw())
      
      g <- ggplot() + geom_line(data= data, mapping = aes(x = pressure, y = response, group = profile), alpha = .1)+ 
        geom_point(data=data, aes(x = pressure, y = response), size = .2, alpha = .1) +
        coord_flip(xlim = c(1950, 0)) +
        scale_x_continuous(trans = 'reverse')+
        scale_color_viridis_d()+
        labs(x = 'Pressure (decibars)', y = label_units, color = 'Function')+
        geom_vline(aes(xintercept = seq(100, 2000, by = 100)), color="green", linetype="dashed") +
        theme(legend.position = 'bottom')
      
      ggsave(filename = paste('../figures/', type,'_residuals_long_', long, '_lat_', lat,'.png', sep = ''), scale = 1.2, height = 5, width = 3.75, dpi = 150)
      #ggsave(filename = paste('../figures/', type,'_residuals_full_large_long_', long, '_lat_', lat,'.png', sep = ''), scale = 1.2, height = 5, width = 3.75, dpi = 600)
      
      cat(paste("Plots are saved in ", paste('../figures/', type,'_residuals_long_', long, '_lat_', lat,'.png', sep = ''), sep = ''), '\n')
    }
    
  }else if(data_type == 'empirical'){
    
    for(type in c('temp', 'psal', 'cross')){
      label_units <- c('Temperature Empirical Variance', 'Salinity Empirical Variance', 'Empirical Cross-Correlation')[c('temp', 'psal', 'cross') == type]
      data$response <- list(data$emp_var_temp, data$emp_var_psal, data$emp_cor)[[which(c('temp', 'psal', 'cross') == type)]]
      
      ggplot() + geom_line(data=data, mapping = aes(x = pressure, y = response), alpha = 1)+ 
        geom_point(data=data, aes(x = pressure, y = response), size = .2, alpha = 1) +
        coord_flip(xlim = c(2000, 0)) +
        scale_x_continuous(trans = 'reverse')+
        scale_color_viridis_d()+
        labs(x = 'Pressure (decibars)', y = label_units, color = 'Function')+
        geom_vline(aes(xintercept = seq(100, 2000, by = 100)), color="green", linetype="dashed") +
        theme(legend.position = 'bottom')
      
      if(type == 'cross'){
        ggsave(filename = paste('../figures/', type,'_empirical_correlation_binned_long_', long, '_lat_', lat,'.png', sep = ''), scale = 1.2, height = 5, width = 3.75, dpi = 150)
        cat(paste("Plots are saved in ", paste('../figures/', type,'_empirical_correlation_binned_long_', long, '_lat_', lat,'.png', sep = ''), sep = ''), '\n')
      }else{
        ggsave(filename = paste('../figures/', type,'_empirical_variance_binned_long_', long, '_lat_', lat,'.png', sep = ''), scale = 1.2, height = 5, width = 3.75, dpi = 150)
        #ggsave(filename = paste('../figures/', type,'_empirical_variance_binned_full_layers_large_long_', long, '_lat_', lat,'.png', sep = ''), scale = 1.2, height = 5, width = 3.75, dpi = 600)
        cat(paste("Plots are saved in ", paste('../figures/', type,'_empirical_variance_binned_long_', long, '_lat_', lat,'.png', sep = ''), sep = ''), '\n')
      }
      
    }
  }
  
}

retrieve_residuals <- function(REFERENCE_LOCATION, PLOT_RESIDUALS = T, PLOT_EMPIRICAL_VALUES = T, RADIUS = 1000, YEAR = 2010){
  
  long <- REFERENCE_LOCATION[1]
  lat <- REFERENCE_LOCATION[2]
  
  df <- get_profile_data_subset(long = long, lat = lat, day = 45.25, h_time = 45.25, RG_def = FALSE,
                                h_space = RADIUS, mode = 'all',  min_prof = 5, exclusion = TRUE)
  
  residuals <- df[df$year == YEAR, ]
  N <- nrow(residuals)
  
  if(PLOT_RESIDUALS){
    
    cat("Plotting residuals as a function of pressure all dbars. . . \n")
    
    plot_along_pressure(data = residuals, data_type = 'residuals', ref_loc = REFERENCE_LOCATION)
    
  }
  
  height = 100
  DAT <- NULL
  for(DEPTH in 1:(2000 / height)){
    ind <- which(df$year == 2010 & df$pressure < DEPTH * height & df$pressure >= (DEPTH - 1) * height )
    if(length(ind) > 0){
      df_sub <- df[ind, ]
      DAT <- rbind(DAT, cbind(long, lat, (DEPTH - 1) * height, var(df_sub$temperature), var(df_sub$salinity), cor(df_sub$temperature, df_sub$salinity)))
    }
  }
  
  empirical_values <- data.frame(pressure = DAT[, 3], emp_var_temp = DAT[, 4], emp_var_psal = DAT[, 5], emp_cor = DAT[, 6])
  
  if(PLOT_EMPIRICAL_VALUES){
    
    cat("Plotting empirical variance and cross-correlation as a function of pressure all dbars. . . \n")
    
    plot_along_pressure(data = empirical_values, data_type = 'empirical', ref_loc = REFERENCE_LOCATION)
    
  }
  
  setClass("Data", representation(empirical_values = "matrix", long = "numeric", lat = "numeric", num_locs = "numeric"))
  
  DATA <- new("Data", empirical_values = DAT, long = long, lat = lat, num_locs = N)
  
  return(DATA)
  
}
