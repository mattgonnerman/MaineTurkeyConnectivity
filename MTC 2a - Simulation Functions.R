### Simulate Spring Seasonal Movement Track for 1 bird
sim.disperse <- function(startpoint.df, rasterday, rasterroost){
  dec.output <- data.frame(ID = 1,
                           CellID = NA, 
                           HS = NA,
                           D = NA,
                           x = startpoint.df$Long[1],
                           y = startpoint.df$Lat[1],
                           A = NA, 
                           TurnA = runif(1, -pi, pi),
                           W = NA)
  output.df <- cbind(startpoint.df, dec.output)
  
  for(i in 1:N.steps.max){
    if(i %% 15 == 0){
      step.decision <- sim.decision(output.df[i,], rasterroost, 180*output.df$TurnA[i]/pi)
      output.df[i+1,] <- cbind(output.df[i,1:12], step.decision) %>% mutate(Step = Step +1)
      output.df$D2End[i+1] <- abs(pointDistance(c(output.df$x[i+1],output.df$y[i+1]), 
                                                c(output.df$EndX[i+1], output.df$EndY[i+1]), lonlat = F))
      if(output.df$D2End[i+1] < end.dist){
        break()
      }
    }else{
      step.decision <- sim.decision(output.df[i,], rasterday, 180*output.df$TurnA[i]/pi)
      output.df[i+1,] <- cbind(output.df[i,1:12], step.decision) %>% mutate(Step = Step +1)
      output.df$D2End[i+1] <- abs(pointDistance(c(output.df$x[i+1],output.df$y[i+1]), 
                                                c(output.df$EndX[i+1], output.df$EndY[i+1]), lonlat = F))
      if(output.df$D2End[i+1] < end.dist){
        break()
      }
    }
  }
  
  
  return(output.df)
}

### Function to simulate 1 decision for moving from one location to another on given raster, weighted
sim.decision <- function(location, raster, prev.angle){
  
  options <- as.data.frame(extract(raster, location[,c("x", "y")], buffer = R, cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[,c("x", "y")])
  options$D <- as.data.frame(extract(D.raster, location[1,c("x", "y")], buffer = R, cellnumbers = T, df = T))[,3]
  options <- cbind(options, xyFromCell(raster, cell = options$CellID)) %>%
    mutate(A = geosphere::bearing(sp::spTransform(sp::SpatialPoints(coords = location[,c("x", "y")], proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")),
                                  sp::spTransform(sp::SpatialPoints(coords = matrix(c(x,y), ncol =2), proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")))) %>%
    mutate(A = ifelse(A < 0, 360 + A, A),
           TurnA = 180 - abs(abs(A - prev.angle) - 180)) %>%
    mutate(TurnA = pi* TurnA/180) %>%
    mutate(W = cell.selection.w(HS, D, A, location$p[1], location$k[1], 1/location$theta[1], location$mu[1], location$rho[1]))
  # mutate(W = runif(nrow(options), 0, 1))
  
  if(i %% 15 == 0){ 
    if(identical(options$W, rep(0.1, nrow(options)))){
      options <- nearest.tree(location, raster)
      decision <- sample(1:nrow(options), 1, prob = options$W)
      return(options[decision,])
    }else{
      decision <- sample(1:nrow(options), 1, prob = options$W)
      return(options[decision,])
    }
  }else{
    decision <- sample(1:nrow(options), 1, prob = options$W)
    return(options[decision,])
  }
}


### Function to calculate the weight of a raster cell for sim.decision
cell.selection.w <- function(H, D, TA, p, k, theta, mu, rho){
  HS <- H^exp(p - 1) 
  S <- dgamma(D, k, 1/theta)
  TnA <- dwrpcauchy(TA, mu, rho)
  w <- HS * S * TnA
  
  w <- ifelse(is.infinite(w), 0, ifelse(is.na(w), 0, w))
  if(identical(w, rep(0, length(w)))){
    w <- rep(0.1, length(w))
    return(w)
  }else{
    return(w)
  }
}

### If a bird doesn't have a roosting location near it when its time to roost (within R), 
### then sample all forested areas within an expanded radius and use distance as weights
nearest.tree <- function(location, raster){
  increased.R <- 1000
  options <- as.data.frame(extract(raster, location[,c("x", "y")], buffer = increased.R, cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[,c("x", "y")])
  options$D <- as.data.frame(extract(D.raster, location[,c("x", "y")], buffer = increased.R, cellnumbers = T, df = T))[,3]
  options <- options %>% mutate(W = ifelse(HS == 0, 0, D))
  return(options)
}