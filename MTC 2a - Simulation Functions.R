### Simulate Spring Seasonal Movement Track for 1 bird
sim.disperse <- function(startpoint.df, rasterday, rasterroost){
    dec.output <- data.frame(ID = 1,
                           CellID = NA, 
                           HS = NA,
                           D = NA,
                           x = startpoint.df$Long[1] + round(rnorm(1, 0, 50)),
                           y = startpoint.df$Lat[1] + round(rnorm(1, 0, 50)),
                           B = runif(1, 0, 360), 
                           TurnA = runif(1, -pi, pi),
                           W = NA)
  output.df <- cbind(startpoint.df, dec.output)
  
  for(i in 1:N.steps.max){
    if(i %% 15 == 0){
      step.decision <- sim.decision(output.df[i,], rasterroost, output.df$B[i], i)
      output.df[i+1,] <- cbind(output.df[i,1:13], step.decision) %>% mutate(Step = Step +1)
      output.df$D2End[i+1] <- abs(pointDistance(c(output.df$x[i+1],output.df$y[i+1]), 
                                                c(output.df$EndX[i+1], output.df$EndY[i+1]), lonlat = F))
      if(output.df$D2End[i+1] < end.dist){
        break()
      }
    }else{
      step.decision <- sim.decision(output.df[i,], rasterday, output.df$B[i], i)
      output.df[i+1,] <- cbind(output.df[i,1:13], step.decision) %>% mutate(Step = Step +1)
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
sim.decision <- function(location, raster, prev.bear, i){
  
  options <- as.data.frame(extract(raster, location[,c("x", "y")], buffer = location$R[1], cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[,c("x", "y")])
  options$D <- as.data.frame(extract(D.raster, location[1,c("x", "y")], buffer = location$R[1], cellnumbers = T, df = T))[,3]
  options <- cbind(options, xyFromCell(raster, cell = options$CellID)) %>%
    mutate(B = geosphere::bearing(sp::spTransform(sp::SpatialPoints(coords = location[,c("x", "y")], proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")),
                                  sp::spTransform(sp::SpatialPoints(coords = matrix(c(x,y), ncol =2), proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")))) %>%
    mutate(B = ifelse(B < 0, 360 + B, B),
           TurnA = abs(B - prev.bear)) %>%
    mutate(TurnA = ifelse(abs(TurnA) > 180, 360 - TurnA, TurnA),
           TurnA = TurnA * pi/180) %>%
    mutate(W = cell.selection.w(HS, D, TurnA, location$p[1], location$k[1], location$rate[1], location$mu[1], location$rho[1])) %>%
    mutate(W = ifelse(is.na(W), 0, W),
           W = ifelse(W < 0, 0,W),
           W = ifelse(D < 1, 0, W))
  
  if(i %% 15 == 0){ 
    if(length(unique(options$W)) == 1){
      options <- nearest.tree(location, raster)
      options <- cbind(options, xyFromCell(raster, cell = options$CellID)) %>%
        mutate(B = geosphere::bearing(sp::spTransform(sp::SpatialPoints(coords = location[,c("x", "y")], proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")),
                                      sp::spTransform(sp::SpatialPoints(coords = matrix(c(x,y), ncol =2), proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")))) %>%
        mutate(B = ifelse(B < 0, 360 + B, B),
               TurnA = abs(B - prev.bear)) %>%
        mutate(TurnA = ifelse(abs(TurnA) > 180, 360 - TurnA, TurnA),
               TurnA = TurnA * pi/180) %>%
        relocate(W, .after = last_col())
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
cell.selection.w <- function(H, D, TA, p, k, rate, mu, rho){
  HS <- H^(exp(p) - 1) 
  S <- dgamma(D, k, scale = 1/rate)
  TnA <- dwrpcauchy(TA, mu, rho)
  w <- HS * S * TnA
  return(w)
}


### If a bird doesn't have a roosting location near it when its time to roost (within R), 
### then sample all forested areas within an expanded radius and use distance as weights
nearest.tree <- function(location, raster){
  increased.R <- 2000
  options <- as.data.frame(extract(raster, location[,c("x", "y")], buffer = increased.R, cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[,c("x", "y")])
  options$D <- as.data.frame(extract(D.raster, location[,c("x", "y")], buffer = increased.R, cellnumbers = T, df = T))[,3]
  options <- options %>% 
    mutate(D = ifelse(is.na(D), 0, D),
           HS = ifelse(is.na(HS), 0, HS)) %>%
    mutate(W = ifelse(HS == 0, 0, 1/D)) %>%
    mutate(W = ifelse(is.na(W), 0, W),
           W = ifelse(W < 0, 0, W),
           W = ifelse(D < 1, 0, W))
  return(options)
}
