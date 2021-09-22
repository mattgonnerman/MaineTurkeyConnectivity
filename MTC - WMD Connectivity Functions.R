### Joint Function to simulate turkey movements between WMDs
simwmdconnect <- function(x){
  startmove <- simstartmove(x)

  if(startmove == 0){
    #Turkey does no leave winter range
    randwithintown(x)
    
  }else{
    #Begin Simulation
    sim.disperse.wmd(x, HS_day, HS_roost)
    
  }
}


########################################################################################################################
#Bernouli trial based on town covariates to determine whether turkey initiates seasonal movements
simstartmove <-function(x){
  #Get covariates from town that intersects start location
  point <- st_point(c(x$Long[1], x$Lat[1]))
  towncov <- towncovs[point,] 
  
  #Return 1 or 0
  startmove <- rbinom(1,1, towncov$startmove.M[1])
  return(startmove)
}


########################################################################################################################
### Simulate Spring Seasonal Movement Track for 1 bird
sim.disperse.wmd <- function(startpoint.df, rasterday, rasterroost){
  dec.output <- data.frame(ID = 1,
                           CellID = NA, 
                           HS = NA,
                           D = NA,
                           x = startpoint.df$Long[1],
                           y = startpoint.df$Lat[1],
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
    }else{
      step.decision <- sim.decision(output.df[i,], rasterday, output.df$B[i], i)
      output.df[i+1,] <- cbind(output.df[i,1:13], step.decision) %>% mutate(Step = Step +1)
      output.df$D2End[i+1] <- abs(pointDistance(c(output.df$x[i+1],output.df$y[i+1]), 
                                                c(output.df$EndX[i+1], output.df$EndY[i+1]), lonlat = F))
    }
    
    stop <- settle.decision(step.decision)
    
    if(stop$Settle == 1){
      break
    }
  }
  return(output.df)
}


########################################################################################################################
### Simulate 1 decision for moving from one location to another on given raster, weighted
sim.decision <- function(location, raster, prev.bear, i){
  
  options <- as.data.frame(raster::extract(raster, location[,c("x", "y")], buffer = location$R[1], cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[,c("x", "y")])
  options$D <- as.data.frame(raster::extract(D.raster, location[1,c("x", "y")], buffer = location$R[1], cellnumbers = T, df = T))[,3]
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
        mutate(TurnA = ifelse(TurnA > 180, 360 - TurnA, TurnA),
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


########################################################################################################################
### Function to calculate the weight of a raster cell for sim.decision
cell.selection.w <- function(H, D, TA, p, k, rate, mu, rho){
  HS <- H^(exp(p) - 1) 
  S <- dgamma(D, k, scale = 1/rate)
  TnA <- dwrpcauchy(TA, mu, rho)
  w <- HS * S * TnA
  return(w)
}


########################################################################################################################
# Determine if Turkey ceases movement
settle.decision <- function(x){
  #Get covariates from town that intersects start location
  point <- st_point(c(x$Long[1], x$Lat[1]))
  towncov <- towncovs[point,] %>% st_drop_geometry() %>%
    dplyr::select(Developed = Develpd, Agriculture = Agrcltr, Wetland) %>%
    mutate(Settled = 1,
           BirdID = settle.input$BirdID[100],
           Wetland = (Wetland - mean(disp.input.raw$Wetland))/sd(disp.input.raw$Wetland),
           Developed = (Developed - mean(disp.input.raw$Developed))/sd(disp.input.raw$Developed),
           Agriculture = (Agriculture - mean(disp.input.raw$Agriculture))/sd(disp.input.raw$Agriculture))
  
  #Create Bernoulli Trial
  explr <- predict(settmodel.Final, newdata =  towncov, type = "expected")
  
  #Return 1 or 0
  startmove <- rbinom(1,1, explr)
  return(startmove)
  
  
}

  
########################################################################################################################
#Select random location within town boundaries for final destination of turkey
randwithintown <- function(x){
  point1 <- st_point(c(x$x[1], x$y[1]))
  townbound <- towncovs[point1,]
  
  points.df <- as.data.frame(sampleRandom(forbin.rast, 10, ext = extent(townbound),
                                          na.rm = T, xy = T))
  points.sf <- st_as_sf(points.df, coords = c("x", "y"), crs = crs(wmdbound)) %>%
    st_join(., townbound, join = st_intersects) %>%
    filter(Town == townbound$Town[1]) %>%
    slice(1)
  
  
  finalpoint <- st_coordinates(points.sf[1,])
  return(finalpoint)  
}

