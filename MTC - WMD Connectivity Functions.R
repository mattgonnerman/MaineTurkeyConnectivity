
N.steps.max <- 15*30 #15 steps * number of days

########################################################################################################################
### Joint Function to simulate turkey movements between WMDs
simwmdconnect <- function(x){
  
  startmove <- simstartmove(x)
  
  point <- st_point(c(x$Long[1], x$Lat[1]))
  lasthex <- hexcovs[point,]$GridID[1]
  
  if(startmove == 0){
    #Turkey DOES NOT leave winter range
    finalloc <- randwithinhex(x)
    
    start.dec.output <- data.frame(ID = 1,
                             CellID = NA, 
                             HS = NA,
                             D = NA,
                             x = x$Long[1],
                             y = x$Lat[1],
                             B = runif(1, 0, 360), 
                             TurnA = runif(1, -pi, pi),
                             W = NA)
    start.output.df <- cbind(x, start.dec.output)
    end.dec.output <- data.frame(ID = 2,
                             CellID = NA, 
                             HS = NA,
                             D = NA,
                             x = finalloc[1],
                             y = finalloc[2],
                             B = runif(1, 0, 360), 
                             TurnA = runif(1, -pi, pi),
                             W = NA)
    end.output.df <- cbind(x, end.dec.output)
    turkeytrack <- rbind(start.output.df, end.output.df)
  }else{
    #Turkey DOES leave winter range
 
    
    turkeytrack <- sim.disperse.wmd(x, HS_day, HS_roost, lasthex)
    
  }
  
  write.table(turkeytrack, "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimTracks.csv", row.names = F,
            append = T, sep = ",", col.names = !file.exists("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimTracks.csv"))
  # return(turkeytrack)
  
}


########################################################################################################################
#Bernouli trial based on town covariates to determine whether turkey initiates seasonal movements
simstartmove <-function(x){
  #Get covariates from town that intersects start location
  point <- st_point(c(x$Long[1], x$Lat[1]))
  hexcov <- hexcovs[point,] 
  
  #Return 1 or 0
  startmove <- rbinom(1,1,
                      ifelse(x$Sex[1] == "M", hexcov$DispProbM[1], hexcov$DispProbF[1]))
  return(startmove)
}


########################################################################################################################
### Simulate Spring Seasonal Movement Track for 1 bird
sim.disperse.wmd <- function(startpoint.df, rasterday, rasterroost, lasthex){
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
      output.df[i+1,] <- cbind(output.df[i,1:12], step.decision) %>% mutate(Step = Step +1)
    }else{
      step.decision <- sim.decision(output.df[i,], rasterday, output.df$B[i], i)
      output.df[i+1,] <- cbind(output.df[i,1:12], step.decision) %>% mutate(Step = Step +1)
    }
    
    point <- st_point(c(step.decision$x[1], step.decision$y[1]))
    newhex <- hexcovs[point,]$GridID[1]
    
    if(lasthex != newhex){
      stop <- settle.decision(output.df[i+1,])
      
      if(stop == 1){
        finalloc <- randwithinhex(output.df[i+1,])
        finalloc.df <- output.df[i+1,] %>%
          mutate(Step = Step + 1,
                 x = finalloc[1],
                 y = finalloc[2],
                 CellID = NA,
                 HS = NA,
                 D = NA, 
                 B = NA,
                 TurnA = NA,
                 W = NA)
        output.df[i+2,] <- finalloc.df
        break
      }else{
        lasthex <- newhex 
      }
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
### If a bird doesn't have a roosting location near it when its time to roost (within R), 
### then sample all forested areas within an expanded radius and use distance as weights
nearest.tree <- function(location, raster){
  increased.R <- 4000
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
  point <- st_point(c(x$x[1], x$y[1]))
  lasthex <- hexcovs[point,]$GridID[1] 
  
  hexcov <- hexcovs[point,]
  
  #Return 1 or 0
  stopmove <- rbinom(1,1, 
                     ifelse(x$Sex[1] == "M", hexcov$SettleProbM[1], hexcov$SettleProbF[1]))
  return(stopmove)
}

  
########################################################################################################################
#Select random location within town boundaries for final destination of turkey
randwithinhex <- function(x){
  point1 <- st_point(c(x$Long[1], x$Lat[1]))
  hexbound <- hexcovs[point1,]
  
  points.df <- as.data.frame(sampleRandom(forbin.rast, 100, ext = extent(hexbound),
                                          na.rm = T, xy = T))
  points.sf <- st_as_sf(points.df, coords = c("x", "y"), crs = crs(wmdbound)) %>%
    st_join(., hexbound, join = st_intersects) %>%
    filter(GridID == hexbound$GridID[1]) %>%
    filter(FullForestBin == 1) %>%
    slice(1)
  
  
  finalpoint <- st_coordinates(points.sf[1,])
  return(finalpoint)  
}

