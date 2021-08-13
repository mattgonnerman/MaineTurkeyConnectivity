####################################
### SIMULATE DISPERSAL MOVEMENTS ###
####################################
# Simulate turkey dispersals from capture locations

### Load Packages
lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)


### User-Defined variables used for simulations or setup
N.simturk <- 10 #The number of simulations PER STARTING POINTS
R <- 250 #Perception Distance, how far away will turkey still be able to consider a patch
N.steps.max <- 500 #15 steps * number of days

### Load/Setup Start and End Locations
startlocs <- st_read("./GIS/Disperser Start.shp")
startlocs <- st_transform(startlocs, crs = 32619)
startlocs$StartX <- st_coordinates(startlocs)[,1]
startlocs$StartY <- st_coordinates(startlocs)[,2]
startlocs <- st_drop_geometry(startlocs)
endlocs <- st_read("./GIS/Disperser End.shp")
endlocs <- st_transform(endlocs, crs = 32619)
endlocs$EndX <- st_coordinates(endlocs)[,1]
endlocs$EndY <- st_coordinates(endlocs)[,2]
endlocs <- st_drop_geometry(endlocs)

obs.paths <- merge(startlocs, endlocs, by = c("BirdID", "ID")) %>%
  arrange(ID) %>%
  rename(OG.ID = ID)


### Load/Setup World
slocs <- st_read("./GIS/Disperser Start.shp")
elocs <- st_read("./GIS/Disperser End.shp")
bothlocs <- st_transform(rbind(slocs, elocs), 32619)
sim.world <- raster(ext = extend(extent(bothlocs), 5000), res = 30)
sim.world <- setValues(sim.world, runif(length(sim.world), 0, 1))

### Create Animals and Setup Starting Locations
sim.turkey.list <- list()
for(i in 1:nrow(obs.paths)){
  sim.turkey.list[[i]] <- data.frame(Sim.ID = 1:N.simturk,
                                OG.ID = rep(obs.paths$OG.ID[i], N.simturk),
                                StartX = rep(obs.paths$StartX[i], N.simturk),
                                StartY = rep(obs.paths$StartY[i], N.simturk),
                                EndX = rep(obs.paths$EndX[i], N.simturk),
                                EndY = rep(obs.paths$EndY[i], N.simturk))
}

#DataFrame with 
sim.turkey <- do.call(rbind.data.frame, sim.turkey.list) %>%
  mutate(p = runif(N.simturk*length(sim.turkey.list), .1, 5),
         rho = runif(N.simturk*length(sim.turkey.list), 0.26868487 - (2*0.01281980), 0.26868487 + (2*0.01281980)),
         mu = runif(N.simturk*length(sim.turkey.list), 0.08464538 - (2*0.04287019), 0.08464538 + (2*0.04287019)),
         k = runif(N.simturk*length(sim.turkey.list), 0.84834637 - (2*1.744392e-02), 0.84834637 + (2*1.744392e-02)),
         theta = runif(N.simturk*length(sim.turkey.list), 0.00311007 - (2*7.517423e-05), 0.00311007 + (2*7.517423e-05))) %>% 
  rename(Long = StartX, Lat = StartY) %>% 
  mutate(Step = 1)


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
      
    }else{
      step.decision <- sim.decision(output.df[i,], rasterday, 180*output.df$TurnA[i]/pi)
      output.df[i+1,] <- cbind(output.df[i,1:12], step.decision) %>% mutate(Step = Step +1)
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
  decision <- sample(1:nrow(options), 1, prob = options$W)
  
  return(options[decision,])
  
}


### Function to calculate the weight of a raster cell for sim.decision
cell.selection.w <- function(H, D, TA, p, k, theta, mu, rho){
  HS <- H^exp(p - 1) 
  S <- dgamma(D, k, 1/theta)
  TnA <- dwrpcauchy(TA, mu, rho)
  w <- HS * S * TnA
  
  w <- ifelse(is.infinite(w), 0, w)
  
  return(w)
}



#### TEST CODE ####
startpoint.df <- sim.turkey[1,]
rasterday <- sim.world
rasterroost<- sim.world



move.sim <- list()
# for(i in 1:nrow(sim.turkey)){
for(i in 1){
  move.sim[[i]] <- sim.disperse(sim.turkey[i,], sim.world, sim.world)
}
test.line <-  st_linestring(as.matrix(move.sim[[1]][,c("x", "y")]))
plot(test.line)


move.sim[[1]][1,c("Long", "Lat")]
location <- sim.turkey[1,]
raster <- sim.world
prev.angle <- 100

sim.decision(location, raster, prev.angle)

test <- sim.disperse(sim.turkey[1,], sim.world, sim.world)


move.sim[[1]][1,]


#####################################################################
