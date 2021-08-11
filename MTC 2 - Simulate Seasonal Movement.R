####################################
### SIMULATE DISPERSAL MOVEMENTS ###
####################################
# Simulate turkey dispersals from capture locations

### Load Packages
lapply(c("dplyr", "raster", "sf", "lubridate", "units"), require, character.only = TRUE)


### User-Defined variables used for simulations or setup
N.simturk <- 10 #The number of simulations PER STARTING POINTS
R <- 250 #Perception Distance, how far away will turkey still be able to consider a patch


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
  mutate(rho = runif(N.simturk*length(sim.turkey.list), 0.1, 5),
         k = runif(N.simturk*length(sim.turkey.list), 0.1, 4),
         theta = 2,
         tau = runif(N.simturk*length(sim.turkey.list), 0.1, 3.5))

move.sim <- list()
for(i in 1:nrow(sim.turkey)){
  move.sim[[i]] <- sim.turkey[i,] %>% rename(Long = StartX, Lat = StartY) %>% mutate(Step = 1)
}

### Simulate Movement
sim.disperse <- function(startpoint.df, rasterday, rasterroost){
  
}


### Function to simulate 1 decision for moving from one location to another on given raster, weighted
sim.decision <- function(location, raster, prev.angle){
  
  options <- as.data.frame(extract(raster, location[1,3:4], buffer = R, cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location[1,3:4])
  options$D <- as.data.frame(extract(D.raster, location[1,3:4], buffer = R, cellnumbers = T, df = T))[,3]
  options <- cbind(options, xyFromCell(raster, cell = options$CellID)) %>%
    mutate(A = geosphere::bearing(sp::spTransform(sp::SpatialPoints(coords = location[1,3:4], proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")),
                                  sp::spTransform(sp::SpatialPoints(coords = matrix(c(x,y), ncol =2), proj4string = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs ")),CRS("+proj=longlat +datum=WGS84 +no_defs ")))) %>%
    mutate(A = ifelse(A < 0, 360 + A, A),
           TurnA = 180 - abs(abs(A - prev.angle) - 180)) %>%
    # mutate(W = cell.selection.w(HS, D, A, location$rho[1], location$k[1], location$theta[1], location$tau[1]))
    mutate(W = runif(nrow(options), 0, 1))
  decision <- sample(1:nrow(options), 1, prob = options$W)
  
  return(options[decision,])
  
}


### Function to calculate the weight of a raster cell for sim.decision
cell.selection.w <- function(H, D, alpha, rho, k, theta, tau){
  sigma <- 1/tau
  sigma2 <- sigma^2
  w <- H^exp(rho - 1) * 1/(gamma(k) * (theta^k)) * D^(k-1) * exp(-D/theta) * 1/((2*pi*sigma2)) * exp(-(acos(alpha)^2)/(2*sigma2))
  return(w)
}

