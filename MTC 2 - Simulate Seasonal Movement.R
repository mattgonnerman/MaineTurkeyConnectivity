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
sim.turkey <- do.call(rbind.data.frame, sim.turkey.list)

move.sim <- list()
for(i in 1:nrow(sim.turkey)){
  move.sim[[i]] <- sim.turkey[i,] %>% rename(Long = StartX, Lat = StartY) %>% mutate(Step = 1)
}

### Simulate Movement
sim.disperse <- function(startpoint.df, raster){
  
}



sim.decision <- function(location, raster, prev.angle){
  
  options <- as.data.frame(extract(raster, location, buffer = R, cellnumbers = T, df = T)) %>%
    rename(HS = layer, CellID = cells)
  D.raster <- raster::distanceFromPoints(rasterFromCells(raster, options$CellID), location)
  options$D <- as.data.frame(extract(D.raster, location, buffer = R, cellnumbers = T, df = T))[,3]
  options <- cbind(options, xyFromCell(raster, cell = options$CellID)) %>%
    mutate(A.rad = base::atan2(cos(location[,2])*sin(y)-sin(location[,2]*cos(y)&cos(x-location[,1])) , #Y
                               cos(y)*sin(y-location[,1])), #X
           A.deg = drop_units(set_units(as_units(A.rad, "radians"), "degrees")),
           A.deg = ifelse(A.deg < 0, 360 + A.deg, A.deg),
           A.diff = )
  return(options)
  
}
test <- sim.decision(move.sim[[1]][1,3:4], sim.world) #, runif(1, 0, 360))



