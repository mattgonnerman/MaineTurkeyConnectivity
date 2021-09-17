####################################
### SIMULATE DISPERSAL MOVEMENTS ###
####################################
# Simulate turkey dispersals from capture locations

for(set in 14){
  ### Load Packages
  lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)
  
  #################################
  ### PREPARE SIMULATION INPUTS ###
  #################################
  
  ### User-Defined variables used for simulations or setup
  N.simturk <- 100 #The number of simulations PER STARTING POINTS
  # R <- 250 #Perception Distance, how far away will turkey still be able to consider a patch
  N.steps.max <- 15*30 #15 steps * number of days
  
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
  
  obs.paths <- merge(startlocs, endlocs, by = c("BirdID", "ID", "ObsType")) %>%
    arrange(ID) %>%
    rename(OG.ID = ID)
  
  
  ### Load/Setup World
  slocs <- st_read("./GIS/Disperser Start.shp")
  elocs <- st_read("./GIS/Disperser End.shp") %>%
    mutate(Sex = NA, Age = NA, Flock_Size = NA)
  bothlocs <- st_transform(rbind(slocs, elocs), 32619)
  HS_day <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Day.tif")
  names(HS_day) <- "layer"
  HS_roost <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Roost.tif")
  names(HS_roost) <- "layer"
  
  
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
           rho = runif(N.simturk*length(sim.turkey.list), 0.0000000000001, 0.26868487 + (20*0.01281980)),
           # mu = runif(N.simturk*length(sim.turkey.list), 0.08464538 - (2*0.04287019), 0.08464538 + (2*0.04287019)),
           mu = rep(0, N.simturk*length(sim.turkey.list)),
           k = runif(N.simturk*length(sim.turkey.list), 0.84834637 - (20*1.744392e-02), 0.84834637 + (20*1.744392e-02)),
           rate = runif(N.simturk*length(sim.turkey.list), 0.00311007 - (20*7.517423e-05), 0.00311007 + (20*7.517423e-05))) %>% 
    mutate(R = qgamma(.95, shape = k, scale = 1/rate)) %>%
    rename(Long = StartX, Lat = StartY) %>% 
    mutate(Step = 0)
  
  source("./MTC - Simulation Functions.R")
  
  ##########################################################################################################
  ###########################
  ### RUN CODE - PARALLEL ###
  ###########################
  lapply(c("parallel"), require, character.only = TRUE)
  
  ### parLapply version
  for(ogbird in 1:nrow(sim.turkey)){
    # Sampling distance is dependent on observation type (Harvest vs Nest)
    # end.dist <- ifelse(obs.paths$ObsType[ogbird] == "H", 6852.906, 1922.514) #Distance simulation needs to be to end point to conclude individual simulation
    
    n.cores <- parallel::detectCores() - 1
    my.cluster <- parallel::makeCluster(
      n.cores, 
      type = "PSOCK"
    )
    clusterEvalQ(my.cluster, {lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)})
    clusterExport(my.cluster, c("sim.turkey", "N.steps.max", "HS_day", "HS_roost"))
    system.time(
      sim.output.list <- parLapply(cl = my.cluster, X = (N.simturk*(ogbird-1)+1):(N.simturk*(ogbird)),
                                   function(simbird) {
                                     source("./MTC - Simulation Functions.R")
                                     sim.disperse(sim.turkey[simbird,], HS_day, HS_roost)
                                   })
    )
    sim.output <- do.call("bind_rows", sim.output.list)
    filelocation <- paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird", ogbird, "Set", set, ".csv", sep = "_")
    write.csv(sim.output, filelocation, append = T)
    parallel::stopCluster(cl = my.cluster)
  }
}
