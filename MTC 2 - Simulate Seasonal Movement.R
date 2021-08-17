####################################
### SIMULATE DISPERSAL MOVEMENTS ###
####################################
# Simulate turkey dispersals from capture locations

### Load Packages
lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)

#################################
### PREPARE SIMULATION INPUTS ###
#################################

### User-Defined variables used for simulations or setup
N.simturk <- 100 #The number of simulations PER STARTING POINTS
# R <- 250 #Perception Distance, how far away will turkey still be able to consider a patch
N.steps.max <- 15*10 #15 steps * number of days
end.dist <- 1000 #Distance simulation needs to be to end point to conclude individual simulation

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
  mutate(R = runif(N.simturk*length(sim.turkey.list), 150, 450),
         p = runif(N.simturk*length(sim.turkey.list), .1, 5),
         rho = runif(N.simturk*length(sim.turkey.list), 0.26868487 - (10*0.01281980), 0.26868487 + (10*0.01281980)),
         # mu = runif(N.simturk*length(sim.turkey.list), 0.08464538 - (2*0.04287019), 0.08464538 + (2*0.04287019)),
         mu = rep(0, N.simturk*length(sim.turkey.list)),
         k = runif(N.simturk*length(sim.turkey.list), 0.84834637 - (10*1.744392e-02), 0.84834637 + (10*1.744392e-02)),
         theta = runif(N.simturk*length(sim.turkey.list), 0.00311007 - (10*7.517423e-05), 0.00311007 + (10*7.517423e-05))) %>% 
  rename(Long = StartX, Lat = StartY) %>% 
  mutate(Step = 0)

source("./MTC 2a - Simulation Functions.R")


###########################
### RUN CODE - PARALLEL ###
###########################
lapply(c("doParallel", "foreach", "parallel"), require, character.only = TRUE)

### parLapply version
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
clusterEvalQ(my.cluster, {lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)})
clusterExport(my.cluster, c("sim.turkey", "N.steps.max", "HS_day", "HS_roost", "end.dist"))

sim.output.list <- parLapply(cl = my.cluster, X = (N.simturk*(15-1)+1):(N.simturk*(15)),
                         function(simbird) {
                           source("./MTC 2a - Simulation Functions.R")
                           sim.disperse(sim.turkey[simbird,], HS_day, HS_roost)
                         })
sim.output <- do.call("bind_rows", sim.output.list)
parallel::stopCluster(cl = my.cluster)


####################
### CREATE PLOTS ###
####################
sim.output <- read.csv("./Simulations/AllOutputs.csv")

### Output line shapefile
sim.lines <- sim.output %>% 
  mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
  st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(32619) %>% 
  group_by(LineID) %>% 
  arrange(Step) %>%
  summarize(m = mean(HS, na.omit = T), do_union = F) %>%
  st_cast("LINESTRING")

### Plot on raster
#Change Habitat suitability raster to dataframe for use in GGplot


#create start and end point sf objects
start <- sim.output3[1, c("x","y")] %>%
  st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(32619)
end <- sim.output3[1, c("EndX","EndY")]%>%
  st_as_sf(coords = c("EndX","EndY")) %>% 
  sf::st_set_crs(32619)



# rastext <- st_read("./GIS/Disperser End.shp") %>% st_transform(32619)
rastext <-merge(extent(sim.lines), extent(end))
HS_df <- as.data.frame(rasterToPoints(crop(HS_day, rastext)))

require(ggplot2)
ggplot(data = sim.lines) +
  geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(aes(color = LineID), show.legend = F) +
  geom_sf(data = start, color = "purple", size = 4, shape = 20) +
  geom_sf(data = end, color = "red", size = 4, shape = 20) +
  theme_classic() +
  scale_fill_continuous(type = "viridis")


###################
#### TEST CODE ####
###################
#Test sim.disperse()
startpoint.df <- sim.turkey[3,]
rasterday <- HS_day
rasterroost <- HS_roost
test <- sim.disperse(sim.turkey[3,], HS_day, HS_roost)
test.list <- list()
test.list[[10]] <- test





sim.lines <- sim.output %>%
  mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
  st_as_sf(coords = c("x","y")) %>%
  sf::st_set_crs(32619) %>%
  group_by(LineID) %>%
  arrange(Step) %>%
  summarize(m = mean(HS, na.omit = T), do_union = F) %>%
  st_cast("LINESTRING")

start <- sim.output[sim.output$Step == 0, c("x","y")] %>%
  st_as_sf(coords = c("x","y")) %>%
  sf::st_set_crs(32619)
end <- sim.output[sim.output$Step == 0, c("EndX","EndY")]%>%
  st_as_sf(coords = c("EndX","EndY")) %>%
  sf::st_set_crs(32619)
# rastext <- st_read("./GIS/Disperser End.shp") %>% st_transform(32619)
rastext <-merge(extent(sim.lines), extent(end))
HS_df <- as.data.frame(rasterToPoints(crop(HS_day, rastext)))

require(ggplot2)
ggplot(data = sim.lines) +
  geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(aes(color = LineID), show.legend = F, lwd = 1) +
  geom_sf(data = start, color = "purple", size = 4, shape = 20) +
  geom_sf(data = end, color = "red", size = 4, shape = 20) +
  theme_classic() +
  scale_fill_continuous(type = "viridis")

#####################################################################