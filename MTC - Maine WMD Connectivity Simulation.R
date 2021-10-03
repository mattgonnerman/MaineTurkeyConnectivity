################################################################################################
### Packages
lapply(c("sf", "raster", "dplyr", "lubridate", "units", "CircStats", "parallel"), require, character.only = T)

### Simulation Functions
source("./MTC - WMD Connectivity Functions.R")

################################################################################################
### Load and Prepare Data
forbin.rast <- raster("./GIS/ExtendedRasters/FullForestBin.tif")
memory.limit(64000)
values(forbin.rast)[values(forbin.rast) == 0] <- NA

wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  arrange(WMD) %>%
  st_transform(crs(forbin.rast)) %>%
  filter(WMD != 29)
wmdabun <- read.csv("./WMDTurkeyAbundance.csv")


#Prep Movement Initiation and Settling Decision Models
source("./MTC - Dispersal Propensity.R")
source("./MTC - Settling Selection.R")

#Get Hex specific settling and dispersal decision probability
hexdispprob <- read.csv("HexCov_Disp.csv")
hexsettleprob <- read.csv("HexCov_Settle.csv")
hexcovs.raw <- st_read("./GIS/HexCovs.shp") %>%
  st_transform(crs(forbin.rast))

hexcovs.1 <- merge(hexcovs.raw, hexdispprob, by = "GridID", all.x = T)
hexcovs <- merge(hexcovs.1, hexsettleprob, by = "GridID", all.x = T)

#Habitat Suitability Surface
HS_day <- raster("./GIS/ExtendedRasters/FullHS_Day.tif")
names(HS_day) <- "layer"
HS_roost <- raster("./GIS/ExtendedRasters/FullHS_Roost.tif")
names(HS_roost) <- "layer"


################################################################################################
### Generate Turkeys
# Create Turkeys with initial locations within WMDs according to 
# IPM estimates/Forest Binary with Islands removed
rm(startlocs.sf)
for(i in 1:nrow(wmdbound)){
  points.df <- as.data.frame(sampleRandom(forbin.rast, wmdabun$TotalTurk[i]*10, ext = extent(wmdbound[i,]),
                            na.rm = T, xy = T))
  points.df$IntendedWMD <- i-1
  points.sf <- st_as_sf(points.df, coords = c("x", "y"), crs = crs(wmdbound)) %>%
    st_join(., wmdbound, join = st_intersects) %>%
    filter(IntendedWMD == WMD) %>%
    slice(1:wmdabun$TotalTurk[i]) %>%
    dplyr::select(WMD)
  if(exists("startlocs.sf")){
    startlocs.sf <- rbind(startlocs.sf, points.sf)
  }else{
    startlocs.sf <- points.sf
  }
}

#Duplicate starting locations, 1 for male 1 for female, and combine into single object
startlocs.sf <- rbind(startlocs.sf %>% mutate(Sex = "M"), startlocs.sf %>% mutate(Sex = "F"))

#Load Posterior Distributions for hyperparameters
source("./MTC - Rejection Sampling Posteriors.R")

# Select Movement behaviors according to calibrated parameter distributions
startlocs.df <- startlocs.sf %>% 
  mutate(StartX = st_coordinates(startlocs.sf)[,1],
         StartY = st_coordinates(startlocs.sf)[,2], 
         p = ifelse(Sex == "M", r_p_M(n=nrow(startlocs.sf)), r_p_F(n=nrow(startlocs.sf))),
         rho = ifelse(Sex == "M", r_rho_M(n=nrow(startlocs.sf)), r_rho_F(n=nrow(startlocs.sf))),
         mu = 0,
         k = ifelse(Sex == "M", r_k_M(n=nrow(startlocs.sf)), r_k_F(n=nrow(startlocs.sf))),
         rate = ifelse(Sex == "M", r_rate_M(n=nrow(startlocs.sf)), r_rate_F(n=nrow(startlocs.sf)))) %>%
  mutate(R = qgamma(.95, shape = k, scale = 1/rate)) %>%
  st_drop_geometry() %>%
  rename(Long = StartX, Lat = StartY, StartWMD = WMD) %>% 
  mutate(Step = 0) %>%
  mutate(BirdID = row_number())
  
st_write(startlocs.sf, "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.shp", delete_layer = T)

startlocs.df <- startlocs.df %>%
  mutate(p = ifelse(p <= 0, ifelse(Sex == "M", r_p_M(n=1), r_p_F(n=1)), p),
         rho = ifelse(rho <= 0, ifelse(Sex == "M", r_rho_M(n=1), r_rho_F(n=1)), rho))
write.csv(startlocs.df, "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.csv", row.names = F)

################################################################################################
### Simulate Movements
# n.cores <- parallel::detectCores() /2
# my.cluster <- parallel::makeCluster(
#   n.cores, 
#   type = "PSOCK"
# )
# clusterEvalQ(my.cluster, {lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)})
# clusterExport(my.cluster, c("startlocs.df", "hexcovs", "lasthex", "N.steps.max", "HS_day", "HS_roost", "forbin.rast"))
# 
# sim.output.list <- parLapply(cl = my.cluster, X = 1:10,
#                              function(simbird) {
#                                source("./MTC - WMD Connectivity Functions.R")
#                                simwmdconnect(startlocs.df[simbird,])
#                              })
# parallel::stopCluster(cl = my.cluster)
# sim.output <- do.call("bind_rows", sim.output.list)
# # filelocation <- paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird", ogbird, "Set", set, ".csv", sep = "_")
# # write.csv(sim.output, filelocation, append = T)
# 
# sim.output.list <- lapply(X = 1:10, FUN = function(simbird) {
#   source("./MTC - WMD Connectivity Functions.R")
#   simwmdconnect(startlocs.df[simbird,])
# })
# sim.output <- do.call("bind_rows", sim.output.list)

read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.csv")

lapply(X = 901:nrow(startlocs.df), FUN = function(simbird) {
  source("./MTC - WMD Connectivity Functions.R")
  simwmdconnect(startlocs.df[simbird,])
})


################################################################################################
#### Organize Results


