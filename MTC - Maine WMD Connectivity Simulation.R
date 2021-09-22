################################################################################################
### Packages
lapply(c("sf", "raster", "dplyr", "lubridate", "units", "CircStats"), require, character.only = T)

### Simulation Functions
source("./MTC - WMD Connectivity Functions.R")

################################################################################################
### Load and Prepare Data
forbin.rast <- raster("./GIS/ForBinLand.tif")
wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  arrange(WMD) %>%
  st_transform(crs(forbin.rast))
wmdabun <- read.csv("./WMDTurkeyAbundance.csv")

values(forbin.rast)[values(forbin.rast) == 0] <- NA


#Prep Movement Initiation and Settling Decision Models
source("./MTC - Dispersal Propensity.R")
source("./MTC - Settling Selection.R")

#Get town covariates and estimate settling and dispersal decision probability
towncovs <- st_read("./GIS/TownCovs.shp") %>% st_transform(crs(forbin.rast)) %>%
  rename(Developed = Develpd, Agriculture = Agrcltr) %>%
  mutate(Y = (Y - mean(disp.input.raw$Y))/sd(disp.input.raw$Y),
         Wetland = (Wetland - mean(disp.input.raw$Wetland))/sd(disp.input.raw$Wetland),
         Developed = (Developed - mean(disp.input.raw$Developed))/sd(disp.input.raw$Developed),
         Agriculture = (Agriculture - mean(disp.input.raw$Agriculture))/sd(disp.input.raw$Agriculture))
towncovs$startmove.M <- predict(dispmodel.Final, towncovs %>% mutate(Sex = "M"), type = "response")
towncovs$startmove.F <- predict(dispmodel.Final, towncovs %>% mutate(Sex = "F"), type = "response")

#Habitat Suitability Surface
HS_day <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Day.tif")
names(HS_day) <- "layer"
HS_roost <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Roost.tif")
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

#Load Posterior Distributions for hyperparameters
source("./MTC - Rejection Sampling Posteriors.R")

# Select Movement behaviors according to calibrated parameter distributions
startlocs.df <- startlocs.sf %>% 
  mutate(StartX = st_coordinates(startlocs.sf)[,1],
         StartY = st_coordinates(startlocs.sf)[,2], 
         p = r_p(n=nrow(startlocs.sf)),
         rho = r_rho(n=nrow(startlocs.sf)),
         mu = 0,
         k = r_k(n=nrow(startlocs.sf)),
         rate = r_rate(n=nrow(startlocs.sf))) %>%
  mutate(R = qgamma(.95, shape = k, scale = 1/rate)) %>%
  st_drop_geometry() %>%
  rename(Long = StartX, Lat = StartY, StartWMD = WMD) %>% 
  mutate(Step = 0)
  

################################################################################################
### Simulate Movements
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
clusterEvalQ(my.cluster, {lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats"), require, character.only = TRUE)})
clusterExport(my.cluster, c("sim.turkey", "N.steps.max", "HS_day", "HS_roost"))

sim.output.list <- parLapply(cl = my.cluster, X = (N.simturk*(ogbird-1)+1):(N.simturk*(ogbird)),
                             function(simbird) {
                               source("./MTC - Simulation Functions.R")
                               sim.disperse(sim.turkey[simbird,], HS_day, HS_roost)
                             })

sim.output <- do.call("bind_rows", sim.output.list)
filelocation <- paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird", ogbird, "Set", set, ".csv", sep = "_")
write.csv(sim.output, filelocation, append = T)
parallel::stopCluster(cl = my.cluster)


################################################################################################
#### Organize Results


