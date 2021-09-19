################################################################################################
### Packages
lapply(c("sf", "raster", "dplyr"), require, character.only = T)

################################################################################################
### Load Data
forbin.rast <- raster("./GIS/ForBinLand.tif")
wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  arrange(WMD) %>%
  st_transform(crs(forbin.rast))
wmdabun <- read.csv("./WMDTurkeyAbundance.csv")

################################################################################################
### Data Prep
values(forbin.rast)[values(forbin.rast) == 0] <- NA

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
for(nturk in 1:nrow(TurkData)){
  
  
}

################################################################################################
#### Organize Results