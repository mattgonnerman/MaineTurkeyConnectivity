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

# Select Movement behaviors according to calibrated parameter distributions
startlocs.sf %>%
  

################################################################################################
### Simulate Movements
for(nturk in 1:nrow(TurkData)){
  
  
}

################################################################################################
#### Organize Results