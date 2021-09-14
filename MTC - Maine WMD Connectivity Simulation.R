################################################################################################
### Packages
lapply(c("sf", "raster", "dplyr"), require, character.only = T)

################################################################################################
### Load Necessary Datasets
forbin.rast <- raster("./GIS/TurkeyConnectivity/ForBinNoIsle.tif")
wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp")

################################################################################################
### Data Prep


################################################################################################
### Generate Turkeys
# Create Turkeys with initial locations within WMDs according to 
# IPM estimates/Forest Binary with Islands removed

# Select Movement behaviors according to calibrated parameter distributions


################################################################################################
### Simulate Movements
for(nturk in 1:nrow(TurkData)){
  
  
}

################################################################################################
#### Organize Results