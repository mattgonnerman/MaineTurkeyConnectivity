# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate"), require, character.only = TRUE)

#################################################################################################################
### LOAD DATA ###
#################
# Load Capture Data
trap.raw <- read.csv("Trapping - Data.csv")
trap.slim <- trap.raw %>%
  dplyr::select(BirdID = AlumBand, BirdID2 = Rivet.Band, CapLoc = Location, CapDate = Date, Recapture, Sex, Age, Flock.Size)

# Load Harvest Data
harvest.raw <- read.csv("Harvests - Harvested Birds.csv")
harvest.slim <- harvest.raw %>% 
  dplyr::select(BirdID = Alum.Band.ID, BirdID2 = Rivet.ID, EndYear = Year, HarvSeason = Season,
                EndTown = Town.of.Harvest)

# Load Nest Data
nest.raw <- read.csv("Nest Monitoring - Nest Info.csv")
nest.slim <- nest.raw %>%
  dplyr::select(BirdID = Alum.Band.ID, EndYear = Year, EndLat = NestLat, EndLong = NestLong, estDate = Est.Laying.Initiation) %>%
  filter(!is.na(BirdID))

# Download Movement Data
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
gpslocations.raw <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login)
gps.slim <- gpslocations.raw@data %>%
  dplyr::select(Lat = location_lat, Long = location_long, Date = timestamp) %>%
  mutate(BirdID = substr(gpslocations.raw@trackId, 2, 10))

# Load Capture Site Information
capsites.raw <- read.csv("CaptureSites - Sheet1.csv")
capsites.slim <- capsites.raw %>%
  dplyr::select(Town, CapLoc = Location.Name, Lat = Latitude, Long = Longitude)
capsites.sf <- st_as_sf(capsites.slim, coords = c("Long", "Lat"), crs = 4326) %>%
  dplyr::select(-Town)

# Load Town Boundaries Shapefile
townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  group_by(TOWN) %>%
  filter(TArea_KM2 == max(TArea_KM2)) %>%
  dplyr::select(Town = TOWN)
townbound <- st_transform(townbound, crs = 4326)
townbound.center.sf <- st_centroid(townbound)
towncenter.df <- data.frame(Town = townbound.center.sf$Town,
                            EndLong = st_coordinates(townbound.center.sf)[,1],
                            EndLat = st_coordinates(townbound.center.sf)[,2])

#Correct CapSite Names
capsites.sf <- st_join(capsites.sf, townbound, join = st_within) %>%
  mutate(CapLong =  st_coordinates(capsites.sf)[,1],
         CapLat =  st_coordinates(capsites.sf)[,2])
capsites.df <- capsites.sf %>% st_drop_geometry()


#################################################################################################################
### MERGE DATA ###
##################
# Starting Locations
caplocs <- merge(trap.slim, capsites.sf , by = "CapLoc", all.x = T) %>%
  dplyr::select(BirdID, BirdID2, StartDate = CapDate, StartTown = Town, CapLoc) %>%
  mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y")) %>%
  mutate(StartYear = year(StartDate))

harvest.ready <- harvest.slim
