### Dispersal propensity ###
# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf"), require, character.only = TRUE)

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
  dplyr::select(BirdID = Alum.Band.ID, BirdID2 = Rivet.ID, HarvYear = Year, HarvSeason = Season,
                HarvTown = Town.of.Harvest, CapTown = Town.of.Capture, HarvDate = Date.of.Harvest)

# Load Nest Data
nest.raw <- read.csv("Nest Monitoring - Nest Info.csv")
nest.slim <- nest.raw %>%
  dplyr::select(BirdID = Alum.Band.ID, NestYear = Year, Lat = NestLat, Long = NestLong, NestDate = Est.Laying.Initiation)

# Download Movement Data
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
gpslocations.raw <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login)
gps.slim <- gpslocations.raw@data %>%
  dplyr::select(Lat = location_lat, Long = location_long, Date = timestamp) %>%
  mutate(BirdID = substr(gpslocations.raw@trackId, 2, 10))

# Load Telemetry Data
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")
telem.slim <- telem.raw %>%
  dplyr::select(BirdID = AlumBand, TelemDate = Date, Fate, Lat = Lat1, Long = Long1)

# Load Capture Site Information
capsites.raw <- read.csv("CaptureSites - Sheet1.csv")
capsites.slim <- capsites.raw %>%
  dplyr::select(Town, CapLoc = Location.Name, Lat = Latitude, Long = Longitude)

capsites.sf <- st_as_sf(capsites.slim, coords = c("Long", "Lat"), crs = projection(gpslocations.raw))
st_write(capsites.sf, dsn = "./GIS", layer = "CaptureSites.shp", driver = "ESRI Shapefile")

# Load Town Boundaries Shapefile
townbound <- st_read("E:/Maine Drive/GIS/Maine_Boundaries_Town_and_Townships_Polygon-shp/Maine_Boundaries_Town_and_Townships_Polygon.shp") %>%
  dplyr::select(Town = TOWN)

###################################################################################################################
### STARTING LOCATION ###
#########################
startloc <- merge(trap.slim, capsites.slim , by = "CapLoc", all.x = T) %>%
  dplyr::select(BirdID, BirdID2, Date = CapDate, Town) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))



###################################################################################################################
### FINAL LOCATION ###
######################
harvest.floc <- harvest.slim %>% dplyr::select(BirdID, BirdID2, Date = HarvDate, Town = HarvTown) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(DataSource = "Harvest")

nest.floc <- nest.slim %>% dplyr::select(BirdID, Date = NestDate, Lat, Long) 
