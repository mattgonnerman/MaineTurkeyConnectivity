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
  dplyr::select(BirdID = Alum.Band.ID, BirdID2 = Rivet.ID, HarvYear = Year, HarvSeason = Season,
                HarvTown = Town.of.Harvest, CapTown = Town.of.Capture, HarvDate = Date.of.Harvest)

# Load Nest Data
nest.raw <- read.csv("Nest Monitoring - Nest Info.csv")
nest.slim <- nest.raw %>%
  dplyr::select(BirdID = Alum.Band.ID, NestYear = Year, Lat = NestLat, Long = NestLong, NestDate = Est.Laying.Initiation) %>%
  filter(!is.na(BirdID))

# Download Movement Data
# login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
# gpslocations.raw <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login)
# gps.slim <- gpslocations.raw@data %>%
#   dplyr::select(Lat = location_lat, Long = location_long, Date = timestamp) %>%
#   mutate(BirdID = substr(gpslocations.raw@trackId, 2, 10))

# Load Telemetry Data
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")
telem.slim <- telem.raw %>%
  dplyr::select(BirdID = AlumBand, TelemDate = Date, Fate, Lat = Lat1, Long = Long1)

# Load Capture Site Information
capsites.raw <- read.csv("CaptureSites - Sheet1.csv")
capsites.slim <- capsites.raw %>%
  dplyr::select(Town, CapLoc = Location.Name, Lat = Latitude, Long = Longitude)
capsites.sf <- st_as_sf(capsites.slim, coords = c("Long", "Lat"), crs = 4326) %>%
  dplyr::select(-Town)

# Load Town Boundaries Shapefile
townbound <- st_read("E:/Maine Drive/GIS/Maine_Boundaries_Town_and_Townships_Polygon-shp/Maine_Boundaries_Town_and_Townships_Polygon.shp") %>%
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
##################################################################################################################
### SUBSET TURKEYS TO DISPERSERS, DIRECT OBSERVATIONS ONLY
starttown <- merge(trap.slim, capsites.sf , by = "CapLoc", all.x = T) %>%
  dplyr::select(BirdID, BirdID2, StartDate = CapDate, StartTown = Town, CapLoc) %>%
  mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y")) %>%
  mutate(StartYear = year(StartDate))

nest.unfiltered <- nest.slim %>%
  dplyr::select(BirdID, NestLat = Lat, NestLong = Long, EndDate = NestDate) %>%
  mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y")) %>%
  filter(!is.na(NestLat)) %>% filter(!is.na(EndDate)) %>%
  mutate(EndYear = year(EndDate))

nest.directobs <- merge(starttown, nest.unfiltered, by = "BirdID", all.y = T) %>%
  filter(EndYear == StartYear) %>%
  dplyr::select(BirdID, CapLoc, StartTown, NestLat, NestLong) %>%
  group_by(BirdID) %>%
  slice(1L)

nest.do.points <- st_as_sf(nest.directobs, coords = c("NestLong", "NestLat"), crs = 4326)
nest.points <- st_join(nest.do.points, townbound, join = st_within) %>%
  rename(EndTown = Town) %>%
  filter(StartTown !=EndTown)

harvest.dispersed <- harvest.slim %>%
  filter(HarvTown != CapTown) %>%
  dplyr::select(BirdID, BirdID2, EndTown = HarvTown)

harvest.dispersed.1 <- harvest.dispersed %>% filter(!is.na(BirdID))
harvest.dispersed.1.merge <- merge(harvest.dispersed.1, trap.slim, by = "BirdID", all.x = T) %>%
  dplyr::select(BirdID, BirdID2 = BirdID2.y, CapLoc, EndTown)
harvest.dispersed.NA <- harvest.dispersed %>% filter(is.na(BirdID))
harvest.dispersed.NA.merge <- merge(harvest.dispersed.NA, trap.slim, by = "BirdID2", all.x = T) %>%
  dplyr::select(BirdID = BirdID.y, BirdID2, CapLoc, EndTown)
harvest.ready <- rbind(harvest.dispersed.NA.merge, harvest.dispersed.1.merge) %>%
  dplyr::select(-BirdID2) %>%
  rename(Town = EndTown)

###################################################################################################################
### SUSBET START AND END LOCATIONS FOR DISPERSERS ###
#####################################################
#Nests
nest.cap <- merge(nest.points, capsites.df, by = c("CapLoc"), all.x = T)
nest.cap$EndLong <- st_coordinates(nest.cap)[,1]
nest.cap$EndLat <- st_coordinates(nest.cap)[,2]
nest.cap <- nest.cap %>% st_drop_geometry()

nest.start <- nest.cap %>%
  dplyr::select(BirdID, CapLong, CapLat) %>%
  distinct() %>%
  arrange(BirdID)
nest.end <- nest.cap %>%
  dplyr::select(BirdID, EndLong, EndLat)%>%
  distinct() %>%
  arrange(BirdID)

#Harvests
harvest.start <- merge(harvest.ready, capsites.df, by = "CapLoc", all.x = T) %>%
  dplyr::select(BirdID, CapLong, CapLat)

harvest.end <- merge(harvest.ready, towncenter.df, by = "Town", all.x = T) %>%
  dplyr::select(BirdID, EndLong, EndLat)

#Merge
startlocs <- rbind(nest.start, harvest.start) %>%
  arrange(BirdID) %>%
  mutate(ID = as.numeric(as.factor(BirdID)))

endlocs <- rbind(nest.end, harvest.end) %>%
  arrange(BirdID) %>%
  mutate(ID = as.numeric(as.factor(BirdID)))

#save as shapefiles
disperser.start <- st_as_sf(startlocs, coords = c("CapLong", "CapLat"), crs = 4326)
disperser.end <- st_as_sf(endlocs, coords = c("EndLong", "EndLat"), crs = 4326)
st_write(disperser.start, dsn = "./GIS", layer = "Disperser Start.shp", driver = "ESRI Shapefile", delete_layer = T)
st_write(disperser.end, dsn = "./GIS", layer = "Disperser End.shp", driver = "ESRI Shapefile", delete_layer = T)
