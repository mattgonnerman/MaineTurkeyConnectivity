# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate", "raster"), require, character.only = TRUE)

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
  filter(Nest.Attempt == 1) %>%
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
### Starting Towns
caplocs1 <- merge(capsites.sf, trap.slim, by = "CapLoc", all.y = T) %>%
  dplyr::select(BirdID, BirdID2, StartDate = CapDate) %>%
  mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y")) %>%
  mutate(StartYear = year(StartDate))
caplocs2 <- st_join(caplocs1, townbound, join = st_within) %>%
  st_drop_geometry() %>%
  rename(StartTown = Town)

### Combine with End Points
#Harvest
hasbirdID1 <- harvest.slim %>% filter(!is.na(BirdID)) %>% dplyr::select(-BirdID2)
nobirdID1 <- harvest.slim %>% filter(is.na(BirdID))  %>% dplyr::select(-BirdID)

caplocs.harv1 <- merge(caplocs2, hasbirdID1, by = "BirdID", all.y = T) %>%
  dplyr::select(-HarvSeason)
caplocs.harv2 <- merge(caplocs2, nobirdID1, by = "BirdID2", all.y = T) %>%
  dplyr::select(-HarvSeason)
startend.harv <- rbind(caplocs.harv1, caplocs.harv2) %>%
  filter(StartYear == EndYear) %>%
  mutate(Disperser = ifelse(StartTown == EndTown, 0, 1))  %>%
  dplyr::select(BirdID, Year = StartYear, StartTown, EndTown, Disperser)

#Nests
nest.ready <- nest.slim %>%
  filter(!is.na(EndLat)) %>%
  filter(!is.na(estDate)) %>%
  rename(EndDate = estDate) %>%
  mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y")) %>%
  st_as_sf(coords = c("EndLong", "EndLat"), crs = 4326)

nestends <- st_join(nest.ready, townbound, join = st_within) %>%
  st_drop_geometry() %>%
  rename(EndTown = Town) %>%
  mutate(EndYear = year(EndDate))
startend.nest <- merge(caplocs2, nestends, by = "BirdID", all.y = T) %>%
  filter(StartYear == EndYear) %>%
  mutate(Disperser = ifelse(StartTown == EndTown, 0, 1)) %>%
  dplyr::select(BirdID, Year = StartYear, StartTown, EndTown, Disperser)

#RBind
startend.full <- rbind(startend.harv, startend.nest)

#################################################################################################################
### COLLECT TOWN HABITAT DATA ###
#################################
townlist <- unique(c(startend.full$StartTown, startend.full$EndTown))
town.reduce <- townbound %>% filter(Town %in% townlist)

#NLCD
NLCDrast <- raster("E:/Maine Drive/GIS/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
# NLCDrast <- projectRaster(NLCDrast, crs = 4326)

town_extract <- extract(NLCDrast, town.reduce) #creates a list for each polygon of all the cell values within it

town_proportions <- lapply(town_extract, FUN= function(x){prop.table(table(x))}) #this returns proportion of each as a list
names(town_proportions) <- town.reduce$Town
rbind.fill <- function(x) {
  nam <- sapply(x, names)
  unam <- unique(unlist(nam))
  len <- sapply(x, length)
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    out[[i]] <- unname(x[[i]])[match(unam, nam[[i]])]
  }
  setNames(as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE), unam)
}
town_percentcover <- rbind.fill(town_proportions) #dataframe of proportions, but no names for columns
colnames(town_percentcover) <- paste("ID", colnames(town_percentcover), sep = "" )
town_percentcover[is.na(town_percentcover)] <- 0
town_percentcover <- town_percentcover %>% 
  mutate(Developed = ID21 + ID22 + ID23 + ID24) %>%
  mutate(Agriculture = ID81 + ID82) %>%
  mutate(Wetland = ID90 + ID95) %>%
  mutate(Grassland = ID71) %>%
  dplyr::select(Developed, Agriculture, Wetland, Grassland)


town.covs1 <- cbind(town.reduce, town_percentcover)

#Roads
Roadslines <- st_combine(st_read("E:/Maine Drive/GIS/Roads/medotpubrds.shp")) %>% 
  st_transform(4326)
roadsclipped <- st_intersection(Roadslines, town.reduce)
roadbytown <- st_join(st_as_sf(roadsclipped), town.reduce, st_intersects) %>%
  st_drop_geometry() %>%
  arrange(Town)
roadlength.km <- st_length(roadsclipped)/1000

poly = st_as_sf(town.reduce)
line = st_as_sf(Roadslines)
# intersection
int = st_intersection(line, poly)
# find out about the length of each line segment
int$len = st_length(int)
# use a meaningful id (so far consists only of 0s)
poly$Id = 1:nrow(poly)
# spatial overlay
join = st_join(poly, st_as_sf(int))
# use the ID of the polygon for the aggregation
out = group_by(join, Town.x) %>%
  summarize(length = sum(len))
# find out about polygons without line segments 
filter(out, is.na(length))
# you can set the length of the polygons without line intersections to 0 
# if you want

roadbytown <- st_drop_geometry(out) %>% 
  mutate(length = ifelse(is.na(length), 0, length)) %>%
  mutate(length = length/1000) %>% 
  rename(Town = Town.x, Road_KM = length)

towns.covs2 <- merge(town.covs1, roadbytown, by = "Town", all.x = T) %>% 
  rename(StartTown = Town)

#Lat/Long

town.covs3 <- cbind(towns.covs2, st_coordinates(st_transform(st_centroid(town.reduce), 32619))[,1:2])

town.covs <- merge(startend.full, town.covs3, by = "StartTown", all.x = T) %>%
  dplyr::select(-geometry)

#################################################################################################################
### Individual Specific Covariates ###
######################################
ind.covs <- trap.slim %>%
  dplyr::select(BirdID, Sex, Age, Flock.Size)
  
#Merge back to observed dispersal info
disp.input <- merge(town.covs, ind.covs, by = "BirdID", all.x = T) %>%
  mutate(Sex = as.factor(Sex),
         Age = as.factor(Age))

write.csv(disp.input, "DispProp_input.csv", row.names = F)

#################################################################################################################
### RUN MODELS AND MODEL SELECTION ###
#################################
require(AICcmodavg)
cand.models <- list()
cand.models[[1]] <- dispmodel.null <- glm(Disperser ~ 1, data = disp.input, family = "binomial")
cand.models[[2]] <- dispmodel.Dev <- glm(Disperser ~ Developed, data = disp.input, family = "binomial")
cand.models[[3]] <- dispmodel.Ag <- glm(Disperser ~ Agriculture, data = disp.input, family = "binomial")
cand.models[[4]] <- dispmodel.Wet <- glm(Disperser ~ Wetland, data = disp.input, family = "binomial")
cand.models[[5]] <- dispmodel.Grass <- glm(Disperser ~ Grassland, data = disp.input, family = "binomial")
cand.models[[6]] <- dispmodel.Road <- glm(Disperser ~ Road_KM, data = disp.input, family = "binomial")
cand.models[[7]] <- dispmodel.X <- glm(Disperser ~ X, data = disp.input, family = "binomial")
cand.models[[8]] <- dispmodel.Y <- glm(Disperser ~ Y, data = disp.input, family = "binomial")
cand.models[[9]] <- dispmodel.XYInt <- glm(Disperser ~ X*Y, data = disp.input, family = "binomial")
cand.models[[10]] <- dispmodel.Dev2 <- glm(Disperser ~ poly(Developed,2), data = disp.input, family = "binomial")
cand.models[[11]] <- dispmodel.Ag2 <- glm(Disperser ~ poly(Agriculture,2), data = disp.input, family = "binomial")
cand.models[[12]] <- dispmodel.Wet2 <- glm(Disperser ~ poly(Wetland,2), data = disp.input, family = "binomial")
cand.models[[13]] <- dispmodel.Grass2 <- glm(Disperser ~ poly(Grassland,2), data = disp.input, family = "binomial")
cand.models[[14]] <- dispmodel.Road2 <- glm(Disperser ~ poly(Road_KM,2), data = disp.input, family = "binomial")
cand.models[[15]] <- dispmodel.Age <- glm(Disperser ~ Age, data = disp.input, family = "binomial")
cand.models[[16]] <- dispmodel.Sex <- glm(Disperser ~ Sex, data = disp.input, family = "binomial")
cand.models[[17]] <- dispmodel.AgeSex <- glm(Disperser ~ Age*Sex, data = disp.input, family = "binomial")
cand.models[[18]] <- dispmodel.Dev2Ag2 <- glm(Disperser ~ poly(Developed,2) + poly(Agriculture,2), data = disp.input, family = "binomial")


aictab(cand.set = cand.models)

### Developed^2
summary(dispmodel.Dev2)
pred.data <- data.frame(Developed = seq(min(disp.input$Developed),max(disp.input$Developed), .01))
disp.prop.predict <- predict(dispmodel.Dev2, pred.data, se.fit = T, interval = "confidence")
pred.data$Est <- disp.prop.predict$fit
pred.data$SE <- disp.prop.predict$se.fit
outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
                                LCL = exp(Est-SE)/(1+exp(Est-SE)),
                                UCL = exp(Est+SE)/(1+exp(Est+SE)))

DP.dev.plot <- ggplot(data = outputs.dev) +
  geom_line(aes(x = Developed, y = Mean)) +
  geom_line(aes(x = Developed, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Developed, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion Developed", y = "Probability of Dispersal") +
  ylim(0,.8)

### Road
summary(dispmodel.Road)
pred.data <- data.frame(Road_KM = seq(min(disp.input$Road_KM),max(disp.input$Road_KM), 1))
disp.prop.predict <- predict(dispmodel.Road, pred.data, se.fit = T, interval = "confidence")
pred.data$Est <- disp.prop.predict$fit
pred.data$SE <- disp.prop.predict$se.fit
outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
                                    LCL = exp(Est-SE)/(1+exp(Est-SE)),
                                    UCL = exp(Est+SE)/(1+exp(Est+SE)))

DP.road.plot <- ggplot(data = outputs.dev) +
  geom_line(aes(x = Road_KM, y = Mean)) +
  geom_line(aes(x = Road_KM, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Road_KM, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Road (km)", y = element_blank()) +
  ylim(0,.8)

### Agriculture^2
summary(dispmodel.Ag2)
pred.data <- data.frame(Agriculture = seq(min(disp.input$Agriculture),max(disp.input$Agriculture), .01))
disp.prop.predict <- predict(dispmodel.Ag2, pred.data, se.fit = T, interval = "confidence")
pred.data$Est <- disp.prop.predict$fit
pred.data$SE <- disp.prop.predict$se.fit
outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
                                    LCL = exp(Est-SE)/(1+exp(Est-SE)),
                                    UCL = exp(Est+SE)/(1+exp(Est+SE)))

DP.ag.plot <- ggplot(data = outputs.dev) +
  geom_line(aes(x = Agriculture, y = Mean)) +
  geom_line(aes(x = Agriculture, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Agriculture, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion Agriculture", y = element_blank()) +
  ylim(0,.8)

### Group Plots
require(patchwork)

top.model.plots <- DP.dev.plot + DP.road.plot + DP.ag.plot +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)

ggsave(top.model.plots, file = "./Results/DispersalPropensity_topmodels.jpg",
       height = 5, width = 12)
