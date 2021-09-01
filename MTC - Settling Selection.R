# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate", "raster", "tidyr"), require, character.only = TRUE)

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
### IDENTIFY AVAILABLE TOWNS ###
################################
### Draw Line between start and end town
town.coords <- townbound.center.sf %>%
  st_drop_geometry()
town.coords$X <- st_coordinates(townbound.center.sf)[,1]
town.coords$Y <- st_coordinates(townbound.center.sf)[,2]

lines.start <- startend.full %>% 
  filter(Disperser == 1) %>%
  dplyr::select(BirdID, Town = StartTown)
lines.end <- startend.full %>% 
  filter(Disperser == 1) %>%
  dplyr::select(BirdID, Town = EndTown)

lines.start <- merge(lines.start, town.coords, by = "Town", all.x = T)
lines.end <- merge(lines.end, town.coords, by = "Town", all.x = T)

#Combine nest and capture/first of year locations and create shapefiles
lines <- rbind(lines.start, lines.end) %>%
  arrange(BirdID) %>%
  group_by(BirdID) %>%
  split(.,.[,"BirdID"])
lines.mat <- lapply(lines, function(x) as.matrix(x[names(x) %in% c("X","Y")]))
lines.multilines.sfg <- st_multilinestring(lines.mat, dim = "XY")
lines.multilines.sfc <- st_sfc(lines.multilines.sfg, crs = 4326)
lines.multilines.sfc <- st_transform(lines.multilines.sfc, 32619)
lines.lines.sfc <- st_cast(lines.multilines.sfc, "LINESTRING")
captoobs.line <- st_sf(data.frame(lines.lines.sfc, data.frame(BirdID = names(lines))))
st_write(captoobs.line, "./GIS/SettleAvailLine.shp", delete_layer = T)


### Buffer line based on average town size
# Mean town size
mean.town.area <- mean(st_area(townbound))
mean.town.radius <- (mean.town.area^(1/2))/pi
captoobs.poly <- st_buffer(captoobs.line, 2*mean.town.radius) %>%
  st_transform(4326)
st_write(captoobs.poly, "./GIS/SettleAvailPoly.shp", delete_layer = T)
### Identify which towns overlap each buffer, these are the available

townoverlap <- st_intersects(captoobs.poly, townbound)

townoverlaploop <- function(x){
  ntowns <- length(townoverlap[[x]])
  townlist <- c()
  for(i in 1:ntowns){
    townlist[i] <- townbound[townoverlap[[x]][i],]$Town
  }
  return(townlist) 
}

townnames.overlap <- lapply(1:length(townoverlap), FUN = townoverlaploop)
names(townnames.overlap) <- paste(captoobs.poly$BirdID, "_", sep ="")

ncol.merge <- max(unlist(lapply(1:length(townoverlap), FUN = function(x){length(townnames.overlap[[x]])})))

mergeid <- lines.end %>% mutate(ID = paste(BirdID, Town, sep = "_"))

settled.towns <- data.frame(Town = unlist(townnames.overlap),
           ListID = names(unlist(townnames.overlap))) %>%
  mutate(BirdID = stringr::str_extract(ListID, ".+?(?=_)")) %>%
  dplyr::select(-ListID) %>% 
  mutate(ID = paste(BirdID, Town, sep = "_")) %>%
  mutate(Settled = ifelse(ID %in% mergeid$ID, 1, 0)) %>%
  filter(BirdID %in% mergeid$BirdID) %>%
  filter(!is.na(Town))

#################################################################################################################
### COLLECT TOWN HABITAT DATA ###
#################################
townlist <- unique(c(settled.towns$Town))
town.reduce <- townbound %>% filter(Town %in% townlist)

#NLCD
NLCDrast <- raster("E:/Maine Drive/GIS/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
# NLCDrast <- projectRaster(NLCDrast, crs = 4326)

town_extract <- raster::extract(NLCDrast, town.reduce) #creates a list for each polygon of all the cell values within it

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

towns.covs2 <- merge(town.covs1, roadbytown, by = "Town", all.x = T)

#Lat/Long

town.covs3 <- cbind(towns.covs2, st_coordinates(st_transform(st_centroid(town.reduce), 32619))[,1:2])


settle.input <- merge(settled.towns, town.covs3, by = "Town", all.x = T) %>%
  dplyr::select(-geometry, -Town, -ID) %>%
  arrange(BirdID, desc(Settled))

write.csv(settle.input, "SettDesc_input.csv", row.names = F)


#################################################################################################################
### RUN CONDITIONAL LOGISTIC REGRESSIONS ###
############################################
require(survival)
require(AICcmodavg)
settle.input <- read.csv("SettDesc_input.csv")

cand.models <- list()
cand.models[[1]] <- settmodel.null <- clogit(Settled ~ strata(BirdID), settle.input)
cand.models[[2]] <- settmodel.Dev <- clogit(Settled ~ Developed + strata(BirdID), settle.input)
cand.models[[3]] <- settmodel.Ag <- clogit(Settled ~ Agriculture + strata(BirdID), settle.input)
cand.models[[4]] <- settmodel.Wet <- clogit(Settled ~ Wetland + strata(BirdID), settle.input)
cand.models[[5]] <- settmodel.Grass <- clogit(Settled ~ Grassland + strata(BirdID), settle.input)
cand.models[[6]] <- settmodel.Road <- clogit(Settled ~ Road_KM + strata(BirdID), settle.input)
cand.models[[7]] <- settmodel.X <- clogit(Settled ~ X + strata(BirdID), settle.input)
cand.models[[8]] <- settmodel.Y <- clogit(Settled ~ Y + strata(BirdID), settle.input)
cand.models[[9]] <- settmodel.XY <- clogit(Settled ~ X*Y+strata(BirdID), settle.input)
cand.models[[10]] <- settmodel.Dev2 <- clogit(Settled ~ poly(Developed,2) + strata(BirdID), settle.input)
cand.models[[11]] <- settmodel.Ag2 <- clogit(Settled ~ poly(Agriculture,2) + strata(BirdID), settle.input)
cand.models[[12]] <- settmodel.Wet2 <- clogit(Settled ~ poly(Wetland,2) + strata(BirdID), settle.input)
cand.models[[13]] <- settmodel.Grass2 <- clogit(Settled ~ poly(Grassland,2) + strata(BirdID), settle.input)
cand.models[[14]] <- settmodel.Road2 <- clogit(Settled ~ poly(Road_KM,2) + strata(BirdID), settle.input)
cand.models[[15]] <- settmodel.Full <- clogit(Settled ~  poly(Agriculture,2) + Developed + poly(Wetland,2) + strata(BirdID), settle.input)

aictab(cand.set = cand.models)

summary(settmodel.Full)

#################################################################################################################
### CREATE PLOTS ###
####################
m.cf <- log(summary(settmodel.Full)$conf.int)

#Agriculture
ag.data.plot <- data.frame(Ag = seq(min(settle.input$Agriculture), max(settle.input$Agriculture), .01),
                           Dev = mean(settle.input$Developed),
                           Wet = mean(settle.input$Wetland)) %>%
  mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
  mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
  mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))

SD.ag2.plot <- ggplot(data = ag.data.plot) +
  geom_line(aes(x = Ag, y = Est)) +
  geom_line(aes(x = Ag, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Ag, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion Agriculture", y = "log(RSF)") +
  scale_y_continuous(trans = 'log10')

#Developed
dev.data.plot <- data.frame(Dev = seq(min(settle.input$Developed), max(settle.input$Developed), .01),
                           Ag = mean(settle.input$Agriculture),
                           Wet = mean(settle.input$Wetland)) %>%
  mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
  mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
  mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))

SD.dev.plot <- ggplot(data = dev.data.plot) +
  geom_line(aes(x = Dev, y = Est)) +
  geom_line(aes(x = Dev, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Dev, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion Developed", y = element_blank()) +
  scale_y_continuous(trans = 'log10')

#Wetland
wet.data.plot <- data.frame(Wet = seq(min(settle.input$Wetland), max(settle.input$Wetland), .01),
                           Dev = mean(settle.input$Developed),
                           Ag = mean(settle.input$Agriculture)) %>%
  mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
  mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
  mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))

SD.wet2.plot <- ggplot(data = wet.data.plot) +
  geom_line(aes(x = Wet, y = Est)) +
  geom_line(aes(x = Wet, y = LCL), linetype = "dashed") +
  geom_line(aes(x = Wet, y = UCL), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion Wetland", y = element_blank()) +
  scale_y_continuous(trans = 'log10')

### Group Plots
require(patchwork)

top.model.plots <- SD.ag2.plot + SD.dev.plot + SD.wet2.plot +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)

ggsave(top.model.plots, file = "./Results/SettlingDecision_topmodels.jpg",
       height = 5, width = 12)
