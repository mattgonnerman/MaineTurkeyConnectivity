### Map Showing Town to Town Movement
lapply(c("sf", "dplyr", "ggplot2"), require, character.only = TRUE)

#Load Maine Towns Object
mainetowns <-  st_read("./GIS/TownCovs.shp") %>%
  dplyr::select(Town) %>%
  group_by(Town) %>%
  summarize(Ref = n())

#Make point object with town centers
towncenters <- st_centroid(mainetowns)

#Load Harvest Return Info
harvestreports <- read.csv("Harvests - Harvested Birds.csv") %>%
  dplyr::select(BirdID = Alum.Band.ID, BirdID2 = Rivet.ID, Origin = Town.of.Capture, Destination = Town.of.Harvest) %>%
  mutate(BirdID = ifelse(is.na(BirdID), BirdID2, BirdID))

#Which birds did not leave town boundary
remained <- harvestreports %>% filter(Origin == Destination)
#Which birds dispersed
dispersed <- harvestreports %>% filter(Origin != Destination)

#Create origins as sf points
dispersed.origin <- dispersed %>% dplyr::select(BirdID, Town = Origin)
dispersed.origin <- merge(dispersed.origin, towncenters, by = "Town", all.x = T) %>%
  arrange(BirdID) %>%
  st_as_sf()
#Create destinations as sf points
dispersed.dest <- dispersed %>% dplyr::select(BirdID, Town = Destination)
dispersed.dest <- merge(dispersed.dest, towncenters, by = "Town", all.x = T) %>%
  arrange(BirdID) %>%
  st_as_sf()
#Combine points into line object
dispersal.lines <- rbind(dispersed.origin, dispersed.dest)%>% 
  group_by(BirdID) %>% 
  summarize(Total = c()) %>% 
  st_cast("MULTIPOINT")%>% 
  st_cast("LINESTRING")

#Summarize Number of birds remaining in origin
totremain <- remained %>% group_by(Origin) %>% summarize(TotalRemain = n()) %>% rename(Town = Origin)
remain.points <- merge(totremain, towncenters, by = "Town", all.x = T) %>%
  arrange(TotalRemain) %>%
  st_as_sf()
totdisp <- dispersed %>% group_by(Origin) %>% summarize(TotalDisperse = n()) %>% rename(Town = Origin)
disperse.points <- merge(totdisp, towncenters, by = "Town", all.x = T) %>%
  arrange(TotalDisperse) %>%
  st_as_sf()

### WMD Polygon
wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)

### Create Map
obsmap <- ggplot() +
  geom_sf(data = wmdpoly, fill = NA, size = 1) +
  geom_sf(data = dispersal.lines, size = 2) +
  geom_sf(data = dispersed.origin, size = 2, color = "blue") +
  geom_sf(data = dispersed.dest, size = 1.2, color = "red") +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(obsmap, file = "./Figures/NestHarvestMap.jpeg",
       width = 8, height = 10)
