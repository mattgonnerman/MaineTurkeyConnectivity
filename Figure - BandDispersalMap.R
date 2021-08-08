### Map Showing Town to Town Movement
lapply(c("sf", "dplyr", "ggplot2"), require, character.only = TRUE)

#Load Maine Towns Object
mainetowns <-  st_read("E:/Maine Drive/GIS/Maine_Boundaries_Town_and_Townships_Polygon-shp/Maine_Boundaries_Town_and_Townships_Polygon.shp") %>%
  dplyr::select(Town = TOWN) %>%
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

### Create Map
ggplot() +
  geom_sf(data = mainetowns, fill = NA, size = .4) +
  geom_sf(data = dispersal.lines, size = 1) +
  geom_sf(data = disperse.points, size = 2, color = "blue") +
  # geom_sf(data = remain.points, size = 1, color = "red") +
  theme_classic()
