lapply(c("dplyr", "ggplot2", "sf", "raster", "tidyverse"), require, character.only = TRUE)


### WMD Polygon
wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)

### Load Simulation Points
simtraits <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.csv") %>%
  dplyr::select(StartWMD, Sex, BirdID)

simconnect.raw1 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv")
simconnect.raw2 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part2.csv")
simconnect.raw3 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part3.csv")
simconnect.raw4 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part4.csv")

simconnect.raw <- do.call("rbind", list(simconnect.raw1,simconnect.raw2,simconnect.raw3,simconnect.raw4))

### Remove accidental repeat simulations, keeping 1st version
simconnect.dups.list <- simconnect.raw %>%
  group_by(BirdID) %>%
  group_split()

dupfunction <-function(df){
  if(df$Step[2] == 0){
    # If second step value is 0, then no dispersal
    return(df[1:2,])
    break
    
  }else{
    
    if(min(diff(df$Step)) == 1){
      
      return(df)
      break
      
    }else{
      
      return(df[1:which(diff(df$Step) < 0)[1],])
      break
      
    }
  }
}

simconnect.list <- lapply(X = simconnect.dups.list, dupfunction)
simconnect.clean <- data.table::rbindlist(simconnect.list)

### Create Lines Objects from Simulation Points
# simconnect.lines <- simconnect.clean %>% 
#   st_as_sf(coords = c("x","y")) %>% 
#   sf::st_set_crs(32619) %>% 
#   group_by(BirdID) %>% 
#   summarize(m = mean(StartWMD, na.omit = T), do_union = F) %>%
#   st_cast("LINESTRING")
# 
# st_write(simconnect.lines, "./GIS/WMDConnectLines.shp", overwrite = T)
simconnect.lines <- st_read("./GIS/WMDConnectLines.shp")
#Merge with Bird Characteristics
simtraits.lines <- merge(simconnect.lines, simtraits, by = "BirdID")

### Find Start and End WMD for each bird
sim.startloc <- simconnect.clean %>% 
  dplyr::select(BirdID, Step, StartWMD)  %>%
  group_by(BirdID) %>%
  filter(row_number() == min(row_number())) %>%
  arrange(BirdID)

sim.endloc <- simconnect.clean %>%
  dplyr::select(BirdID, Step, x, y) %>%
  group_by(BirdID) %>%
  filter(Step == max(Step))%>%
  filter(row_number() == max(row_number())) %>%
  arrange(BirdID) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly)) %>%
  st_join(., wmdpoly, join = st_intersects) %>%
  st_drop_geometry() %>%
  rename(EndWMD = WMD) %>%
  mutate(EndWMD = ifelse(is.na(EndWMD), 99, EndWMD))

### Remove start and end locations
movepointsonly <- simconnect.clean %>%
  filter(Step != 0) %>%
  group_by(BirdID) %>%
  filter(Step != max(Step)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  sf::st_set_crs(32619)
  
movelinesonly <- movepointsonly %>%
  st_as_sf(coords = c("x","y")) %>%
  sf::st_set_crs(32619) %>%
  group_by(BirdID) %>%
  summarize(m = mean(StartWMD, na.omit = T), do_union = F) %>%
  st_cast("LINESTRING")

st_write(movelinesonly, "./GIS/WMDConnectLines_SeasonMove.shp", overwrite = T)
movelinesonly <- st_read("./GIS/WMDConnectLines_SeasonMove.shp")

### Designate Plot Location and filter points/lines to within polygon to reduce processing time
plotcenter <- st_as_sf(data.frame(y = 44.811083, x = -68.774231), coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs(movepointsonly))
plotbuffer <- st_buffer(plotcenter, 50000, endCapStyle = "SQUARE") %>% mutate(In = 1)

moveinbuffer <- simconnect.lines %>%
  mutate(NPoints = mapview::npts(simconnect.lines, by_feature = T)) %>%
  filter(NPoints > 2) %>%
  st_join(., plotbuffer, st_intersects) %>%
  filter(In == 1)

movepoints.reduced <- st_join(movepointsonly %>% dplyr::select(BirdID), st_buffer(plotbuffer, 3000), st_intersects) %>%
   filter(In == 1)

### Create kernel density surface from points
# require(raster)
# require(adehabitatHR)

movepoints.sp <- as(movepoints.reduced %>% dplyr::select(-BirdID, -In), "Spatial")
move.kde <- kernelUD(movepoints.sp, h="href", grid = 1000, extent = 0)
move.raster <- raster(move.kde)
writeRaster(move.raster, "./GIS/SimMoveDensity.tif")
move.raster <- raster("./GIS/SimMoveDensity.tif")
move.raster <- projectRaster(move.raster, crs = 4326)
move.raster_pts <- rasterToPoints(move.raster, spatial = TRUE)
move.raster_df  <- data.frame(move.raster_pts)

### load and prep Habitat Suitability surface
hssurface <- raster("./GIS/ExtendedRasters/FullHS_Day.tif")
hssurface.crop <- crop(hssurface, extent(st_buffer(plotbuffer, 2000)))
hssurface.crop <- projectRaster(hssurface.crop, crs = 4326)
hssurface_pts <- rasterToPoints(hssurface.crop, spatial = TRUE)
hssurface_df  <- data.frame(hssurface_pts)
rm("hssurface_pts", 'hssurface.crop', 'hssurface')

### Loat basemap for points/lines plot
require(ggmap)
lineMap <- get_stamenmap(bbox = c(left = extent(plotbuffer %>% st_transform(4326))[1],
                                   bottom = extent(plotbuffer %>% st_transform(4326))[3],
                                   right = extent(plotbuffer %>% st_transform(4326))[2],
                                   top = extent(plotbuffer %>% st_transform(4326))[4]),
                          maptype = "terrain",
                          crop = F,
                          zoom = 11,
                          color = "color")

### Add lat and lon columns for ggmap
movepoints.reduced$lon <- st_coordinates(movepoints.reduced %>% st_transform(4326))[,1]
movepoints.reduced$lat <- st_coordinates(movepoints.reduced %>% st_transform(4326))[,2]



####################
### CREATE PLOTS ###
####################

### Habitat Suitability Plot
hs.plot <- ggplot() +
  geom_raster(data = hssurface_df , aes(x = x, y = y, fill = FullHS_Day)) +
  theme_linedraw(base_size = 25) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(.7,"cm"),
        legend.key.height = unit(.5,"cm")) +
  viridis::scale_fill_viridis(labels = c("Low", "High"),
                              breaks = c(min(hssurface_df$FullHS_Day),
                                         max(hssurface_df$FullHS_Day))) +
  coord_sf(xlim = extent(plotbuffer %>% st_transform(4326))[c(1,2)],
           ylim = extent(plotbuffer %>% st_transform(4326))[c(3,4)],
           expand = F, label_graticule = "SE") +
  theme(axis.title = element_blank(),
        aspect.ratio = 1)

### Plot of Simulated Locations
location.plot <- ggmap(lineMap) +
  geom_sf(data = movepoints.reduced %>% st_transform(4326),
          shape = 16, fill = alpha("red", .01), color = alpha("red", .01)) +
  coord_sf(xlim = extent(plotbuffer %>% st_transform(4326))[c(1,2)],
           ylim = extent(plotbuffer %>% st_transform(4326))[c(3,4)],
           expand = F, label_graticule = "SE") +
  theme_linedraw(base_size = 25) +
  theme(axis.title = element_blank(),
        aspect.ratio = 1)

### Location Kernel Density Plot
kde.plot <- ggplot() +
  geom_raster(data = move.raster_df , aes(x = x, y = y, fill = SimMoveDensity)) +
  theme_linedraw(base_size = 25) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(.7,"cm"),
        legend.key.height = unit(.5,"cm")) +
  coord_sf(xlim = extent(plotbuffer %>% st_transform(4326))[c(1,2)],
           ylim = extent(plotbuffer %>% st_transform(4326))[c(3,4)],
           expand = F, label_graticule = "SE") +
  viridis::scale_fill_viridis(labels = c("Low", "High"),
                              breaks = c(min(move.raster_df$SimMoveDensity),
                                         max(move.raster_df$SimMoveDensity))) +
  theme(axis.title = element_blank(),
        aspect.ratio = 1)


require(patchwork)

comboplot <- hs.plot + location.plot + kde.plot + 
  plot_layout(ncol = 3) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 30), plot.tag.position = c(-0.03,.95))


ggsave(comboplot, file = "./Figures/HS and KDE ZoomMap.jpg",
       width = 35, height = 15)
