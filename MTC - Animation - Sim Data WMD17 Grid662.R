lapply(c("dplyr", "ggplot2", "sf", "raster", "tidyverse", "move", "moveVis"), require, character.only = TRUE)

### Load Hex Grid Cells
hex <- st_read("./GIS/HexCovs.shp") %>%
  st_transform(32619)

hex662 <- hex[hex$GridID == "Grid_662",]

hexmap <- hex[hex$GridID %in% c("Grid_662", "Grid_610", "Grid_635", "Grid_636", "Grid_687", "Grid_688", "Grid_714"),] %>%
  st_transform(4326)

### Load Simulation Points
simtraits <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.csv") %>%
  dplyr::select(StartWMD, Sex, BirdID) %>%
  filter(StartWMD == 17 )

simconnect.raw1 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv")
simconnect.raw2 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part2.csv")
simconnect.raw3 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part3.csv")
simconnect.raw4 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part4.csv")

simconnect.raw <- do.call("rbind", list(simconnect.raw1,simconnect.raw2,simconnect.raw3,simconnect.raw4)) 


hex662overlap <- simconnect.raw %>% filter(BirdID %in% unique(simtraits$BirdID)) %>%
  st_as_sf(coords = c("x", "y"), crs = 32619) %>%
  st_intersects(., hex662, sparse = F) %>%
  which(.==T)
  
simconnect.WMD17 <- simconnect.raw %>% filter(BirdID %in% unique(simtraits$BirdID))
simconnect.Grid662overlap <- simconnect.WMD17[hex662overlap,]

sim.seasonal <- simconnect.raw %>% filter(BirdID %in% unique(simconnect.Grid662overlap$BirdID)) %>%
  distinct() %>%
  group_by(BirdID) %>%
  summarize(Total = n()) %>%
  filter(Total > 3)

sim.662.df <- simconnect.raw %>% filter(BirdID %in% unique(simconnect.Grid662overlap$BirdID)) %>%
  dplyr::select(StartWMD, x, y, Step, BirdID, ID) %>%
  filter(BirdID %in% sim.seasonal$BirdID) %>%
  mutate(ts = as.POSIXct("2020-01-01 01:00:00") + (Step*60)) %>%
  distinct() %>%
  group_by(BirdID) %>%
  slice(1:(n()-1)) %>%
  ungroup()

sim.662.move <- df2move(df = sim.662.df, proj = projection(hex662), x = "x", y = "y", time = "ts", track_id = "BirdID")

### load and prep Habitat Suitability surface
hssurface <- raster("./GIS/ExtendedRasters/FullHS_Day.tif")
hssurface.crop <- crop(hssurface, extent(st_buffer(hex662, 2000)))
hssurface.crop <- projectRaster(hssurface.crop, crs = crs(sim.662.move))



### Animate Tracks
sim.662.align <- align_move(sim.662.move, res = 60, digit = 0, unit = "secs")

rtimeslist <- sort(unique(as.POSIXct(sim.662.align$time)))
rlist <- list()
rlist[1:length(rtimeslist)] <- hssurface.crop

sim.662.frames <- frames_spatial(sim.662.align, ext = extent(st_buffer(hex662, 2000)),
                                 tail_length = 2, path_legend = F,
                                 # map_service = "mapbox", map_type = "satellite",
                                 # map_token = "pk.eyJ1IjoibWF0dGdvbm5lcm1hbiIsImEiOiJjazBlMHg1Zm0wZGJzM2xxZGo3dzNoajVoIn0.532tvnjRUu5Orb2MFkQn9w") %>%
                                 r_list = rlist,
                                 r_times = rtimeslist) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_gg(gg = expr(geom_sf(data = hexmap %>% st_transform(crs(sim.662.align)),
                             fill = NA, color = "black", lwd = 1.2)))%>% 
  add_gg(gg = expr(coord_sf(xlim = extent(st_buffer(hex662, 2000) %>% st_transform(crs(sim.662.align)))[1:2],
                                                           ylim = extent(st_buffer(hex662, 2000) %>% st_transform(crs(sim.662.align)))[3:4])))

animate_frames(sim.662.frames, out_file = "./Figures/AnimationTest.gif",
               fps = 20, overwrite = T, display = T)



