lapply(c("dplyr", "ggplot2", "sf", "raster"), require, character.only = T)

### Load data and shapefiles
#Load WMD connectivity simulations
simconnect.raw1 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv")
simconnect.raw2 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part2.csv")
simconnect.raw3 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part3.csv")
simconnect.raw4 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part4.csv")

simconnect.raw <- do.call("rbind", list(simconnect.raw1,simconnect.raw2,simconnect.raw3,simconnect.raw4))

# Remove accidental repeat simulations, keeping 1st version
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


wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)

### Find Start and End WMD for each bird
sim.startloc <- simconnect.clean %>% 
  group_by(BirdID) %>%
  filter(row_number() == min(row_number())) %>%
  arrange(BirdID) %>%
  ungroup() %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly))

sim.endloc <- simconnect.clean %>%
  dplyr::select(BirdID, Step, x, y) %>%
  group_by(BirdID) %>%
  filter(Step == max(Step))%>%
  filter(row_number() == max(row_number())) %>%
  arrange(BirdID) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly)) %>%
  st_join(., wmdpoly, join = st_intersects) %>%
  rename(EndWMD = WMD) %>%
  mutate(EndWMD = ifelse(is.na(EndWMD), 99, EndWMD))


#Load wmd boundary shapefile
wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)

startdens <- ggplot(data = wmdpoly) +
  # geom_sf(data = sim.startloc) +
  stat_density_2d(data = sim.startloc, 
                  mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 1) +
  geom_sf(fill = NA) +
  theme_linedraw(base_size = 25) +
  viridis::scale_fill_viridis()

enddens <- ggplot(data = wmdpoly) +
    # geom_sf(data = sim.endloc) +
  stat_density_2d(data = sim.endloc, 
                  mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 1) +
  geom_sf(fill = NA) +
  theme_linedraw(base_size = 25) +
  viridis::scale_fill_viridis()
  # theme(legend.position = c(.75, .13),
  #       legend.direction = "vertical",
  #       legend.key.width = unit(2, "cm"))

require(patchwork)

comboplot <- startdens + enddens
ggsave(comboplot, filename = "./Figures/Start versus End KDE.jpg",
       width = 20, height = 15)

startdens <- ggplot(data = wmdpoly) +
  geom_sf(data = sim.startloc, alpha = .3) +
  geom_sf(fill = NA, alpha = .5) +
  theme_linedraw(base_size = 25) +
  viridis::scale_fill_viridis()

enddens <- ggplot(data = wmdpoly) +
  geom_sf(data = sim.endloc, alpha = .3) +
  geom_sf(fill = NA, alpha = .5) +
  theme_linedraw(base_size = 25) +
  viridis::scale_fill_viridis()
# theme(legend.position = c(.75, .13),
#       legend.direction = "vertical",
#       legend.key.width = unit(2, "cm"))

require(patchwork)

comboplot <- startdens + enddens
ggsave(comboplot, filename = "./Figures/Start versus End Points.jpg",
       width = 20, height = 15)