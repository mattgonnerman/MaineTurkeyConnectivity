##########################################################################################################
####################
### CREATE PLOTS ###
####################
lapply(c("dplyr", "ggplot2", "sf", "raster"), require, character.only = TRUE)
HS_day <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Day.tif")
names(HS_day) <- "layer"
sim.output <- read.csv(paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Narrow Prior Calibration Runs/CalSims_OGBird",
                             9, "Set",2,".csv", sep = "_"))

#create start and end point sf objects
start <- sim.output[sim.output$Step == 0, c("x","y")] %>%
  distinct() %>%
  st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(32619)
end <- sim.output[, c("EndX","EndY")]%>%
  distinct() %>%
  st_as_sf(coords = c("EndX","EndY")) %>% 
  sf::st_set_crs(32619)

# Identify start and end Hex
hexgrid <- st_read("./GIS/HexCovs.shp") %>% st_transform(32619)
hexstart <- hexgrid[start[1,],]
hexend <- hexgrid[end,]



### SUGGESTED MAP ELEMENTS
### Basemap = ggmap terrain or HS surface
### All movement tracks same color
### Movement tracks that overlap with end grid cell different color
### Start and end location points
### Hexgrid to show start and end locations with decreased alpha


### Output line shapefile
sim.lines <- sim.output %>% 
  mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
  st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(32619) %>% 
  group_by(LineID) %>% 
  arrange(Step) %>%
  summarize(m = mean(HS, na.omit = T), do_union = F) %>%
  st_cast("LINESTRING")

best.lines <- sim.lines[hexend,]
### Plot on raster
#Change Habitat suitability raster to dataframe for use in GGplot



# Adjust extent to include all elements and crop raster
rastext <-merge(extent(sim.lines), extent(end))
rastext <- extend(rastext,2000)
HS_df <- as.data.frame(rasterToPoints(crop(HS_day, rastext)))

sim.plot <- ggplot(data = best.lines) +
  geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = sim.lines %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = alpha("yellow", .8), show.legend = F) +
  geom_sf(data = best.lines %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "#c4200e", show.legend = F, lwd = .8) +
  geom_sf(data = start[1,] %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "black", size = 6, shape = 20) +
  geom_sf(data = end %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "black", size = 6, shape = 20) +
  geom_sf(data = hexgrid %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = alpha("grey60", 0.35)) +
  geom_sf(data = hexstart %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = "black") +
  geom_sf(data = hexend %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = "black") +
  geom_sf_label(data = end, aes(label = "Destination", geometry = geometry), inherit.aes = FALSE,
               color = "black", nudge_x = 5300, nudge_y = -8700) +
  geom_sf_label(data = start[1,], aes(label = "Origin", geometry = geometry), inherit.aes = FALSE,
               color = "black", nudge_x = -3400, nudge_y = 10800) +
  # scale_color_discrete(type = heat.colors(n = nrow(best.lines))) +
  scale_fill_continuous(type = "viridis") +
  coord_sf(xlim = rastext[1:2], 
           ylim = rastext[3:4], 
           expand = F) +
  theme_linedraw(base_size = 20) +
  theme(axis.title = element_blank(),
        legend.position = "none")

ggsave(sim.plot, file = "./Figures/Example Rejection Sampling - HS.jpg", 
       width = 10, height = 10)

# sim.plot <- ggplot(data = best.lines) +
#   geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
#   geom_sf(data = sim.lines, color = "yellow", show.legend = F) +
#   geom_sf(color = "orange", show.legend = F, lwd = 1.3) +
#   geom_sf(data = start[1,], color = "blue", size = 4, shape = 20) +
#   geom_sf(data = end, color = "red", size = 4, shape = 20) +
#   geom_sf(data = hexgrid, fill = NA, color = alpha("black", 0.25)) +
#   geom_sf(data = hexstart, fill = NA, color = "blue") +
#   geom_sf(data = hexend, fill = NA, color = "red") +
#   theme_classic(base_size = 20) +
#   scale_fill_continuous(type = "viridis") +
#   coord_sf(xlim = extent(rastext)[1:2],
#            ylim = extent(rastext)[3:4],
#            expand = F, label_graticule = "WS",
#            ndiscr = 30) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")

require(ggmap)
rastext <-merge(extent(sim.lines %>% st_transform(4326)), extent(end %>% st_transform(4326)))
rastext <- extend(rastext, 0.01)
simMap <- get_stamenmap(bbox = c(left = rastext[1],
                                   bottom = rastext[3],
                                   right = rastext[2],
                                   top = rastext[4]),
                          maptype = "terrain",
                          crop = F,
                          zoom = 11,
                          color = "color")


sim.map <- ggmap(simMap) +
  geom_sf(data = sim.lines %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = alpha("yellow", .8), show.legend = F) +
  geom_sf(data = best.lines %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "#c4200e", show.legend = F, lwd = .8) +
  geom_sf(data = start[1,] %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "black", size = 4, shape = 20) +
  geom_sf(data = end %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          color = "black", size = 4, shape = 20) +
  geom_sf(data = hexgrid %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = alpha("grey60", 0.35)) +
  geom_sf(data = hexstart %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = "black") +
  geom_sf(data = hexend %>% st_transform(4326), aes(geometry = geometry), inherit.aes = FALSE,
          fill = NA, color = "black") +
  geom_sf_text(data = end, aes(label = "Destination", geometry = geometry), inherit.aes = FALSE,
               color = "black", nudge_x = .07, nudge_y = -.08) +
  geom_sf_text(data = start[1,], aes(label = "Origin", geometry = geometry), inherit.aes = FALSE,
               color = "black", nudge_x = -.037, nudge_y = .1) +
# scale_color_discrete(type = heat.colors(n = nrow(best.lines))) +
  theme_linedraw(base_size = 20) +
  theme(axis.title = element_blank(),
        legend.position = "none")

ggsave(sim.map, file = "./Figures/Example Rejection Sampling.jpg", 
       width = 10, height = 10)

# require(ggplot2)
# ggplot(data = sim.lines) +
#   geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
#   geom_sf(aes(color = LineID), show.legend = F) +
#   geom_sf(color = "yellow") +
#   geom_sf(data = start, color = "blue", size = 4, shape = 20) +
#   geom_sf(data = end, color = "red", size = 4, shape = 20) +
#   theme_classic() +
#   scale_fill_continuous(type = "viridis")

# ### Subset to the best X simulations
# best.sims <- sim.output %>% 
#   group_by(Sim.ID) %>%
#   summarize(MinDist = min(D2End, na.rm = T)) %>%
#   arrange(MinDist) %>%
#   slice(1)
# 
# best.lines <- sim.output %>%
#   filter(Sim.ID %in% best.sims$Sim.ID) %>%
#   mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
#   st_as_sf(coords = c("x","y")) %>% 
#   sf::st_set_crs(32619) %>% 
#   group_by(LineID) %>% 
#   arrange(Step) %>%
#   summarize(m = mean(HS, na.omit = T), do_union = F) %>%
#   st_cast("LINESTRING")

# #create start and end point sf objects
# start <- sim.output[sim.output$Step == 0, c("x","y")] %>%
#   distinct() %>%
#   st_as_sf(coords = c("x","y")) %>% 
#   sf::st_set_crs(32619)
# end <- sim.output[, c("EndX","EndY")]%>%
#   distinct() %>%
#   st_as_sf(coords = c("EndX","EndY")) %>% 
#   sf::st_set_crs(32619)
# # Adjust extent to include all elements and crop raster
# rastext <-merge(extent(best.lines), extent(end))
# rastext <- extend(rastext,2000)
# HS_df <- as.data.frame(rasterToPoints(crop(HS_day, rastext)))
# ggplot(data = best.lines) +
#   geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
#   geom_sf(aes(color = LineID), show.legend = F, lwd = 1.3) +
#   # geom_sf(color = "yellow") +
#   geom_sf(data = start, color = "blue", size = 4, shape = 20) +
#   geom_sf(data = end, color = "red", size = 4, shape = 20) +
#   theme_classic() +
#   scale_fill_continuous(type = "viridis")



