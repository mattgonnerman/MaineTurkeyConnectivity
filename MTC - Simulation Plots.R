##########################################################################################################
####################
### CREATE PLOTS ###
####################
lapply(c("dplyr", "ggplot2", "sf", "raster"), require, character.only = TRUE)
HS_day <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/HS_Day.tif")
names(HS_day) <- "layer"

sim.plot.loop <- function(BirdID, SetID){
  sim.output <- read.csv(paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird",
                               BirdID, "Set", SetID,".csv", sep = "_"))
  
  ### Output line shapefile
  sim.lines <- sim.output %>% 
    mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
    st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(32619) %>% 
    group_by(LineID) %>% 
    arrange(Step) %>%
    summarize(m = mean(HS, na.omit = T), do_union = F) %>%
    st_cast("LINESTRING")
  
  ### Plot on raster
  #Change Habitat suitability raster to dataframe for use in GGplot
  
  
  #create start and end point sf objects
  start <- sim.output[sim.output$Step == 0, c("x","y")] %>%
    distinct() %>%
    st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(32619)
  end <- sim.output[, c("EndX","EndY")]%>%
    distinct() %>%
    st_as_sf(coords = c("EndX","EndY")) %>% 
    sf::st_set_crs(32619)
  # Adjust extent to include all elements and crop raster
  rastext <-merge(extent(sim.lines), extent(end))
  rastext <- extend(rastext,2000)
  HS_df <- as.data.frame(rasterToPoints(crop(HS_day, rastext)))
  
  ### Subset to the best X simulations
  best.sims <- sim.output %>% 
    group_by(Sim.ID) %>%
    summarize(MinDist = min(D2End, na.rm = T)) %>%
    ungroup() %>%
    arrange(MinDist) %>%
    filter(MinDist < (1 + quantile(MinDist, .1)))
  
  best.lines <- sim.output %>%
    filter(Sim.ID %in% best.sims$Sim.ID) %>%
    mutate(LineID = paste(OG.ID, Sim.ID, sep = "_")) %>%
    st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(32619) %>% 
    group_by(LineID) %>% 
    arrange(Step) %>%
    summarize(m = mean(HS, na.omit = T), do_union = F) %>%
    st_cast("LINESTRING")
  
  sim.plot <- ggplot(data = best.lines) +
    geom_raster(data = HS_df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data = sim.lines, color = "yellow", show.legend = F) +
    geom_sf(aes(color = LineID), show.legend = F, lwd = 1.1) +
    geom_sf(data = start, color = "blue", size = 4, shape = 20) +
    geom_sf(data = end, color = "red", size = 4, shape = 20) +
    theme_classic() +
    scale_fill_continuous(type = "viridis")
  
  ggsave(sim.plot, file = paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/SimPlots/CalSims_OGBird",
                                BirdID, "Set", SetID,".jpg", sep = "_"), 
         width = 10, height = 10)
}


sapply(1:72, FUN = sim.plot.loop, SetID = 2)
