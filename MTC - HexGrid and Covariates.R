lapply(c("dplyr", "sf"), require, character.only = T)

townbound <- st_read("./GIS/TownCovs.shp") %>%
  st_transform(32619)

hexgrid.blank <- st_make_grid