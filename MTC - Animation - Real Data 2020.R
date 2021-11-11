lapply(c("dplyr", "ggplot2", "moveVis", "sf", "raster", "move"), require, character.only = T)

### Animation of GPS birds from 2020 from March 1 through May 31 overlayed
### on NLCD data and hexgrid


### MOVEMENT DATA
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")

turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine",
                             login = login, animal = c("1626", "1622", "1619"), 
                             timestamp_start = "20200301000000000",
                             timestamp_end = "20200531000000000")

m <- align_move(turkeygps, res = 1, unit = "hours")

move.sf <- turkeygps@data %>%
  dplyr::select(x = location_long, y = location_lat) %>%
  st_as_sf(coords = c("x", "y"))
st_crs(move.sf) <- crs(turkeygps)

### HEX GRID
hex <- st_read("./GIS/HexCovs.shp") %>%
  st_transform(crs(turkeygps))
hex1 <- hex %>% st_crop(., extent(turkeygps))
hex <- hex[hex1,]

### NLCD RASTER
nlcd <- raster("./GIS/ExtendedRasters/FullLC.tif")
nlcd.clip <- crop(nlcd, hex %>% st_transform(crs(nlcd)))
nlcd.clip <- projectRaster(nlcd.clip, crs = crs(turkeygps))
nlcd.pts <- rasterToPoints(nlcd.clip, spatial = TRUE)
nlcd.df  <- data.frame(nlcd.pts)

ggplot() +
  geom_raster(data = nlcd.df , aes(x = x, y = y, fill = FullLC)) +
  geom_sf(data = hex, fill = NA, lwd = 1, color = alpha("black", alpha = .6)) +
  scale_fill_discrete()
