lapply(c("dplyr", "landscapemetrics", "raster", "sf"), require, character.only = T)

#Prepare Town Boundary Polygon Object
townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  group_by(TOWN) %>%
  filter(TArea_KM2 == max(TArea_KM2))%>%
  filter(ISLAND == "n") %>%
  filter(LAND == "y") %>%
  dplyr::select(Town = TOWN) %>%
  st_transform(crs(forestbin))


#NLCD
NLCDrast <- raster("E:/GitHub/NestHabitatQuality/GIS/NLCD_clipped.tif")

town_extract <- extract(NLCDrast, townbound) #creates a list for each polygon of all the cell values within it

town_proportions <- lapply(town_extract, FUN= function(x){prop.table(table(x))}) #this returns proportion of each as a list
names(town_proportions) <- townbound$Town
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


town.covs1 <- cbind(townbound, town_percentcover) %>%
  st_transform(crs(NLCDrast))

#Roads
Roadslines <- st_combine(st_read("E:/Maine Drive/GIS/Roads/medotpubrds.shp")) %>% 
  st_transform(crs(townbound))
roadsclipped <- st_intersection(Roadslines, townbound)
roadbytown <- st_join(st_as_sf(roadsclipped), townbound, st_intersects) %>%
  st_drop_geometry() %>%
  arrange(Town)
roadlength.km <- st_length(roadsclipped)/1000

poly = st_as_sf(townbound)
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

towns.covs2 <- merge(town.covs1, roadbytown, by = "Town", all.x = T) %>% 
  rename(StartTown = Town)

#Lat/Long

town.covs3 <- cbind(towns.covs2, st_coordinates(st_transform(st_centroid(towns.covs2), 32619))[,1:2])

#Landscape Metrics
forestbin <- raster("./GIS/forestbinary/forestbinary.tif")

town.lsm <- sample_lsm(forestbin, townbound,
                       level = "landscape", what = c("lsm_l_ai", "lsm_l_ed", "lsm_l_contag"))
town.lsm <- as.data.frame(town.lsm) %>%
  dplyr::select(metric, value, plot_id) %>%
  tidyr::pivot_wider(id_cols = plot_id, names_from = metric, values_from = value) %>%
  dplyr::select(-plot_id, Ag_Index = ai, Connectance = contag, Edge_Density = ed)

town.lsm <- cbind(townbound, town.lsm)
town.covs4 <- merge(town.covs3 %>% rename(Town = StartTown),
                    town.lsm %>% st_drop_geometry(), by = "Town", all.x = T)

st_write(town.covs4, "./GIS/TownCovs.shp")
