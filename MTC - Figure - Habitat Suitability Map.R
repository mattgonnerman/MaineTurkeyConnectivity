lapply(c("dplyr", 'sf', 'raster', "tidyverse", 'ggplot2'), require, character.only = T)



hssurface <- raster("./GIS/ExtendedRasters/FullHS_Day.tif")


wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  arrange(WMD) %>%
  st_transform(crs(hssurface)) %>%
  filter(WMD != 29)

hssurface.crop <- crop(hssurface, extent(st_buffer(wmdbound, 3000)))
hssurface.crop <- aggregate(hssurface.crop, fact = 4)
# convert raster to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
hssurface_pts <- rasterToPoints(hssurface.crop, spatial = TRUE)
# Then to a 'conventional' dataframe
hssurface_df  <- data.frame(hssurface_pts)
rm("hssurface_pts", 'hssurface.crop', 'hssurface')



hs.plot <- ggplot() +
  geom_raster(data = hssurface_df , aes(x = x, y = y, fill = FullHS_Day)) +
  geom_sf(data = wmdbound, fill = NA, color = alpha("black", .3), lwd = 1.2) +
  theme_void(base_size = 15) +
  theme(legend.position = c(.83, .15),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(.7,"cm"),
        legend.key.height = unit(.5,"cm")) +
  scale_fill_gradientn(colors = c('#f7feae','#b7e6a5','#7ccba2','#46aea0','#089099','#00718b','#045275'), 
                       values = seq(0, 1, length.out = 7),
                       breaks = seq(0, 1, length.out = 5))

ggsave(hs.plot, file = "./Figures/Habitat Suitability Map.jpg",
       width = 10, height = 10)
