lapply(c("dplyr", "ggplot2", "pdqr", "CircStats", "sf", "sp", "raster", "units"), require, character.only = T)

### Movement Radius Decisions plot
### load and prep Habitat Suitability surface
hssurface <- raster("./GIS/ExtendedRasters/FullHS_Day.tif")

T0 <- st_sfc(st_point(c(521164, 5073000), dim = "XY"))
st_crs(T0) <- crs(hssurface)
T0cont <- st_sfc(st_point(c(521164, 5074503), dim = "XY"))
st_crs(T0cont) <- crs(hssurface)

T1 <- st_sfc(st_point(c(521164, 5073503), dim = "XY"))
st_crs(T1) <- crs(hssurface)
T1_R <- st_buffer(T1, 1000)

L01 <- st_cast(st_union(T0,T1),"LINESTRING")
L01cont <- st_cast(st_union(T0cont,T1),"LINESTRING")

hssurface.crop <- crop(hssurface, st_bbox(st_buffer(T1_R, 200)))
hssurface_pts <- rasterToPoints(hssurface.crop, spatial = TRUE)
hssurface_df  <- data.frame(hssurface_pts)

T2A <- st_sfc(st_point(c(521800, 5073000), dim = "XY"))
st_crs(T2A) <- crs(hssurface)
T2B <- st_sfc(st_point(c(521000, 5073700), dim = "XY"))
st_crs(T2B) <- crs(hssurface)

L12A <- st_cast(st_union(T2A,T1),"LINESTRING")
L12B <- st_cast(st_union(T2B,T1),"LINESTRING")

examplechoice <- ggplot() +
  geom_raster(data = hssurface_df , aes(x = x, y = y, fill = FullHS_Day)) +
  
  geom_sf(data = T2A, size = 2, color = "deeppink3") +
  geom_sf(data = L12A, lwd = 1, linetype = "solid", color = "deeppink3") +
  annotate('text', st_coordinates(T2A)[1], st_coordinates(T2A)[2]-80,
           label=expression('T'["a"]), size=4, color = "deeppink3") +
  annotate('text', st_coordinates(T1)[1]+300, st_coordinates(T1)[2]-300, 
           label=expression(italic(d)["a"]), color = "deeppink3") +
  annotate('text', st_coordinates(T1)[1]+320, st_coordinates(T1)[2]+110, 
           label=expression(italic(alpha)["a"]), parse = T, color = "deeppink3") +
  geom_curve(aes(x = st_coordinates(T0)[1], y = st_coordinates(T1)[2]+300,
                 xend = st_coordinates(T1)[1] +200, yend = st_coordinates(T1)[2]-150),
             curvature = -.7, arrow = arrow(length = unit(0.1, "inches")), color = "deeppink3") +
  
  geom_sf(data = T2B, size = 2, color = "blue2") +
  geom_sf(data = L12B, lwd = 1, linetype = "solid", color = "blue2") +
  annotate('text', st_coordinates(T2B)[1]-30, st_coordinates(T2B)[2]+50,
           label=expression('T'["b"]), color = "blue2") +
  annotate('text', st_coordinates(T1)[1]-130, st_coordinates(T1)[2]+80, 
           label=expression(italic(d)["b"]), color = "blue2") +
  annotate('text', st_coordinates(T1)[1]-70, st_coordinates(T1)[2]+240, 
           label=expression(italic(alpha)["b"]), parse = T, color = "blue2") +
  geom_curve(aes(x = st_coordinates(T0)[1], y = st_coordinates(T1)[2]+200,
                 xend = st_coordinates(T1)[1] -120, yend = st_coordinates(T1)[2]+150),
             curvature = .5, arrow = arrow(length = unit(0.1, "inches")), color = "blue2") +
  
  geom_sf(data = T1_R, fill = NA, lwd = 2) +
  geom_sf(data = T1, size = 2) +
  geom_sf(data = T0, size = 2) +
  geom_sf(data = L01, lwd = 1, linetype = "dashed") +
  geom_sf(data = L01cont, lwd = 1, linetype = "dashed") +
  annotate('text', st_coordinates(T0)[1], st_coordinates(T0)[2]-70, 
           label=expression('T'[0])) +
  annotate('text', st_coordinates(T1)[1]+50, st_coordinates(T1)[2]+30, 
           label=expression('T'[1])) +
  
  viridis::scale_fill_viridis(labels = c(0, 0.4),
                              breaks = c(min(hssurface_df$FullHS_Day),
                                         max(hssurface_df$FullHS_Day)),
                              guide = guide_colorbar(title.position = "left")) +
  theme_linedraw(base_size = 12) +
  coord_sf(xlim = extent(hssurface_df)[c(1,2)],
           ylim = extent(hssurface_df)[c(3,4)],
           expand = F, label_graticule = "SW") +
  theme(legend.position = "none",
        legend.key.width = unit(.7,"cm"),
        legend.key.height = unit(.5,"cm"),
        axis.title = element_blank()) +
  labs(fill = "Habitat\nSuitability")

### Step Length/Gamma Plot
df <- data.frame(theta = 1:1000)
ggplot(data = df, )


sh <- .075
rt <- .002
as  <- drop_units(st_distance(T1, T2A))
bs <- drop_units(st_distance(T1, T2B))

distplot <- ggplot(data=df,aes(x=theta)) +
  stat_function(fun=dgamma, args=list(shape=sh, rate=rt)) +
  geom_point(aes(x = as, y = dgamma(as, shape = sh, rate = rt)), color = "deeppink3") +
  annotate('text', as, dgamma(as, shape = sh, rate = rt)+.0005, 
           label=expression(italic(d)["a"]), color = "deeppink3") +
  
  geom_point(aes(x = bs, y = dgamma(bs, shape = sh, rate = rt)), color = "blue2") +
  annotate('text', bs, dgamma(bs, shape = sh, rate = rt)+.0005, 
           label=expression(italic(d)["b"]), color = "blue2")+
  theme_classic(base_size = 12) + 
  coord_cartesian(ylim = c(0, 0.01), xlim = c(0,1000)) +
  labs(x = "Distance (meters)") +
  theme(aspect.ratio = 1,
        axis.title.y = element_blank())



### Turning Angle/Wrapped Cauchy Plot
ta <- data.frame(TA = seq(-pi, pi, .01))

as_Spatial(T0)
geosphere::bearing(as_Spatial(T0 %>% st_transform(4326)), as_Spatial(T1 %>% st_transform(4326)))
geosphere::bearing(as_Spatial(T1 %>% st_transform(4326)), as_Spatial(T2A %>% st_transform(4326)))

geosphere::bearing(as_Spatial(T1 %>% st_transform(4326)), as_Spatial(T2B %>% st_transform(4326)))

mu <- 0
rho <- .3
a  <- (pi/180)*geosphere::bearing(as_Spatial(T1 %>% st_transform(4326)), as_Spatial(T2A %>% st_transform(4326)))
b <- (pi/180)*geosphere::bearing(as_Spatial(T1 %>% st_transform(4326)), as_Spatial(T2B %>% st_transform(4326)))

angplot <- ggplot(data=ta,aes(x=TA)) +
  stat_function(fun=dwrpcauchy, args=list(mu=mu, rho=rho))+
  geom_point(aes(x = a, y = dwrpcauchy(a, mu = mu, rho = rho)), color = "deeppink3") +
  annotate('text', a, dwrpcauchy(a, mu = mu, rho = rho)+.014, 
           label=expression(italic(alpha)["a"]), parse = T, color = "deeppink3") +
  
  geom_point(aes(x = b, y = dwrpcauchy(b, mu = mu, rho = rho)), color = "blue2") +
  annotate('text', b-.2, dwrpcauchy(b, mu = mu, rho = rho)+.010, 
           label=expression(italic(alpha)["b"]), parse = T, color = "blue2") +
  theme_classic(base_size = 12) + 
  coord_cartesian(ylim = c(0, 0.3), xlim = c(-pi,pi)) +
  labs(x = "Turning Angle (radians)", y = "Probability") +
  theme(aspect.ratio = 1)


require(patchwork)
layout <- "
AA
AA
BC
"

comboplot <- examplechoice + distplot + angplot + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20), plot.tag.position = c(-0.05,.95))

ggsave(comboplot, filename = "./Figures/IBM Example.jpg",
       width = 10, height = 10)
