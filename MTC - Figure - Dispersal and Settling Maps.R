lapply(c("dplyr", 'sf', "tidyverse", 'ggplot2'), require, character.only = T)


hexdispprob <- read.csv("HexCov_Disp.csv")
hexsettleprob <- read.csv("HexCov_Settle.csv")
hexcovs.raw <- st_read("./GIS/HexCovs.shp")

hexcovs.1 <- merge(hexcovs.raw, hexdispprob, by = "GridID", all.x = T)
hexgrid <- merge(hexcovs.1, hexsettleprob, by = "GridID", all.x = T) %>%
  filter(!is.na(DispProbF))

wmdbound <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  arrange(WMD) %>%
  st_transform(crs(hexgrid)) %>%
  filter(WMD != 29)

hexgrid.ME <- st_join(hexgrid, wmdbound, st_intersects) %>% filter(!is.na(WMD))

dispF.plot <- ggplot(data = hexgrid.ME) +
  geom_sf(aes(fill = DispProbF), color = NA) +
  geom_sf(data = wmdbound, fill = NA, color = alpha("black", .3), lwd = 1.2) +
  scale_fill_gradientn(colors = c('#f7feae','#b7e6a5','#7ccba2','#46aea0','#089099','#00718b','#045275'), 
                       values = seq(0, 1, length.out = 7),
                       breaks = seq(0, 1, length.out = 5)) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.83, .15),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1,"cm"))

dispM.plot <- ggplot(data = hexgrid.ME) +
  geom_sf(aes(fill = DispProbM), color = NA) +
  geom_sf(data = wmdbound, fill = NA, color = alpha("black", .3), lwd = 1.2) +
  scale_fill_gradientn(colors = c('#f7feae','#b7e6a5','#7ccba2','#46aea0','#089099','#00718b','#045275'), 
                       values = seq(0, 1, length.out = 7),
                       breaks = seq(0, 1, length.out = 5)) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.83, .15),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1,"cm"))

settleF.plot <- ggplot(data = hexgrid.ME) +
  geom_sf(aes(fill = SettleProbF), color = NA) +
  geom_sf(data = wmdbound, fill = NA, color = alpha("black", .3), lwd = 1.2) +
  scale_fill_gradientn(colors = c('#f7feae','#b7e6a5','#7ccba2','#46aea0','#089099','#00718b','#045275'), 
                       values = seq(0, 1, length.out = 7),
                       breaks = seq(0, 1, length.out = 5)) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.83, .15),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1,"cm"))

settleM.plot <- ggplot(data = hexgrid.ME) +
  geom_sf(aes(fill = SettleProbM), color = NA) +
  geom_sf(data = wmdbound, fill = NA, color = alpha("black", .3), lwd = 1.2) +
  scale_fill_gradientn(colors = c('#f7feae','#b7e6a5','#7ccba2','#46aea0','#089099','#00718b','#045275'), 
                       values = seq(0, 1, length.out = 7),
                       breaks = seq(0, 1, length.out = 5)) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.83, .15),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1,"cm"))
  
require(patchwork)

combo.hexmaps <- dispF.plot + dispM.plot + settleF.plot + settleM.plot + 
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 30), plot.tag.position = c(0.05,.9))

ggsave(combo.hexmaps, file = "./Figures/Dispersal and Settle Prob Maps.jpg",
       width = 20, height = 20)
