lapply(c("dplyr", 'sf', 'raster', "tidyverse"), require, character.only = T)

#Load start and end points for turkeys that did seasonal movements outside of their original WMD
sim.combo <- read.csv("WMDSim_StartandEnd.csv")

#Load wmd boundary shapefile
wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)


### Percent Total Loss of Individuals Map
set1 <- sim.combo %>%
  count(StartWMD, EndWMD) %>%
  filter(StartWMD != EndWMD)

netout <- set1 %>% group_by(StartWMD) %>% summarize(NetOut = sum(n)) %>% rename(WMD = StartWMD)
netin <- set1 %>% group_by(EndWMD) %>% summarize(NetIn = sum(n)) %>% rename(WMD = EndWMD)
netdispl.df <- merge(netout, netin, by = "WMD", all = T) %>%
  mutate(NetOut = ifelse(is.na(NetOut), 0, NetOut),
         NetIn = ifelse(is.na(NetIn), 0, NetIn)) %>%
  mutate(NetDispl = NetIn - NetOut)
wmdabund <- read.csv("WMDTurkeyAbundance.csv") %>% mutate(TotalTurk = TotalTurk*2) %>%
  dplyr::select(WMD, TotalTurk)  
netdispl.sf <- merge(W_wmdpoly, netdispl.df, by = "WMD")
netdispl.sf <- merge(netdispl.sf, wmdabund, by = "WMD", all.x = T) %>%
  mutate(PercentDisp = NetDispl/TotalTurk)

breaks <- round(c(min(netdispl.sf$PercentDisp), 0, 
                  max(netdispl.sf[netdispl.sf$PercentDisp < 0.85,]$PercentDisp),
                  max(netdispl.sf$PercentDisp)), 2)
colval <- (breaks - min(breaks))/(max(breaks) - min(breaks))
percent.plot <- ggplot(data = netdispl.sf) +
  geom_sf(aes(fill = PercentDisp), color = "black", lwd = 2) +
  viridis::scale_fill_viridis(breaks = breaks,
                              values = colval) +
  theme_void(base_size = 35) +
  labs(fill = "Percent\nGain") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1.7,"cm"),
        legend.key.height = unit(2,"cm"))


### Exchange Between WMDs 
set1 <- sim.combo %>%
  count(StartWMD, EndWMD) %>%
  filter(StartWMD != EndWMD) %>%
  rename(WMD1 =StartWMD, WMD2 = EndWMD) %>%
  rowwise() %>%
  mutate(TransferID = paste0(sort(c(WMD1, WMD2)), collapse = "_")) %>%
  ungroup() %>%
  group_by(TransferID) %>%
  summarize(Total = sum(n)) %>%
  mutate(WMD1 = stringr::str_extract(TransferID, "[^_]+"),
         WMD2 = stringr::str_extract(TransferID, "[^_]+$")) %>%
  dplyr::select(-TransferID)

S_wmdcent <- st_centroid(S_wmdpoly)
wmdcent <- as.data.frame(st_coordinates(S_wmdcent)) %>%
  mutate(WMD1 = S_wmdpoly$WMD, WMD2 = S_wmdpoly$WMD)
centNA <- data.frame(WMD1 = 99, WMD2 = 99, X = extent(wmdpoly)[1], Y = extent(wmdpoly)[4])
wmdcent1 <- rbind(wmdcent, centNA)

set2 <- merge(set1, wmdcent1 %>% dplyr::select(-WMD2), by = "WMD1", all.x = T) %>% rename(StartX = X, StartY = Y)
set3 <- merge(set2, wmdcent1 %>% dplyr::select(-WMD1), by = "WMD2", all.x = T) %>% rename(EndX = X, EndY = Y) %>%
  filter(WMD2 != 99)

linemap <- ggplot(data = W_wmdpoly) +
  geom_sf(fill = NA) +
  geom_segment(data = set3, aes(x = StartX, y = StartY, xend = EndX, yend = EndY, size = Total)) +
  viridis::scale_color_viridis() +
  theme_void(base_size = 35) +
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

### Correlation Plot showing transition probabilities
# source("./MTC - Migratory Connectivity Estimate.R")

transprob.long <- as.data.frame(psi) %>%
  mutate(StartWMD = 0:28) %>%
  pivot_longer(cols = 1:30, names_to = "EndWMD", names_prefix = "END") %>%
  rename(TransitionProb = value) %>%
  filter(StartWMD != EndWMD) %>%
  mutate(EndWMD = as.integer(EndWMD)) %>%
  mutate(EndWMD = ifelse(EndWMD == 99, -1, EndWMD))
samedf <- data.frame(StartWMD = 0:28, EndWMD = 0:28, TransitionProb = NA) 
transprob.long <- rbind(transprob.long, samedf)
tran.matrix <- ggplot(data = transprob.long, aes(x= as.factor(EndWMD), y = as.factor(StartWMD), fill = TransitionProb)) + 
  geom_tile(color = "grey60") +
  viridis::scale_fill_viridis(na.value = "white",
                      limit = c(0,max(transprob.long$TransitionProb)), space = "Lab", name="Transition\nProbability") +
  theme_minimal(base_size = 35)+
  labs(x = "Destination (WMD)", y = "Origin (WMD)") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        aspect.ratio = 1,
        axis.ticks = element_line(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(1, "cm")) +
  scale_x_discrete(labels = c("", "0", "", "", "", "4", "", "", "", "8", "", "", "",
                              "12", "", "", "", "16", "", "", "", "20", "", "", "",
                              "24", "", "", "", "28")) +
  scale_y_discrete(labels = c("0", "", "", "", "4", "", "", "", "8", "", "", "",
                              "12", "", "", "", "16", "", "", "", "20", "", "", "",
                              "24", "", "", "", "28"))

require(patchwork)

# layout <- "
# AAAACCCCC
# AAAACCCCC
# AAAACCCCC
# BBBBCCCCC
# BBBBCCCCC
# BBBBCCCCC
# "
# plot_layout(design = layout) +
turkmove.plot <- tran.matrix + linemap + percent.plot + 
  plot_layout(widths = c(1,1,1)) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 35), plot.tag.position = c(0.05,.9))
ggsave(turkmove.plot, file = "./Figures/WMDConnectivity Summary Plots.jpg",
       width = 33, height = 12)

