lapply(c("dplyr", 'sf', 'raster', "tidyverse"), require, character.only = T)

source("MTC - Migratory Connectivity Estimate.R")

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
  # viridis::scale_fill_viridis(breaks = breaks[c(1,3,4)],
  #                             values = colval,
  #                             guide = guide_colorbar(title = "Percent Change", 
  #                                                    title.position = "top",
  #                                                    title.hjust = .5)) +
  scale_fill_gradientn(colors = c("#292ad9", "white","orange", "#a61520"),
                       breaks = c(round(min(netdispl.sf$PercentDisp),2), 0,
                                  abs(round(min(netdispl.sf$PercentDisp),2)), round(max(netdispl.sf$PercentDisp),2)),
                       values = c(0,abs(min(netdispl.sf$PercentDisp)/(max(netdispl.sf$PercentDisp) - min(netdispl.sf$PercentDisp))),
                                  2*abs(min(netdispl.sf$PercentDisp)/(max(netdispl.sf$PercentDisp) - min(netdispl.sf$PercentDisp))),1),
                       guide = guide_colorbar(title = "Percent Change", title.position = "top", title.hjust = .5,
                                              label.theme = element_text(angle = 45))) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.70, .13),
        legend.direction = "horizontal",
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(1,"cm"))


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
  geom_segment(data = set3, lineend = "round",
               aes(x = StartX, y = StartY, xend = EndX, yend = EndY, size = Total)) +
  scale_size_continuous(breaks = c(1,50, 250, 500, 800), 
                        guide = guide_legend(title = "Total\nImmigration",
                                             title.position = "top",
                                             title.hjust = .5)) +
  theme_void(base_size = 25) +
  theme(legend.position = c(.75, .13),
        legend.direction = "vertical",
        legend.key.width = unit(2, "cm"))
  

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
  # viridis::scale_fill_viridis(na.value = "white",
  #                     limit = c(0,max(transprob.long$TransitionProb)), space = "Lab", name="Transition\nProbability",
  #                     guide = guide_colorbar(title = "Transition Probability",
  #                                            title.position = "top",
  #                                            title.hjust = .5)) +
  scale_fill_gradient(#colors = c("#292ad9", "white", "#a61520"),
                      low = "white", high = "#a61520",
                      labels = c(0, round(max(transprob.long$TransitionProb, na.rm = T)/2,2), round(max(transprob.long$TransitionProb, na.rm = T),2)), 
                      breaks = c(0, round(max(transprob.long$TransitionProb, na.rm = T)/2,2), round(max(transprob.long$TransitionProb, na.rm = T),2)-.01), 
                      space = "Lab", name="Transition\nProbability",
                      guide = guide_colorbar(title = "Transition Probability", title.position = "top", title.hjust = .5)) +
  theme_minimal(base_size = 25)+
  labs(x = "Destination (WMD)", y = "Origin (WMD)") +
  theme(legend.position = c(.5, -.1),
        legend.direction = "horizontal",
        aspect.ratio = 1,
        axis.ticks = element_line(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(1, "cm")) +
  scale_x_discrete(labels = c("", "0", "", "2", "", "4", "", "6", "", "8", "", "10", "",
                              "12", "", "14", "", "16", "", "18", "", "20", "", "22", "",
                              "24", "", "26", "", "28"),
                   position = "top") +
  scale_y_discrete(labels = c("0", "", "2", "", "4", "", "6", "", "8", "", "10", "",
                              "12", "", "14", "", "16", "", "18", "", "20", "", "22", "",
                              "24", "", "26", "", "28"))

require(patchwork)

layout <- "
AB
CC
CC
"
turkmove.plot <- percent.plot + tran.matrix + linemap + 
  plot_layout(widths = c(1.2,1,1.2)) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 35), plot.tag.position = c(0.10,.88))
ggsave(turkmove.plot, file = "./Figures/WMDConnectivity Summary Plots.jpg",
       width = 33, height = 16)

