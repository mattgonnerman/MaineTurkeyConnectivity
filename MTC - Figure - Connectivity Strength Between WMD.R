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
  geom_sf(aes(fill = PercentDisp)) +
  viridis::scale_fill_viridis(breaks = breaks,
                              values = colval) +
  theme_void(base_size = 45) +
  labs(fill = "Percent\nGain") +
  theme(legend.position = c(.80, .16),
        legend.title = element_blank(),
        plot.title = element_text(vjust = -20),
        legend.key.width = unit(1.2,"cm"),
        legend.key.height = unit(1.5,"cm"))


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
  theme_void(base_size = 45) +
  theme(legend.position = c(.80, .16),
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
  geom_tile(color = "black") +
  viridis::scale_fill_viridis(na.value = "white",
                      limit = c(0,max(transprob.long$TransitionProb)), space = "Lab", name="Transition\nProbability") +
  theme_minimal(base_size = 45)+
  labs(x = "Destination (WMD)", y = "Origin (WMD)") +
  theme(legend.position = c(.5, 1.20),
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank(),
        aspect.ratio = 1)


require(patchwork)

layout <- "
AAAACCCCC
AAAACCCCC
AAAACCCCC
BBBBCCCCC
BBBBCCCCC
BBBBCCCCC
"

turkmove.plot <- tran.matrix + linemap + percent.plot + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 50))
ggsave(turkmove.plot, file = "./Figures/WMDConnectivity Summary Plots.jpg",
       width = 30, height = 30)

, plot.tag.position = c(0.05,.9)
# 
# 
# 
# netdispl <- function(xrow, df){
#   turkout <- xrow[3]
#   start <- xrow[1]
#   end <-xrow[2]
#   
#   turkin <- set1$n[which(set1$StartWMD == end & set1$EndWMD == start)]
#   netturk <- turkout - turkin
#   
#   if(identical(netturk, numeric(0))){
#     return(turkout)
#   }else{
#     return(turkout - turkin)
#   }
#   
# }
# 
# set2$NetDispl <- unlist(apply(set1, 1, netdispl))
# 
# #The final colum for NetDispl refers to the net number of turkeys that left a wmd for the end wmd
# set3 <- merge(set1, set2, by = c("StartWMD", "EndWMD"), all = T) %>%
#   filter(NetDispl > 0)
# 
# wmdcent <- as.data.frame(st_coordinates(S_wmdcent)) %>%
#   mutate(StartWMD = c(0:28, 99), EndWMD = c(0:28,99)) %>%
#   mutate(EndX = X, EndY = Y) %>%
#   rename(StartX = X, StartY = Y)
# wmd.cent.start <- wmdcent %>% dplyr::select(StartWMD, StartX, StartY)
# wmd.cent.end <- wmdcent %>% dplyr::select(EndWMD, EndX, EndY)
# 
# set4 <- merge(set3, wmd.cent.end, by = "EndWMD", all.x = T)
# set5 <- merge(set4, wmd.cent.start, by = "StartWMD", all.x = T)
# set6 <- set5 %>% filter(EndWMD != 99)
# 
# ggplot(data = W_wmdpoly) +
#   geom_sf(fill) +
#   geom_segment(data = set6, aes(x = StartX, y = StartY, xend = EndX, yend = EndY, size = NetDispl),
#                arrow = arrow(length = unit(1, "cm"))) +
#   scale_size_continuous(range = c(.5,2))
# theme_classic()
# 
# ### CONSIDER USING DOTS AT CENTROID WHERE SIZE = NET DISPLACEMENT
# netout <- set1 %>% group_by(StartWMD) %>% summarize(NetOut = sum(n)) %>% rename(WMD = StartWMD)
# netin <- set1 %>% group_by(EndWMD) %>% summarize(NetIn = sum(n)) %>% rename(WMD = EndWMD)
# netdispl.df <- merge(netout, netin, by = "WMD", all.x = T) %>%
#   mutate(NetDispl = NetIn - NetOut)
# wmdabund <- read.csv("WMDTurkeyAbundance.csv") %>% mutate(TotalTurk = TotalTurk*2) %>%
#   dplyr::select(WMD, TotalTurk)  
# netdispl.sf <- merge(W_wmdpoly, netdispl.df, by = "WMD")
# netdispl.sf <- merge(netdispl.sf, wmdabund, by = "WMD", all.x = T) %>%
#   mutate(PercentDisp = NetDispl/TotalTurk)
# 
# breaks <- round(c(min(netdispl.sf$NetDispl), 0, max(netdispl.sf$NetDispl)), 2)
# colval <- (breaks - min(breaks))/(max(breaks) - min(breaks))
# net.plot <- ggplot(data = netdispl.sf) +
#   geom_sf(aes(fill = NetDispl)) +
#   scale_fill_gradientn(colors = c("#b24525", "#ece9d5", "#146f6c"),
#                        breaks = breaks,
#                        values = colval) +
#   theme_void(base_size = 20) +
#   labs(fill = "Net\nGain") +
#   theme(legend.position = c(.83, .15),
#         legend.title = element_blank(),
#         plot.title = element_text(vjust = -20),
#         legend.key.width = unit(1,"cm"),
#         legend.key.height = unit(1,"cm"))
# 
# breaks <- round(c(min(netdispl.sf$PercentDisp), 0, 
#                   max(netdispl.sf[netdispl.sf$PercentDisp < 0.85,]$PercentDisp),
#                   max(netdispl.sf$PercentDisp)), 2)
# colval <- (breaks - min(breaks))/(max(breaks) - min(breaks))
# percent.plot <- ggplot(data = netdispl.sf) +
#   geom_sf(aes(fill = PercentDisp)) +
#   scale_fill_gradientn(colors = c("#b24525", "#ece9d5", "#146f6c", "#222325"),
#                        breaks = breaks,
#                        values = colval) +
#   # scale_fill_gradient(low = "#4ff2c2",
#   #                     high = "#222325",
#   #                     breaks = breaks)
#   theme_void(base_size = 20) +
#   labs(fill = "Percent\nGain") +
#   theme(legend.position = c(.83, .15),
#         legend.title = element_blank(),
#         plot.title = element_text(vjust = -20),
#         legend.key.width = unit(1,"cm"),
#         legend.key.height = unit(1,"cm"))
# 
# require(patchwork)
# 
# turkmove.plot <- net.plot + percent.plot + 
#   plot_annotation(tag_levels = 'A') & 
#   theme(plot.tag = element_text(size = 30), plot.tag.position = c(0.05,.9))
# ggsave(turkmove.plot, file = "./Figures/NetMovement.jpg",
#        width = 20, height = 12)
# 

# ### Correlation Plot
# transprob.long <- as.data.frame(psi) %>%
#   mutate(StartWMD = 0:28) %>%
#   pivot_longer(cols = 1:30, names_to = "EndWMD", names_prefix = "END") %>%
#   rename(TransitionProb = value) %>%
#   filter(StartWMD != EndWMD) %>%
#   mutate(EndWMD = as.integer(EndWMD)) %>%
#   mutate(EndWMD = ifelse(EndWMD == 99, -1, EndWMD))
# tran.matrix <- ggplot(data = transprob.long, aes(x= as.factor(EndWMD), y = as.factor(StartWMD), fill = TransitionProb)) + 
#   geom_tile() +
#   scale_fill_gradient(low = "blue", high = "red", 
#                       limit = c(0,max(transprob.long$TransitionProb)), space = "Lab", name="Transition\nProbability") +
#   theme_minimal()+
#   labs(x = "Destination (WMD)", y = "Origin (WMD)")
# ggsave(tran.matrix, file = "./Figures/TransitionMatrix_All.jpg",
#        height = 10, width = 10)
# 
# 
# netdispl <- function(xrow){
#   turkout <- xrow[3]
#   start <- xrow[1]
#   end <-xrow[2]
#   
#   turkin <- transprob.long$TransitionProb[which(transprob.long$StartWMD == end & transprob.long$EndWMD == start)]
#   netturk <- turkout - turkin
#   
#   if(identical(netturk, numeric(0))){
#     return(turkout)
#   }else{
#     return(turkout - turkin)
#   }
#   
# }
# 
# transprob.long$NetTrans <- unlist(apply(transprob.long, 1, netdispl))
# View(transprob.long)
# 
# nettrans <- transprob.long %>% filter(NetTrans > 0)
# tran.matrix.sparse <- ggplot(data = nettrans, aes(x= as.factor(EndWMD), y = as.factor(StartWMD), fill = NetTrans)) + 
#   geom_tile() +
#   scale_fill_gradient(low = "blue", high = "red", na.value = "grey50",
#                       limit = c(0,max(nettrans$NetTrans)), space = "Lab", name="Transition\nProbability") +
#   theme_minimal() +
#   labs(x = "Destination (WMD)", y = "Origin (WMD)")
# ggsave(tran.matrix.sparse, file = "./Figures/TransitionMatrix_Net.jpg",
#        height = 10, width = 10)
# 
# set3 <- merge(set1, set2, by = c("StartWMD", "EndWMD"), all = T) %>%
#   filter(NetDispl > 0)
# 
# 
# ### Net Transition Probability Map
# wmdcent <- as.data.frame(st_coordinates(S_wmdcent)) %>%
#   mutate(StartWMD = c(0:28, 99), EndWMD = c(0:28,99)) %>%
#   mutate(EndX = X, EndY = Y) %>%
#   rename(StartX = X, StartY = Y)
# wmd.cent.start <- wmdcent %>% dplyr::select(StartWMD, StartX, StartY)
# wmd.cent.end <- wmdcent %>% dplyr::select(EndWMD, EndX, EndY)
# 
# set4 <- merge(nettrans, wmd.cent.end, by = "EndWMD", all.x = T)
# set5 <- merge(set4, wmd.cent.start, by = "StartWMD", all.x = T)
# set6 <- set5 %>% filter(EndWMD != 99)
# 
# arrowmap <- ggplot(data = W_wmdpoly) +
#   geom_sf(fill = NA) +
#   geom_segment(data = set6, aes(x = StartX, y = StartY, xend = EndX, yend = EndY, size = NetTrans)) +
#   geom_segment(data = set6, aes(x = StartX, y = StartY, xend = StartX+((EndX-StartX)/1.5), yend = StartY+((EndY-StartY)/1.5), size = NetTrans),
#                arrow = arrow(length = unit(.5, "cm"), type = "open")) +
#   scale_size_continuous(range = c(.25,2.5)) +
#   theme_void()
# 
# ggsave(arrowmap, file = "./Figures/TransitionMap.jpg",
#        height = 10, width = 10)
