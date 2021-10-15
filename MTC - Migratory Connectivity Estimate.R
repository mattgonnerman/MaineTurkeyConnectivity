### https://github.com/SMBC-NZP/MigConnectivity/blob/master/vignettes/MigConnectivity.pdf

# install.packages("devtools")
# devtools::install_github("SMBC-NZP/MigConnectivity")
lapply(c("MigConnectivity", "dplyr", 'sf', 'raster', "tidyverse"), require, character.only = T)


### Load data and shapefiles
#Load WMD connectivity simulations
simconnect.raw1 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv")
simconnect.raw2 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part2.csv")
simconnect.raw3 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part3.csv")
simconnect.raw4 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part4.csv")

simconnect.raw <- do.call("rbind", list(simconnect.raw1,simconnect.raw2,simconnect.raw3,simconnect.raw4))

# Remove accidental repeat simulations, keeping 1st version
simconnect.dups.list <- simconnect.raw %>%
  group_by(BirdID) %>%
  group_split()

dupfunction <-function(df){
  if(df$Step[2] == 0){
    # If second step value is 0, then no dispersal
    return(df[1:2,])
    break
    
  }else{
    
    if(min(diff(df$Step)) == 1){
      
      return(df)
      break
      
    }else{
      
      return(df[1:which(diff(df$Step) < 0)[1],])
      break
      
    }
  }
}

simconnect.list <- lapply(X = simconnect.dups.list, dupfunction)

simconnect.clean <- data.table::rbindlist(simconnect.list)


wmdpoly <- st_read("E:/Maine Drive/GIS/WMD Boundaries/Maine_Wildlife_Management_Districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER) %>%
  filter(WMD != 29) %>%
  arrange(WMD)

### Find Start and End WMD for each bird
sim.startloc <- simconnect.clean %>% 
  dplyr::select(BirdID, Step, StartWMD)  %>%
  group_by(BirdID) %>%
  filter(row_number() == min(row_number())) %>%
  arrange(BirdID)

sim.endloc <- simconnect.clean %>%
  dplyr::select(BirdID, Step, x, y) %>%
  group_by(BirdID) %>%
  filter(Step == max(Step))%>%
  filter(row_number() == max(row_number())) %>%
  arrange(BirdID) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly)) %>%
  st_join(., wmdpoly, join = st_intersects) %>%
  st_drop_geometry() %>%
  rename(EndWMD = WMD) %>%
  mutate(EndWMD = ifelse(is.na(EndWMD), 99, EndWMD))

### NEED THE FOLLOWING FOR MANTEL
# 1) # of Winter (W) and Spring (S) regions (Number of WMDs)
N_W <- length(unique(sim.startloc$StartWMD))
N_S <- length(unique(sim.endloc$EndWMD))

# 2) Distance between S and W regions
W_wmdpoly <- wmdpoly %>% filter(WMD %in% unique(sim.startloc$StartWMD))
W_wmdcent <- st_centroid(W_wmdpoly)
W_wmddist <- matrix(as.numeric(st_distance(W_wmdcent)),
                    nrow = N_W, ncol = N_W)

S_wmdpoly <- wmdpoly %>% filter(WMD %in% unique(sim.endloc$EndWMD))
S_wmdcent <- st_centroid(S_wmdpoly)
centNA <- st_sf(WMD = 99, geometry = st_sfc(st_point(c(extent(wmdpoly)[1], extent(wmdpoly)[4]))), crs = crs(S_wmdpoly))
S_wmdcent <- rbind(S_wmdcent, centNA)
S_wmddist <- matrix(as.numeric(st_distance(S_wmdcent)),
                       nrow = N_S, ncol = N_S)

# 3) Transition Probabilities bewteen each S and W 
sim.combo <- merge(sim.startloc, sim.endloc, by = "BirdID", all = T) %>%
  dplyr::select(BirdID, StartWMD, EndWMD) %>%
  filter(StartWMD != EndWMD)
write.csv(sim.combo, "WMDSim_StartandEnd.csv")

psi_ik <- as.matrix(sim.combo %>%
  count(StartWMD, EndWMD) %>%
  arrange(EndWMD) %>%
  pivot_wider(names_from = EndWMD, names_prefix = "END", values_from = n, values_fill = 0) %>%
  arrange(StartWMD) %>%
  dplyr::select(-StartWMD))

psi_il <- rowSums(psi_ik)

psi <- psi_ik/psi_il

rowSums(psi) #Check that all are 1

# 4) Relative abundance within each region where individuals originate from
nW <- rowSums(psi_ik)
reln_W <- nW/sum(nW)

n <- sum(nW)

### Calculate Strength of Migrator Connectivity
MC <- calcMC(originDist = W_wmddist,
             targetDist = S_wmddist,
             originRelAbund = reln_W,
             psi = psi,
             sampleSize = n)

