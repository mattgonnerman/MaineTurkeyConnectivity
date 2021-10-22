### https://github.com/SMBC-NZP/MigConnectivity/blob/master/vignettes/MigConnectivity.pdf

# install.packages("devtools")
# devtools::install_github("SMBC-NZP/MigConnectivity")
lapply(c("dplyr", 'sf', 'raster', "tidyverse", "units"), require, character.only = T)


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
  dplyr::select(BirdID, Step, x, y)  %>%
  group_by(BirdID) %>%
  filter(row_number() == min(row_number())) %>%
  arrange(BirdID) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly))

sim.endloc <- simconnect.clean %>%
  dplyr::select(BirdID, Step, x, y) %>%
  group_by(BirdID) %>%
  filter(Step == max(Step))%>%
  filter(row_number() == max(row_number())) %>%
  arrange(BirdID) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(wmdpoly))

#Simulated Turkey Characteristics
startlocs.df <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/WMDConnectSimStart.csv")

startlocs.df$DistTravel <- drop_units(st_distance(sim.startloc, sim.endloc, by_element = T))

### Proportion that ended in new WMD
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

combo <- merge(sim.startloc, sim.endloc, by = "BirdID", all = T) %>%
  mutate(Disperser = ifelse(StartWMD == EndWMD, 0, 1))
startlocs.df$LeftWMD <- combo$Disperser

simmovesumm <- simconnect.clean %>%
  group_by(BirdID) %>%
  summarize(Total = n()) %>%
  mutate(SeasonMove = ifelse(Total == 2, 0,1))
startlocs.df$SeasonMove <- simmovesumm$SeasonMove

### How many turkeys initiated seasonal movements
startlocs.df %>% 
  group_by(Sex, SeasonMove) %>%
  summarize(Total = n())  

#How many turkeys left the WMD they started in
startlocs.df %>% 
  group_by(LeftWMD, Sex) %>%
  summarize(Total = n())


#Mean Distance Traveled 
summaryWMD <- startlocs.df %>% 
  group_by( SeasonMove, LeftWMD, Sex) %>%
  summarize(Total = n(),
            Mean = mean(DistTravel),
            Median = median(DistTravel),
            Min = min(DistTravel),
            Max = max(DistTravel))

write.csv(summaryWMD, "./WMDConnectMovementSummary.csv", row.names = F)

ggplot(data = startlocs.df) +
  geom_violin(aes(x = Sex, y = DistTravel, color = as.factor(LeftWMD), fill = as.factor(SeasonMove)))
