### WMD Connect file cleanup
require(tidyverse)

cleanup <- dir(path = "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSim",
               full.names = T, pattern = "WMDConnectSimTrack")

df <- cleanup %>% 
  map(function(x) {
    read.csv(x)
  }) %>%
  reduce(rbind)

head(df)

write.table(df, "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks.csv", row.names = F,
          append = T, sep = ",", col.names = !file.exists("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks.csv"))

# Delete all the files so you don't duplicate entries
sapply(cleanup, unlink)

df.merged <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks.csv")

#How many simulations have been completed
nrow(df.merged)
length(unique(df.merged$BirdID))


#How Many Birds Dispersed
df.merged %>%
  group_by(BirdID) %>% 
  summarize(Total = n()) %>%
  ungroup() %>%
  mutate(Dispersed = ifelse(Total == 2, 0, 1)) %>%
  group_by(Dispersed) %>%
  summarize(Total = n())
