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

write.table(df, "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv", row.names = F,
          append = T, sep = ",", col.names = !file.exists("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv"))

# Delete all the files so you don't duplicate entries
sapply(cleanup, unlink)

# df.merged <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks.csv")
# 
# #How many simulations have been completed
# nrow(df.merged)
# length(unique(df.merged$BirdID))
# 
# 
# #How Many Birds Dispersed
# df.merged %>%
#   group_by(BirdID) %>% 
#   summarize(Total = n()) %>%
#   ungroup() %>%
#   mutate(Dispersed = ifelse(Total == 2, 0, 1)) %>%
#   group_by(Dispersed) %>%
#   summarize(Total = n())

df.merged.1 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part1.csv")
df.merged.2 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part2.csv")
df.merged.3 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part3.csv")
df.merged.4 <- read.csv("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/WMDConnectSimTracks_part4.csv")

df.merged <- do.call("rbind", list(df.merged.1, df.merged.2, df.merged.3, df.merged.4))

df.screwed <- df.merged %>%
  group_by(BirdID) %>%
  summarize(Total = n()) %>%
  filter(Total > 451)



df.comp <- unique(df.merged$BirdID)
length(df.comp)

save(df.comp, file = "E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/CompletedSims.RData")
