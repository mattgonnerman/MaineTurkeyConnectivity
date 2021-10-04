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
