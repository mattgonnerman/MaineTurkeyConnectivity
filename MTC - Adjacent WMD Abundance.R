require(sf)
require(dplyr)

turkest <- read.csv("WMDTurkeyAbundance_OGOnly.csv") %>%
  mutate(EstA = ifelse(Avail == "N", NA, EstA),
         EstJ = ifelse(Avail == "N", NA, EstJ),
         TotalTurk = ifelse(Avail == "N", NA, TotalTurk))
wmdpoly <- st_read("E:/Maine Drive/GIS/wildlife_mgmt_districts2/wildlife_mgmt_districts.shp") %>%
  dplyr::select(WMD = IDENTIFIER, Area = SHAPE_area)
wmdpoly$Area[which(wmdpoly$WMD ==27)] <- wmdpoly$Area[which(wmdpoly$WMD ==27)]/4

turkdens <- merge(wmdpoly, turkest, by = "WMD", all.y = T) %>%
  mutate(JDens = EstJ/Area,
         ADens = EstA/Area)

turkdens.est <- turkdens %>% filter(!is.na(EstA))
turkdens.na <- turkdens %>% filter(is.na(EstA))

touching <- st_touches(turkdens)

touching.na <- st_touches(turkdens.na, turkdens)


turkdens.out <- turkdens

#Have to do the wmds neighboring others first
st_touches(turkdens.na, turkdens.est)
for(i in c(1,5,6,7)){
  temppoly <- turkdens[touching.na[[i]],]
  turkdens.out$JDens[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- mean(temppoly$JDens, na.rm = T)
  turkdens.out$ADens[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- mean(temppoly$ADens, na.rm = T)
    
  turkdens.out$EstA[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- turkdens.out$ADen[which(turkdens.out$WMD == turkdens.na$WMD[i])] * turkdens$Area[which(turkdens.out$WMD == turkdens.na$WMD[i])]
  turkdens.out$EstJ[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- turkdens.out$JDen[which(turkdens.out$WMD == turkdens.na$WMD[i])] * turkdens$Area[which(turkdens.out$WMD == turkdens.na$WMD[i])]
}

turkdens.na <- turkdens.out %>% filter(is.na(EstA))
turkdens.est <- turkdens %>% filter(!is.na(EstA))
touching.na <- st_touches(turkdens.na, turkdens.out)
st_touches(turkdens.na, turkdens.out %>% filter(!is.na(EstA)))
for(i in c(1:4)){
  temppoly <- turkdens.out[touching.na[[i]],]
  turkdens.out$JDens[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- mean(temppoly$JDens, na.rm = T)
  turkdens.out$ADens[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- mean(temppoly$ADens, na.rm = T)
  
  turkdens.out$EstA[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- turkdens.out$ADen[which(turkdens.out$WMD == turkdens.na$WMD[i])] * turkdens$Area[which(turkdens.out$WMD == turkdens.na$WMD[i])]
  turkdens.out$EstJ[which(turkdens.out$WMD == turkdens.na$WMD[i])] <- turkdens.out$JDen[which(turkdens.out$WMD == turkdens.na$WMD[i])] * turkdens$Area[which(turkdens.out$WMD == turkdens.na$WMD[i])]
}

turkdens.final <- turkdens.out %>% mutate(TotalTurk = ceiling(EstA + EstJ)) %>%
  st_drop_geometry()

sum(turkdens.final$TotalTurk)

write.csv(turkdens.final, "WMDTurkeyAbundance.csv", row.names = F)
