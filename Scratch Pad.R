
#Scratch Pad for testing these functions

startpoint.df <- test1 <- x <-  startlocs.df[1,]

simstartmove(test1)

dec.output <- data.frame(ID = 1,
                         CellID = NA, 
                         HS = NA,
                         D = NA,
                         x = test1$Long[1],
                         y = test1$Lat[1],
                         B = runif(1, 0, 360), 
                         TurnA = runif(1, -pi, pi),
                         W = NA)

location <- test2 <- cbind(test1, dec.output)
raster <- HS_roost

step.decision <- x <- sim.decision(test2, HS_roost, 130, 1)
test3 <- cbind(test2[1,1:12], step.decision) %>% mutate(Step = Step +1)


settle.decision(test3)



source("./MTC - WMD Connectivity Functions.R")
test4 <- sim.disperse.wmd(startlocs.df[1,], HS_day, HS_roost)

options <- as.data.frame(raster::extract(raster, location[,c("x", "y")], buffer = location$R[1], cellnumbers = T, df = T)) %>%
  rename(HS = layer, CellID = cells)

rasterday <- HS_day
rasterroost <- HS_roost


sim.output <- test4

simwmdconnect(startlocs.df[1,])
