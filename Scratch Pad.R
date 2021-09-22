
#Scratch Pad for testing these functions

test1 <- x <-  startlocs.df[1,]

simstartmove(test1)

randwithintown(test1)

sim.decision(test1, HS_day, 130, 1)

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
raster <- HS_day

x <- sim.decision(test2, HS_day, 130, 1)
