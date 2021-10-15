# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate", "raster", "tidyr"), require, character.only = TRUE)

# #################################################################################################################
# ### LOAD DATA ###
# #################
# # Load Capture Data
# trap.raw <- read.csv("Trapping - Data.csv")
# trap.slim <- trap.raw %>%
#   dplyr::select(BirdID = AlumBand, BirdID2 = Rivet.Band, CapLoc = Location, CapDate = Date, Recapture, Sex, Age, Flock.Size)
# 
# # Load Harvest Data
# harvest.raw <- read.csv("Harvests - Harvested Birds.csv")
# harvest.slim <- harvest.raw %>%
#   dplyr::select(BirdID = Alum.Band.ID, BirdID2 = Rivet.ID, EndYear = Year, HarvSeason = Season,
#                 EndTown = Town.of.Harvest)
# 
# # Load Nest Data
# nest.raw <- read.csv("Nest Monitoring - Nest Info.csv")
# nest.slim <- nest.raw %>%
#   filter(Nest.Attempt == 1) %>%
#   dplyr::select(BirdID = Alum.Band.ID, EndYear = Year, EndLat = NestLat, EndLong = NestLong, estDate = Est.Laying.Initiation) %>%
#   filter(!is.na(BirdID))
# 
# 
# # Load Capture Site Information
# capsites.raw <- read.csv("CaptureSites - Sheet1.csv")
# capsites.slim <- capsites.raw %>%
#   dplyr::select(Town, CapLoc = Location.Name, Lat = Latitude, Long = Longitude)
# capsites.sf <- st_as_sf(capsites.slim, coords = c("Long", "Lat"), crs = 4326) %>%
#   dplyr::select(-Town)
# 
# # Load Town Boundaries Shapefile
# townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
#   group_by(TOWN) %>%
#   filter(TArea_KM2 == max(TArea_KM2)) %>%
#   dplyr::select(Town = TOWN)
# townbound <- st_transform(townbound, crs = 4326)
# townbound.center.sf <- st_centroid(townbound)
# towncenter.df <- data.frame(Town = townbound.center.sf$Town,
#                             EndLong = st_coordinates(townbound.center.sf)[,1],
#                             EndLat = st_coordinates(townbound.center.sf)[,2])
# 
# #Correct CapSite Names
# capsites.sf <- st_join(capsites.sf, townbound, join = st_within) %>%
#   mutate(CapLong =  st_coordinates(capsites.sf)[,1],
#          CapLat =  st_coordinates(capsites.sf)[,2])
# capsites.df <- capsites.sf %>% st_drop_geometry()
# 
# Hex Polygons
hexcovs <- st_read("./GIS/HexCovs.shp") %>%
  st_transform(crs = 4326)
# 
# #################################################################################################################
# ### MERGE DATA ###
# ##################
# ### Starting Location
# caplocs1 <- merge(capsites.sf, trap.slim, by = "CapLoc", all.y = T) %>%
#   dplyr::select(BirdID, BirdID2, StartDate = CapDate) %>%
#   mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y")) %>%
#   mutate(StartYear = year(StartDate))
# caplocs2 <- st_join(caplocs1, hexcovs %>% dplyr::select(GridID), join = st_within) %>%
#   rename(StartHex = GridID) %>%
#   mutate(StartX = st_coordinates(.)[,1],
#          StartY = st_coordinates(.)[,2]) %>%
#   st_drop_geometry()
# 
# ### Combine with End Points
# #Harvest
# hasbirdID1 <- harvest.slim %>% filter(!is.na(BirdID)) %>% dplyr::select(-BirdID2)
# nobirdID1 <- harvest.slim %>% filter(is.na(BirdID))  %>% dplyr::select(-BirdID)
# 
# caplocs.harv1 <- merge(caplocs2, hasbirdID1, by = "BirdID", all.y = T) %>%
#   dplyr::select(-HarvSeason)
# caplocs.harv2 <- merge(caplocs2, nobirdID1, by = "BirdID2", all.y = T) %>%
#   dplyr::select(-HarvSeason)
# caplocs.harv3 <- rbind(caplocs.harv1, caplocs.harv2) %>%
#   filter(StartYear == EndYear) %>%
#   rename(Town = EndTown)
# caplocs.harv4 <- merge(caplocs.harv3, towncenter.df, by = "Town", all.x = T)
# caplocs.harv.sf <- st_as_sf(caplocs.harv4, coords = c("EndLong", "EndLat"), crs = 4326)
# startend.harv <- st_join(caplocs.harv.sf, hexcovs %>% dplyr::select(GridID), join = st_within) %>%
#   rename(EndHex = GridID) %>%
#   filter(StartHex != EndHex)  %>%
#   mutate(EndX = st_coordinates(.)[,1],
#          EndY = st_coordinates(.)[,2]) %>%
#   st_drop_geometry() %>%
#   dplyr::select(BirdID, Year = StartYear, StartHex, EndHex, StartX, StartY, EndX, EndY)
# 
# #Nests
# nest.ready <- nest.slim %>%
#   filter(!is.na(EndLat)) %>%
#   filter(!is.na(estDate)) %>%
#   rename(EndDate = estDate) %>%
#   mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y")) %>%
#   st_as_sf(coords = c("EndLong", "EndLat"), crs = 4326)
# 
# nestends <- st_join(nest.ready, hexcovs %>% dplyr::select(GridID), join = st_within) %>%
#   mutate(EndX = st_coordinates(.)[,1],
#          EndY = st_coordinates(.)[,2]) %>%
#   st_drop_geometry() %>%
#   rename(EndHex = GridID) %>%
#   mutate(EndYear = year(EndDate))
# startend.nest <- merge(caplocs2, nestends, by = "BirdID", all.y = T) %>%
#   filter(StartYear == EndYear) %>%
#   filter(StartHex != EndHex)  %>%
#   dplyr::select(BirdID, Year = StartYear, StartHex, EndHex, StartX, StartY, EndX, EndY)
# 
# #RBind
# startend.full <- rbind(startend.harv, startend.nest) %>%
#   filter(BirdID != 359 & EndX != -68.41931) #2nd attempt ignored
# 
# #################################################################################################################
# ### IDENTIFY AVAILABLE TOWNS ###
# ################################
# ### Draw Line between start and end town
# lines.start <- startend.full %>%
#   dplyr::select(BirdID, Year, X = StartX, Y = StartY, Hex = StartHex) %>%
#   mutate(SE = "Start")
# lines.end <- startend.full %>%
#   dplyr::select(BirdID, Year, X = EndX, Y = EndY, Hex = EndHex) %>%
#   mutate(SE = "End")
# 
# #Combine nest and capture/first of year locations and create shapefiles
# lines <- rbind(lines.start, lines.end) %>%
#   arrange(BirdID, Year) %>%
#   group_by(BirdID, Year) %>%
#   split(.,.[,"BirdID"])
# lines.mat <- lapply(lines, function(x) as.matrix(x[names(x) %in% c("X","Y")]))
# lines.multilines.sfg <- st_multilinestring(lines.mat, dim = "XY")
# lines.multilines.sfc <- st_sfc(lines.multilines.sfg, crs = 4326)
# lines.multilines.sfc <- st_transform(lines.multilines.sfc, 32619)
# lines.lines.sfc <- st_cast(lines.multilines.sfc, "LINESTRING")
# captoobs.line <- st_sf(data.frame(lines.lines.sfc, data.frame(BirdID = names(lines))))
# st_write(captoobs.line, "./GIS/SettleAvailLine_HEX.shp", delete_layer = T)
# 
# 
# ### Buffer line based on average town size
# # Mean town size
# mean.town.area <- mean(st_area(townbound))
# mean.town.radius <- (mean.town.area^(1/2))/pi
# captoobs.poly <- st_buffer(captoobs.line, 2*mean.town.radius) %>%
#   st_transform(4326)
# st_write(captoobs.poly, "./GIS/SettleAvailPoly.shp", delete_layer = T)
# 
# ### Identify which towns overlap each buffer, these are the available
# hexoverlap <- st_intersects(captoobs.poly, hexcovs %>% dplyr::select(GridID))
# 
# hexoverlaploop <- function(x){
#   nhexs <- length(hexoverlap[[x]])
#   hexlist <- c()
#   for(i in 1:nhexs){
#     hexlist[i] <- hexcovs[hexoverlap[[x]][i],]$GridID
#   }
#   return(hexlist)
# }
# 
# hexnames.overlap <- lapply(1:length(hexoverlap), FUN = hexoverlaploop)
# names(hexnames.overlap) <- paste(captoobs.poly$BirdID, "_", sep ="")
# 
# ncol.merge <- max(unlist(lapply(1:length(hexoverlap), FUN = function(x){length(hexnames.overlap[[x]])})))
# 
# mergeid <- lines.end %>% mutate(ID = paste(BirdID, Hex, sep = "_"))
# 
# settled.hexs <- data.frame(GridID = unlist(hexnames.overlap),
#            ListID = names(unlist(hexnames.overlap))) %>%
#   mutate(BirdID = stringr::str_extract(ListID, ".+?(?=_)")) %>%
#   dplyr::select(-ListID) %>%
#   mutate(ID = paste(BirdID, GridID, sep = "_")) %>%
#   mutate(Settled = ifelse(ID %in% mergeid$ID, 1, 0)) %>%
#   filter(BirdID %in% mergeid$BirdID) %>%
#   filter(!is.na(GridID))
# 
# #################################################################################################################
# ### COLLECT TOWN HABITAT DATA ###
# #################################
# ind.covs1 <- merge(settled.hexs, hexcovs, by = "GridID", all.x = T) %>%
#   dplyr::select(-geometry, -GridID, -ID) %>%
#   arrange(BirdID, desc(Settled)) %>%
#   rename(Agriculture = Agrcltr,
#          Developed = Develpd,
#          Grassland = Herbacs,
#          Wetland = Wetlnds,
#          Connectance = Cnnctnc)
# 
# # Sex
# sex.cov <- trap.slim %>% dplyr::select(BirdID, Sex)
# settle.input <- merge(ind.covs1, sex.cov, by = "BirdID", all.x = T) %>%
#   filter(!is.na(Developed))
# 
# #################################################################################################################
# ### DISTANCE TO START ###
# #################################
# disttostart1 <- settled.hexs %>% dplyr::select(BirdID, GridID)
# disttostart2 <- hexcovs %>% dplyr::select(GridID) %>%
#   mutate(HexX = st_coordinates(st_centroid(hexcovs))[,1],
#          HexY = st_coordinates(st_centroid(hexcovs))[,2]) %>%
#   st_drop_geometry()
# disttostart3 <- merge(disttostart1, disttostart2, by = "GridID", all.x = T)
# disttostart4 <- lines.start %>% dplyr::select(BirdID, X, Y)
# disttostart5 <- merge(disttostart3, disttostart4, by = "BirdID", all.x = T)
# 
# disttostart <- pointDistance(disttostart5[,3:4], disttostart5[,5:6], lonlat = T)
# settle.input$DisttoStart <- disttostart
# 
# write.csv(settle.input, "SettDesc_input_HEX.csv", row.names = F)


#################################################################################################################
### RUN CONDITIONAL LOGISTIC REGRESSIONS ###
############################################
require(survival)
require(AICcmodavg)
settle.input <- read.csv("SettDesc_input_HEX.csv")
settle.input[,3:15] <- sapply(3:15, FUN = function(x){scale(settle.input[,x], center = T, scale = T)})
settle.input[,17] <- scale(settle.input[,17], center = T, scale = T)

cand.models <- list()

cand.models[[1]] <- settmodel.null <- clogit(Settled ~ strata(BirdID), settle.input)

cand.models[[2]] <- settmodel.X <- clogit(Settled ~ X + strata(BirdID), settle.input)
cand.models[[3]] <- settmodel.Y <- clogit(Settled ~ Y + strata(BirdID), settle.input)
cand.models[[4]] <- settmodel.XY <- clogit(Settled ~ X*Y+strata(BirdID), settle.input)

cand.models[[5]] <- settmodel.Dev <- clogit(Settled ~ Developed + strata(BirdID), settle.input)
cand.models[[6]] <- settmodel.Dev2 <- clogit(Settled ~ poly(Developed,2) + strata(BirdID), settle.input)
cand.models[[7]] <- settmodel.DevSex <- clogit(Settled ~ Developed + Developed:Sex + strata(BirdID), settle.input)
cand.models[[8]] <- settmodel.Dev2Sex <- clogit(Settled ~ Developed + I(Developed^2) + Developed:Sex +I(Developed^2):Sex + strata(BirdID), settle.input)

cand.models[[9]] <- settmodel.Ag <- clogit(Settled ~ Agriculture + strata(BirdID), settle.input)
cand.models[[10]] <- settmodel.Ag2 <- clogit(Settled ~ poly(Agriculture,2) + strata(BirdID), settle.input)
cand.models[[11]] <- settmodel.AgSex <- clogit(Settled ~ Agriculture + Agriculture:Sex + strata(BirdID), settle.input)
cand.models[[12]] <- settmodel.Ag2Sex <- clogit(Settled ~ Agriculture + I(Agriculture^2) + Agriculture:Sex +I(Agriculture^2):Sex + strata(BirdID), settle.input)

cand.models[[13]] <- settmodel.Wet <- clogit(Settled ~ Wetland + strata(BirdID), settle.input)
cand.models[[14]] <- settmodel.Wet2 <- clogit(Settled ~ poly(Wetland,2) + strata(BirdID), settle.input)
cand.models[[15]] <- settmodel.WetSex <- clogit(Settled ~ Wetland + Wetland:Sex + strata(BirdID), settle.input)
cand.models[[16]] <- settmodel.Wet2Sex <- clogit(Settled ~ Wetland + I(Wetland^2) + Wetland:Sex +I(Wetland^2):Sex + strata(BirdID), settle.input)

cand.models[[17]] <- settmodel.Grass <- clogit(Settled ~ Grassland + strata(BirdID), settle.input)
cand.models[[18]] <- settmodel.Grass2 <- clogit(Settled ~ poly(Grassland,2) + strata(BirdID), settle.input)
cand.models[[19]] <- settmodel.GrassSex <- clogit(Settled ~ Grassland + Grassland:Sex + strata(BirdID), settle.input)
cand.models[[20]] <- settmodel.Grass2Sex <- clogit(Settled ~ Grassland + I(Grassland^2)+ Grassland:Sex +I(Grassland^2):Sex + strata(BirdID), settle.input)

cand.models[[21]] <- settmodel.Road <- clogit(Settled ~ Road_KM + strata(BirdID), settle.input)
cand.models[[22]] <- settmodel.Road2 <- clogit(Settled ~ poly(Road_KM,2) + strata(BirdID), settle.input)
cand.models[[23]] <- settmodel.RoadSex <- clogit(Settled ~ Road_KM + Road_KM:Sex + strata(BirdID), settle.input)
cand.models[[24]] <- settmodel.Road2Sex <- clogit(Settled ~ Road_KM + I(Road_KM^2) + Road_KM:Sex +I(Road_KM^2):Sex + strata(BirdID), settle.input)

cand.models[[25]] <- settmodel.AgInd <- clogit(Settled ~ Ag_Indx + strata(BirdID), settle.input)
cand.models[[26]] <- settmodel.AgInd2 <- clogit(Settled ~ poly(Ag_Indx,2) + strata(BirdID), settle.input)
cand.models[[27]] <- settmodel.AgIndSex <- clogit(Settled ~ Ag_Indx + Ag_Indx:Sex + strata(BirdID), settle.input)
cand.models[[28]] <- settmodel.AgInd2Sex <- clogit(Settled ~ Ag_Indx + I(Ag_Indx^2) + Ag_Indx:Sex +I(Ag_Indx^2):Sex + strata(BirdID), settle.input)

cand.models[[29]] <- settmodel.Connect <- clogit(Settled ~ Connectance + strata(BirdID), settle.input)
cand.models[[30]] <- settmodel.Connect2 <- clogit(Settled ~ poly(Connectance,2) + strata(BirdID), settle.input)
cand.models[[31]] <- settmodel.ConnectSex <- clogit(Settled ~ Connectance + Connectance:Sex + strata(BirdID), settle.input)
cand.models[[32]] <- settmodel.Connect2Sex <- clogit(Settled ~ Connectance + I(Connectance^2) + Connectance:Sex +I(Connectance^2):Sex + strata(BirdID), settle.input)

cand.models[[33]] <- settmodel.EdgeDens <- clogit(Settled ~ Edg_Dns + strata(BirdID), settle.input)
cand.models[[34]] <- settmodel.EdgeDens2 <- clogit(Settled ~ poly(Edg_Dns,2) + strata(BirdID), settle.input)
cand.models[[35]] <- settmodel.EdgeDensSex <- clogit(Settled ~ Edg_Dns + Edg_Dns:Sex + strata(BirdID), settle.input)
cand.models[[36]] <- settmodel.EdgeDens2Sex <- clogit(Settled ~ Edg_Dns + I(Edg_Dns^2) + Edg_Dns:Sex +I(Edg_Dns^2):Sex + strata(BirdID), settle.input)

aictab(cand.set = cand.models)

cand.models[[37]] <- settmodel.Full <- clogit(Settled ~  Wetland + I(Wetland^2) + Wetland:Sex +I(Wetland^2):Sex + 
                                                Agriculture + I(Agriculture^2) + Agriculture:Sex +I(Agriculture^2):Sex +
                                                Developed + Developed:Sex + 
                                                X + strata(BirdID), settle.input)

aictab(cand.set = cand.models)
settmodel.Full

cand.models[[38]] <- settmodel.Final <- clogit(Settled ~  Wetland + I(Wetland^2) + Wetland:Sex +I(Wetland^2):Sex + 
                                                 Agriculture + I(Agriculture^2) + Agriculture:Sex +I(Agriculture^2):Sex +
                                                + strata(BirdID), settle.input)
aictab(cand.set = cand.models)


### Produce Probability of settling for each
settle.input.raw <- read.csv("SettDesc_input_HEX.csv")
wetmean <- mean(settle.input.raw$Wetland)
wetsd <- sd(settle.input.raw$Wetland)
agmean <- mean(settle.input.raw$Agriculture)
agsd <- sd(settle.input.raw$Agriculture)

x <- settmodel.Final$coefficients

settle.predict <- hexcovs %>% st_drop_geometry() %>% dplyr::select(GridID, Wetland = Wetlnds, Agriculture = Agrcltr) %>%
  mutate(Wetland = (Wetland - wetmean)/wetsd,
         Agriculture = (Agriculture - agmean)/agsd) %>%
  mutate(PreLinkF = x[1]*Wetland + x[2]*(Wetland^2) + x[3]*Agriculture + x[4]*(Agriculture^2)) %>%
  mutate(PreLinkM = PreLinkF + x[5]*Wetland + x[6]*(Wetland^2) + x[7]*Agriculture + x[8]*(Agriculture^2)) %>%
  mutate(SettleProbF = exp(PreLinkF)/(1+exp(PreLinkF)),
         SettleProbM = exp(PreLinkM)/(1+exp(PreLinkM))) %>%
  dplyr::select(GridID, SettleProbF, SettleProbM)


summary(settle.predict)

write.csv(settle.predict, "HexCov_Settle.csv", row.names = F)

###Examine Model outputs
settle.input.raw <- read.csv("SettDesc_input_HEX.csv")
wetmean <- mean(settle.input.raw$Wetland)
wetsd <- sd(settle.input.raw$Wetland)
agmean <- mean(settle.input.raw$Agriculture)
agsd <- sd(settle.input.raw$Agriculture)

preddf <- data.frame(Agriculture = 0,
                     Sex = rep(c(1, 0), each = length(seq(-2,2,.1))),
                     Wetland = rep(seq(-2,2,.1),2)) 
x <- settmodel.Final$coefficients

settle.predict <- preddf %>%
  mutate(PreLink = x[1]*Wetland + x[2]*(Wetland^2) + x[5]*Sex*(Wetland^2) + x[3]*Agriculture + x[4]*(Agriculture^2) + x[6]*Sex*(Agriculture^2)) %>%
  mutate(SettleProb = exp(PreLink)/(1+exp(PreLink))) %>%
  mutate(Wetland = (Wetland*wetsd)+wetmean)

ggplot(data = settle.predict, aes(x = Wetland, y = SettleProb, color = as.factor(Sex))) +
  geom_line() +
  xlim(0,0.35) +
  theme_classic()



preddf <- data.frame(Agriculture = rep(seq(-2,2,.1),2),
                     Sex = rep(c(1, 0), each = length(seq(-2,2,.1))),
                     Wetland = 0) 
x <- settmodel.Final$coefficients

settle.predict <- preddf %>%
  mutate(PreLink = x[1]*Wetland + x[2]*(Wetland^2) + x[5]*Sex*(Wetland^2) + x[3]*Agriculture + x[4]*(Agriculture^2) + x[6]*Sex*(Agriculture^2)) %>%
  mutate(SettleProb = exp(PreLink)/(1+exp(PreLink))) %>%
  mutate(Agriculture = (Agriculture*agsd)+agmean)

ggplot(data = settle.predict, aes(x = Agriculture, y = SettleProb, color = as.factor(Sex))) +
  geom_line() +
  xlim(0,0.15) +
  theme_classic()

preddf <- data.frame(Agriculture = 0,
                     Sex = c(1, 0),
                     Wetland = 0) 
x <- settmodel.Final$coefficients

settle.predict <- preddf %>%
  mutate(PreLink = x[1]*Wetland + x[2]*(Wetland^2) + x[5]*Sex*(Wetland^2) + x[3]*Agriculture + x[4]*(Agriculture^2) + x[6]*Sex*(Agriculture^2)) %>%
  mutate(SettleProb = exp(PreLink)/(1+exp(PreLink)))

ggplot(data = settle.predict, aes(x = Wetland, y = SettleProb, color = as.factor(Sex))) +
  geom_line() +
  theme_classic()


#################################################################################################################
### CREATE PLOTS ###
####################

### Plot settling probability across grid
require(ggplot2)

settlehexplot <- merge(hexcovs, settle.predict, by = "GridID", all.x = T)

ggplot(data = settlehexplot) +
  geom_sf(aes(fill = SettleProbF))

ggplot(data = settlehexplot) +
  geom_sf(aes(fill = SettleProbM))

# m.cf <- log(summary(settmodel.Full)$conf.int)
# 
# #Agriculture
# ag.data.plot <- data.frame(Ag = seq(min(settle.input$Agriculture), max(settle.input$Agriculture), .01),
#                            Dev = mean(settle.input$Developed),
#                            Wet = mean(settle.input$Wetland)) %>%
#   mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
#   mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
#   mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))
# 
# SD.ag2.plot <- ggplot(data = ag.data.plot) +
#   geom_line(aes(x = Ag, y = Est)) +
#   geom_line(aes(x = Ag, y = LCL), linetype = "dashed") +
#   geom_line(aes(x = Ag, y = UCL), linetype = "dashed") +
#   theme_classic() +
#   labs(x = "Proportion Agriculture", y = "log(RSF)") +
#   scale_y_continuous(trans = 'log10')
# 
# #Developed
# dev.data.plot <- data.frame(Dev = seq(min(settle.input$Developed), max(settle.input$Developed), .01),
#                            Ag = mean(settle.input$Agriculture),
#                            Wet = mean(settle.input$Wetland)) %>%
#   mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
#   mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
#   mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))
# 
# SD.dev.plot <- ggplot(data = dev.data.plot) +
#   geom_line(aes(x = Dev, y = Est)) +
#   geom_line(aes(x = Dev, y = LCL), linetype = "dashed") +
#   geom_line(aes(x = Dev, y = UCL), linetype = "dashed") +
#   theme_classic() +
#   labs(x = "Proportion Developed", y = element_blank()) +
#   scale_y_continuous(trans = 'log10')
# 
# #Wetland
# wet.data.plot <- data.frame(Wet = seq(min(settle.input$Wetland), max(settle.input$Wetland), .01),
#                            Dev = mean(settle.input$Developed),
#                            Ag = mean(settle.input$Agriculture)) %>%
#   mutate(Est = exp((Ag*m.cf[1,1]) + ((Ag^2)*m.cf[2,1]) + (Dev*m.cf[3,1]) + (Wet*m.cf[4,1]) + ((Wet^2)*m.cf[5,1]))) %>%
#   mutate(LCL = exp((Ag*m.cf[1,3]) + ((Ag^2)*m.cf[2,3]) + (Dev*m.cf[3,3]) + (Wet*m.cf[4,3]) + ((Wet^2)*m.cf[5,3]))) %>%
#   mutate(UCL = exp((Ag*m.cf[1,4]) + ((Ag^2)*m.cf[2,4]) + (Dev*m.cf[3,4]) + (Wet*m.cf[4,4]) + ((Wet^2)*m.cf[5,4])))
# 
# SD.wet2.plot <- ggplot(data = wet.data.plot) +
#   geom_line(aes(x = Wet, y = Est)) +
#   geom_line(aes(x = Wet, y = LCL), linetype = "dashed") +
#   geom_line(aes(x = Wet, y = UCL), linetype = "dashed") +
#   theme_classic() +
#   labs(x = "Proportion Wetland", y = element_blank()) +
#   scale_y_continuous(trans = 'log10')
# 
# ### Group Plots
# require(patchwork)
# 
# top.model.plots <- SD.ag2.plot + SD.dev.plot + SD.wet2.plot +
#   plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)
# 
# ggsave(top.model.plots, file = "./Results/SettlingDecision_topmodels.jpg",
#        height = 5, width = 12)
