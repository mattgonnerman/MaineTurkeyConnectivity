# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate", "raster"), require, character.only = TRUE)

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
# # Download Movement Data
# login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
# gpslocations.raw <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login)
# gps.slim <- gpslocations.raw@data %>%
#   dplyr::select(Lat = location_lat, Long = location_long, Date = timestamp) %>%
#   mutate(BirdID = substr(gpslocations.raw@trackId, 2, 10))
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
# 
# #################################################################################################################
# ### MERGE DATA ###
# ##################
# ### Starting Towns
# caplocs1 <- merge(capsites.sf, trap.slim, by = "CapLoc", all.y = T) %>%
#   dplyr::select(BirdID, BirdID2, StartDate = CapDate) %>%
#   mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y")) %>%
#   mutate(StartYear = year(StartDate))
# caplocs2 <- st_join(caplocs1, townbound, join = st_within) %>%
#   st_drop_geometry() %>%
#   rename(StartTown = Town)
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
# startend.harv <- rbind(caplocs.harv1, caplocs.harv2) %>%
#   filter(StartYear == EndYear) %>%
#   mutate(Disperser = ifelse(StartTown == EndTown, 0, 1))  %>%
#   dplyr::select(BirdID, Year = StartYear, StartTown, EndTown, Disperser)
# 
# #Nests
# nest.ready <- nest.slim %>%
#   filter(!is.na(EndLat)) %>%
#   filter(!is.na(estDate)) %>%
#   rename(EndDate = estDate) %>%
#   mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y")) %>%
#   st_as_sf(coords = c("EndLong", "EndLat"), crs = 4326)
# 
# nestends <- st_join(nest.ready, townbound, join = st_within) %>%
#   st_drop_geometry() %>%
#   rename(EndTown = Town) %>%
#   mutate(EndYear = year(EndDate))
# startend.nest <- merge(caplocs2, nestends, by = "BirdID", all.y = T) %>%
#   filter(StartYear == EndYear) %>%
#   mutate(Disperser = ifelse(StartTown == EndTown, 0, 1)) %>%
#   dplyr::select(BirdID, Year = StartYear, StartTown, EndTown, Disperser)
# 
# #RBind
# startend.full <- rbind(startend.harv, startend.nest)
# 
# #################################################################################################################
# ### COLLECT TOWN HABITAT DATA ###
# #################################
# townlist <- unique(c(startend.full$StartTown, startend.full$EndTown))
# town.reduce <- townbound %>% filter(Town %in% townlist)
# 
# towncovs <- st_read("./GIS/TownCovs.shp") %>%
#   rename(Developed = Develpd, Agriculture = Agrcltr, Grassland = Grsslnd, Connectance = Cnnctnc, StartTown = Town)
# 
# town.covs <- merge(startend.full, towncovs, by = "StartTown", all.x = T) %>%
#   dplyr::select(-geometry)
# 
# #################################################################################################################
# ### Individual Specific Covariates ###
# ######################################
# ind.covs <- trap.slim %>%
#   dplyr::select(BirdID, Sex, Age, Flock.Size)
#   
# #Merge back to observed dispersal info
# disp.input <- merge(town.covs, ind.covs, by = "BirdID", all.x = T) %>%
#   mutate(Sex = as.factor(Sex),
#          Age = as.factor(Age))
# 
# write.csv(disp.input, "DispProp_input.csv", row.names = F)
disp.input.raw <- read.csv("DispProp_input.csv")
disp.input <- disp.input.raw
disp.input[,6:15] <- sapply(6:15, FUN = function(x){scale(disp.input[,x], center = T, scale = T)})

#################################################################################################################
### RUN MODELS AND MODEL SELECTION ###
#################################
require(AICcmodavg)
cand.models <- list()
cand.models[[1]] <- dispmodel.null <- glm(Disperser ~ 1, data = disp.input, family = "binomial")
cand.models[[2]] <- dispmodel.Dev <- glm(Disperser ~ Developed, data = disp.input, family = "binomial")
cand.models[[3]] <- dispmodel.Ag <- glm(Disperser ~ Agriculture, data = disp.input, family = "binomial")
cand.models[[4]] <- dispmodel.Wet <- glm(Disperser ~ Wetland, data = disp.input, family = "binomial")
cand.models[[5]] <- dispmodel.Grass <- glm(Disperser ~ Grassland, data = disp.input, family = "binomial")
cand.models[[6]] <- dispmodel.Road <- glm(Disperser ~ Road_KM, data = disp.input, family = "binomial")
cand.models[[7]] <- dispmodel.X <- glm(Disperser ~ X, data = disp.input, family = "binomial")
cand.models[[8]] <- dispmodel.Y <- glm(Disperser ~ Y, data = disp.input, family = "binomial")
cand.models[[9]] <- dispmodel.XYInt <- glm(Disperser ~ X*Y, data = disp.input, family = "binomial")
cand.models[[10]] <- dispmodel.Dev2 <- glm(Disperser ~ poly(Developed,2), data = disp.input, family = "binomial")
cand.models[[11]] <- dispmodel.Ag2 <- glm(Disperser ~ poly(Agriculture,2), data = disp.input, family = "binomial")
cand.models[[12]] <- dispmodel.Wet2 <- glm(Disperser ~ poly(Wetland,2), data = disp.input, family = "binomial")
cand.models[[13]] <- dispmodel.Grass2 <- glm(Disperser ~ poly(Grassland,2), data = disp.input, family = "binomial")
cand.models[[14]] <- dispmodel.Road2 <- glm(Disperser ~ poly(Road_KM,2), data = disp.input, family = "binomial")
cand.models[[15]] <- dispmodel.Age <- glm(Disperser ~ Age, data = disp.input, family = "binomial")
cand.models[[16]] <- dispmodel.Sex <- glm(Disperser ~ Sex, data = disp.input, family = "binomial")
cand.models[[17]] <- dispmodel.AgeSex <- glm(Disperser ~ Age*Sex, data = disp.input, family = "binomial")
cand.models[[18]] <- dispmodel.Dev2Ag2 <- glm(Disperser ~ poly(Developed,2) + poly(Agriculture,2), data = disp.input, family = "binomial")
cand.models[[19]] <- dispmodel.DevSex <- glm(Disperser ~ Developed*Sex, data = disp.input, family = "binomial")
cand.models[[20]] <- dispmodel.AgSex <- glm(Disperser ~ Agriculture*Sex, data = disp.input, family = "binomial")
cand.models[[21]] <- dispmodel.WetSex <- glm(Disperser ~ Wetland*Sex, data = disp.input, family = "binomial")
cand.models[[22]] <- dispmodel.GrassSex <- glm(Disperser ~ Grassland*Sex, data = disp.input, family = "binomial")
cand.models[[23]] <- dispmodel.RoadSex <- glm(Disperser ~ Road_KM*Sex, data = disp.input, family = "binomial")
cand.models[[24]] <- dispmodel.Dev2Sex <- glm(Disperser ~ Developed + I(Developed^2) + Sex + Sex*Developed + Sex*I(Developed^2), data = disp.input, family = "binomial")
cand.models[[25]] <- dispmodel.Ag2Sex <- glm(Disperser ~ Agriculture + I(Agriculture^2) + Sex + Sex*Agriculture + Sex*I(Agriculture^2), data = disp.input, family = "binomial")
cand.models[[26]] <- dispmodel.Wet2Sex <- glm(Disperser ~ Wetland + I(Wetland^2) + Sex + Sex*Wetland + Sex*I(Wetland^2), data = disp.input, family = "binomial")
cand.models[[27]] <- dispmodel.Grass2Sex <- glm(Disperser ~ Grassland + I(Grassland^2) + Sex + Sex*Grassland + Sex*I(Grassland^2), data = disp.input, family = "binomial")
cand.models[[28]] <- dispmodel.Road2Sex <- glm(Disperser ~ Road_KM + I(Road_KM^2) + Sex + Sex*Road_KM + Sex*I(Road_KM^2), data = disp.input, family = "binomial")
cand.models[[29]] <- dispmodel.Aggregate <- glm(Disperser ~ Ag_Indx, data = disp.input, family = "binomial")
cand.models[[30]] <- dispmodel.Connect <- glm(Disperser ~ Connectance, data = disp.input, family = "binomial")
cand.models[[31]] <- dispmodel.EdgeDens <- glm(Disperser ~ Edg_Dns, data = disp.input, family = "binomial")
cand.models[[32]] <- dispmodel.Aggregate2 <- glm(Disperser ~ poly(Ag_Indx,2), data = disp.input, family = "binomial")
cand.models[[33]] <- dispmodel.Connect2 <- glm(Disperser ~ poly(Connectance,2), data = disp.input, family = "binomial")
cand.models[[34]] <- dispmodel.EdgeDens2 <- glm(Disperser ~ poly(Edg_Dns,2), data = disp.input, family = "binomial")
cand.models[[35]] <- dispmodel.AggregateSex <- glm(Disperser ~ Ag_Indx*Sex, data = disp.input, family = "binomial")
cand.models[[36]] <- dispmodel.ConnectSex <- glm(Disperser ~ Connectance*Sex, data = disp.input, family = "binomial")
cand.models[[37]] <- dispmodel.EdgeDensSex <- glm(Disperser ~ Edg_Dns*Sex, data = disp.input, family = "binomial")
cand.models[[38]] <- dispmodel.Aggregate2Sex <- glm(Disperser ~ Ag_Indx + I(Ag_Indx^2) + Sex + Sex*Ag_Indx + Sex*I(Ag_Indx^2), data = disp.input, family = "binomial")
cand.models[[39]] <- dispmodel.Connect2Sex <- glm(Disperser ~ Connectance + I(Connectance^2) + Sex + Sex*Connectance + Sex*I(Connectance^2), data = disp.input, family = "binomial")
cand.models[[40]] <- dispmodel.EdgeDens2Sex <- glm(Disperser ~ Edg_Dns + I(Edg_Dns^2) + Sex + Sex*Edg_Dns + Sex*I(Edg_Dns^2), data = disp.input, family = "binomial")
cand.models[[41]] <- dispmodel.FULL <- glm(Disperser ~ Y + Developed + I(Developed^2) + Sex + Sex*Developed + Sex*I(Developed^2) +
                                             Road_KM + I(Road_KM^2) + Sex + Sex*Road_KM + Sex*I(Road_KM^2), data = disp.input, family = "binomial")
cand.models[[42]] <- dispmodel.Final <- glm(Disperser ~ Y + Developed + I(Developed^2) + Sex + Sex*Developed + Sex*I(Developed^2), data = disp.input, family = "binomial")

# aictab(cand.set = cand.models)
# summary(cand.models[[42]])
# 
# ### Developed^2*Sex
# summary(dispmodel.FULL2)
# pred.data <- data.frame(Developed = rep(seq(min(disp.input$Developed),max(disp.input$Developed), .01),2),
#                         Sex = c(rep("M", length(seq(min(disp.input$Developed),max(disp.input$Developed), .01))),
#                                 rep("F", length(seq(min(disp.input$Developed),max(disp.input$Developed), .01)))),
#                         Y = 0)
# disp.prop.predict <- predict(dispmodel.FULL2, pred.data, se.fit = T, interval = "confidence")
# pred.data$Est <- disp.prop.predict$fit
# pred.data$SE <- disp.prop.predict$se.fit
# outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
#                                 LCL = exp(Est-SE)/(1+exp(Est-SE)),
#                                 UCL = exp(Est+SE)/(1+exp(Est+SE)))
# 
# DP.dev.plot <- ggplot(data = outputs.dev, group = Sex) +
#   geom_line(aes(x = Developed, y = Mean, color = Sex)) +
#   geom_line(aes(x = Developed, y = LCL, color = Sex), linetype = "dashed") +
#   geom_line(aes(x = Developed, y = UCL, color = Sex), linetype = "dashed") +
#   geom_point(data = disp.input, aes(x = Developed, color = Sex, y = Disperser), position = "dodge") +
#   theme_classic() +
#   labs(x = "Proportion Developed", y = "Probability of Dispersal")
# 
# pred.data <- data.frame(Y = rep(seq(min(disp.input$Y),max(disp.input$Y), .01),2),
#                         Sex = "M",
#                         Developed = 0)
# disp.prop.predict <- predict(dispmodel.FULL2, pred.data, se.fit = T, interval = "confidence")
# pred.data$Est <- disp.prop.predict$fit
# pred.data$SE <- disp.prop.predict$se.fit
# outputs.y <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
#                                     LCL = exp(Est-SE)/(1+exp(Est-SE)),
#                                     UCL = exp(Est+SE)/(1+exp(Est+SE)))
# 
# DP.Y.plot <- ggplot(data = outputs.y) +
#   geom_line(aes(x = Y, y = Mean)) +
#   geom_line(aes(x = Y, y = LCL), linetype = "dashed") +
#   geom_line(aes(x = Y, y = UCL), linetype = "dashed") +
#   geom_point(data = disp.input, aes(x = Y, y = Disperser), position = "dodge") +
#   theme_classic() +
#   labs(x = "Latitude", y = "Probability of Dispersal")
# 
# 
# # ### Road
# # summary(dispmodel.Road)
# # pred.data <- data.frame(Road_KM = rep(seq(min(disp.input$Road_KM),max(disp.input$Road_KM), 1),2),
# #                         Sex = c(rep("M", length(seq(min(disp.input$Road_KM),max(disp.input$Road_KM), 1))),
# #                                 rep("F", length(seq(min(disp.input$Road_KM),max(disp.input$Road_KM), 1)))))
# # disp.prop.predict <- predict(dispmodel.RoadSex, pred.data, se.fit = T, interval = "confidence")
# # pred.data$Est <- disp.prop.predict$fit
# # pred.data$SE <- disp.prop.predict$se.fit
# # outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
# #                                     LCL = exp(Est-SE)/(1+exp(Est-SE)),
# #                                     UCL = exp(Est+SE)/(1+exp(Est+SE)))
# # 
# # DP.road.plot <- ggplot(data = outputs.dev, group = Sex) +
# #   geom_line(aes(x = Road_KM, y = Mean, color = Sex)) +
# #   geom_line(aes(x = Road_KM, y = LCL, color = Sex), linetype = "dashed") +
# #   geom_line(aes(x = Road_KM, y = UCL, color = Sex), linetype = "dashed") +
# #   geom_point(data = disp.input, aes(x = Road_KM, color = Sex, y = Disperser), position = "dodge") +
# #   theme_classic() +
# #   labs(x = "Road (km)", y = element_blank()) +
# #   theme(legend.position = "none")
# 
# # ### Agriculture^2
# # summary(dispmodel.Ag2)
# # pred.data <- data.frame(Agriculture = seq(min(disp.input$Agriculture),max(disp.input$Agriculture), .01))
# # disp.prop.predict <- predict(dispmodel.Ag2, pred.data, se.fit = T, interval = "confidence")
# # pred.data$Est <- disp.prop.predict$fit
# # pred.data$SE <- disp.prop.predict$se.fit
# # outputs.dev <- pred.data %>% mutate(Mean = exp(Est)/(1+exp(Est)),
# #                                     LCL = exp(Est-SE)/(1+exp(Est-SE)),
# #                                     UCL = exp(Est+SE)/(1+exp(Est+SE)))
# # 
# # DP.ag.plot <- ggplot(data = outputs.dev) +
# #   geom_line(aes(x = Agriculture, y = Mean)) +
# #   geom_line(aes(x = Agriculture, y = LCL), linetype = "dashed") +
# #   geom_line(aes(x = Agriculture, y = UCL), linetype = "dashed") +
# #   theme_classic() +
# #   labs(x = "Proportion Agriculture", y = element_blank())
# 
# ### Group Plots
# require(patchwork)
# 
# top.model.plots <- DP.dev.plot + DP.Y.plot + 
#   plot_annotation(tag_levels = 'A') + plot_layout(ncol = 2)
# 
# ggsave(top.model.plots, file = "./Results/DispersalPropensity_topmodels.jpg",
#        height = 5, width = 8)
