### Step Selection Function to Create Habitat Suitability Surface ###
# Load Packages
lapply(c("dplyr", "ggplot2", "move", "sf", "lubridate"), require, character.only = TRUE)

# ### Load Dispersal Movement Tracks from Movebanks ###
# login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
# 
# 
# #################################################################################################
# ### Download Used Locations for Seasonal Movements
# # Load df with dispersal timestamps
# # Start time is when hen began longer steps and left her wintering home range 
# # End time is when hen returned to localized movements in nesting range
# movement.times <- read.csv("Spring Movement Timestamps.csv") %>%
#   mutate(Start = gsub("[^0-9]", "", Start)) %>%
#   mutate(End = gsub("[^0-9]", "", End)) %>%
#   filter(WNSame == "N") #Filter birds that had overlapping nesting and wintering ranges
# 
# for(i in 1:nrow(movement.times)){
#   animalname <- as.character(movement.times$BirdID[i])
#   timestart <- movement.times$Start[i]
#   timeend <- movement.times$End[i]
#   
#   turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
#                                login = login,
#                                animal = animalname,
#                                timestamp_start = timestart,
#                                timestamp_end = timeend)
#   full_ind <- turkeygps@data %>%  
#     mutate(BirdID = animalname) %>%
#     mutate(timestamp = with_tz(timestamp, tzone = "America/New_York"))
#   
#   if(i == 1){
#     movement.points <- full_ind
#     }else{
#       movement.points <- rbind(movement.points, full_ind)
#   }
# }
# movement.points1 <- movement.points %>%
#   dplyr::select(BirdID, location_lat, location_long, timestamp) 
# write.csv(movement.points1, "GPS Seasonal - Used.csv", row.names = F) #All points 
# 
# 
# 
# ##############################################################################################
# ### Create Available Points from Used Locations
# # https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/6_RSF_SSF.html#5_ssf_with_multiple_animals
# pcks <- list("sp", "sf", "dplyr", "raster", "rgdal", "lubridate", "amt")
# sapply(pcks, require, char = TRUE)
# 
# #Load used points
# seasonalmove.used <- read.csv("GPS Seasonal - Used.csv") %>%
#   mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")) %>%
#   rename(Lat = location_lat, Long = location_long) %>%
#   mutate(ID = paste(BirdID, year(timestamp), sep = "_"))
# 
# #Save them as a shapefile for easier viewing later
# usedlocations.sf <- st_as_sf(seasonalmove.used,
#                              coords = c("Long", "Lat"))
# st_write(usedlocations.sf, ".", layer = "usedlocations", driver = "ESRI Shapefile", delete_layer = T)
# 
# # Generate step specific available points
# nested.full <- seasonalmove.used %>%
#   nest(-ID) %>%
#   mutate(track = map(data, ~ mk_track(., Long, Lat, timestamp, BirdID, crs= CRS(projection(turkeygps)))))
# 
# full.true.steps <- map(nested.full$track, steps, keep_cols = 'end')
# full.random.steps <- map(full.true.steps, random_steps, n=50, include_observed = T)
# full.steps <- bind_rows(full.random.steps, .id="ID")
# AllPoints.ssf <- as.data.frame(full.steps) %>%
#   dplyr::select(-dt_) %>%
#   mutate(Year = year(t1_)) %>%
#   group_by(BirdID, Year, step_id_, case_) %>%
#   slice(1:20) %>%
#   ungroup() %>%
#   arrange(BirdID, t1_, case_)
# 
# write.csv(AllPoints.ssf, "GPS Seasonal - SSF points.csv", row.names = F) #All points 
# 
# #Save them as a shapefile for easier viewing later
# alllocations.sff <- st_as_sf(AllPoints.ssf,
#                              coords = c("x2_", "y2_"))
# st_write(alllocations.sff, ".", layer = "SSF All points", driver = "ESRI Shapefile", delete_layer = T)
# 
# 
# 
# ################################################################################################
# ### EXTRACT VALUES
# pcks <- list("sp", "sf", "dplyr", "raster", "rgdal", "lubridate", "amt")
# sapply(pcks, require, char = TRUE)
# ### Merge Locations with Landcover Covariates (Rasters were prepared in ArcGIS)
# ## Limited to Statewide Datasets
# # Distance to Roads
# DtR.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/D2Road_30m.tif")
# # Distance to Forest Edge - LiDAR
# DtFE.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/D2Edge_30m.tif")
# # Slope - DEM
# Slope.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/ME_Slope_30m.tif")
# # Agriculture - NLCD
# Ag.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/Ag_30m_bin.tif")
# # Developed - NLCD
# Dev.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/Dev_30m_bin.tif")
# #Forest - NLCD
# Forest.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/Forest_30m_bin.tif")
# #Wetlands - Maine GIS Catalog
# Wetland.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/Wetland_30m.tif")
# # Streams/Rivers - NLCD
# Water.rast <- raster("E:/GitHub/MaineTurkeyConnectivity/GIS/TurkeyConnectivity/Water_30m_bin.tif")
# 
# 
# #Extract Spatial Covariates
# alllocations.sff <- st_read("SSF All points.shp") %>% 
#   st_set_crs("+proj=longlat +datum=WGS84") %>%
#   st_transform(projection(Dev.rast))
# 
# alllocations.sff$DtR.cov <- raster::extract(DtR.rast, alllocations.sff)
# alllocations.sff$DtFE.cov <- raster::extract(DtFE.rast, alllocations.sff)
# alllocations.sff$Slope.cov <- raster::extract(Slope.rast, alllocations.sff)
# alllocations.sff$Ag.cov <- raster::extract(Ag.rast, alllocations.sff)
# alllocations.sff$Dev.cov <- raster::extract(Dev.rast, alllocations.sff)
# alllocations.sff$Wetland.cov <- raster::extract(Wetland.rast, alllocations.sff)
# 
# ### Convert to final dataframe for SSF and clean up
# AllPoints.ssf.df <- alllocations.sff %>%
#   mutate(ConditionID = paste(BirdID, Year, step_d_, sep="_"))
# st_geometry(AllPoints.ssf.df) <- NULL
# write.csv(AllPoints.ssf.df, "SSF_SeasonalMove_inputs.csv", row.names = F)
# 
# 

####################################################################################################
### INLA ###
############
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
require(INLA)
require(dplyr)

AllPoints.ssf.df <- read.csv("SSF_SeasonalMove_inputs.csv") %>%
  mutate(BirdID = as.numeric(as.factor(BirdID)))

#Create functions for outputs
inla_emarginal <- function(r.out){ 
  results <- sapply(r.out$marginals.hyperpar, 
                    function(y) 
                      inla.emarginal(function(x) x, inla.tmarginal(function(x) 1/x, y)))
  
  names(results) <- sapply(as.vector(as.character(names(results))), function(y) gsub("Precision", x=y, "Mean of variance"))
  results
}

inla_mmarginal <- function(r.out){ 
  results <- sapply(r.out$marginals.hyperpar, 
                    function(y) 
                      inla.mmarginal(inla.tmarginal(function(x) 1/x, y)))
  
  names(results) <- sapply(as.vector(as.character(names(results))), function(y) gsub("Precision", x=y, "Mode of variance"))
  results
}

#Set mean and precision priors for slope coefficients
mean.beta <- 0
prec.beta <- 1e-4  

niid <- length(unique(AllPoints.ssf.df$BirdID))

################################################################################### 
### MODELS
sink("Seasonal Movement SSF Results.csv")
cat("SeasonalMove ~ 1")
cat('\n')
formula.random <- case_ ~  -1 + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) #Conditional

ssf.null.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.null.seasonalmove$summary.fixed)
write.csv(ssf.null.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.null.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.null.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.null.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.null.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.null.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ Ag + Ag|IND")
cat('\n')
formula.random <- case_ ~  -1 + Ag.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,Ag.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.Ag.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                              control.fixed = list(
                                mean = mean.beta,
                                prec = list(default = prec.beta)),
                              control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.Ag.seasonalmove$summary.fixed)
write.csv(ssf.Ag.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.Ag.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.Ag.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.Ag.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.Ag.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.Ag.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ Dev + Dev|IND")
cat('\n')
formula.random <- case_ ~  -1 + Dev.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,Dev.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.Dev.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.Dev.seasonalmove$summary.fixed)
write.csv(ssf.Dev.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.Dev.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.Dev.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.Dev.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.Dev.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.Dev.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ DtR + DtR|IND")
cat('\n')
formula.random <- case_ ~  -1 + DtR.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,DtR.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.DtR.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.DtR.seasonalmove$summary.fixed)
write.csv(ssf.DtR.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.DtR.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.DtR.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.DtR.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.DtR.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.DtR.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ DtFE + DtFE|IND")
cat('\n')
formula.random <- case_ ~  -1 + DtFE.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,DtFE.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.DtFE.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.DtFE.seasonalmove$summary.fixed)
write.csv(ssf.DtFE.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.DtFE.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.DtFE.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.DtFE.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.DtFE.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.DtFE.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ Wetland + Wetland|IND")
cat('\n')
formula.random <- case_ ~  -1 + Wetland.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,Wetland.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.Wetland.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.Wetland.seasonalmove$summary.fixed)
write.csv(ssf.Wetland.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.Wetland.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.Wetland.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.Wetland.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.Wetland.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.Wetland.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")

cat("SeasonalMove ~ Slope + Slope|IND")
cat('\n')
formula.random <- case_ ~  -1 + Slope.cov + #Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID,Slope.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05))))

ssf.Slope.seasonalmove <- inla(formula.random, family ="Poisson", data=AllPoints.ssf.df, 
                            control.fixed = list(
                              mean = mean.beta,
                              prec = list(default = prec.beta)),
                            control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.Slope.seasonalmove$summary.fixed)
write.csv(ssf.Slope.seasonalmove$summary.hyperpar)
cat('Posterior Mean of Variance: ')
cat('\n')
cat(inla_emarginal(ssf.Slope.seasonalmove))
cat('\n')
cat('Posterior Mode of Variance: ')
cat('\n')
cat(inla_mmarginal(ssf.Slope.seasonalmove))
cat("\n")
cat('WAIC: ')
cat('\n')
cat(ssf.Slope.seasonalmove$waic$waic)
cat("\n")
cat('DIC: ')
cat('\n')
cat(ssf.Slope.seasonalmove$dic$dic)
cat("\n")
cat('Marginal Likelihood: ')
cat('\n')
cat(ssf.Slope.seasonalmove$mlik[2,1])
cat("\n")
cat("\n")



sink()


save(ssf.Slope.seasonalmove, ssf.Wetland.seasonalmove, ssf.Ag.seasonalmove, ssf.Dev.seasonalmove,
     ssf.DtR.seasonalmove, ssf.DtFE.seasonalmove, ssf.null.seasonalmove, file = "SSFResults.RData")
load("SSFResults.RData")


### Global Model
globaldata <- AllPoints.ssf.df
globaldata$BirdID1 = globaldata$BirdID2 = globaldata$BirdID3 = globaldata$BirdID4 = globaldata$BirdID5 = globaldata$BirdID6 = globaldata$BirdID
formula.random <- case_ ~  -1 + Ag.cov + Dev.cov + Wetland.cov + Slope.cov + DtR.cov + DtFE.cov +#Fixed effects
  sl_ + #step length
  f(ConditionID, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = T))) + #Conditional
  f(BirdID1,Ag.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) +
  f(BirdID2,Dev.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) +
  f(BirdID3,Wetland.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) +
  f(BirdID4,Slope.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) +
  f(BirdID5,DtR.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) +
  f(BirdID6,DtFE.cov,values=1:niid,model="iid", #Random Slope
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) 

ssf.GLOBAL.seasonalmove <- inla(formula.random, family ="Poisson", data=globaldata, 
                               control.fixed = list(
                                 mean = mean.beta,
                                 prec = list(default = prec.beta)),
                               control.compute = list(mlik = TRUE, dic = TRUE, waic = TRUE))
write.csv(ssf.GLOBAL.seasonalmove$summary.fixed, "Global SSF Coefficients.csv")
ssf.GLOBAL.seasonalmove$summary.hyperpar
ssf.GLOBAL.seasonalmove$waic$waic
save(ssf.GLOBAL.seasonalmove, file = "SSF Global Model.RData")
