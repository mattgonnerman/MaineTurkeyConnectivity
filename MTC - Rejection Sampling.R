lapply(c("dplyr", "sf", "raster"), require, character.only = T)

### Data Prep
#Load Town Boundary Polygon
townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  st_transform(crs = 32619)

#Need to know if Nest or Harvest to specify E
disperser.end <- st_read("./GIS/Disperser End.shp")
H.ID <- which(disperser.end$ObsType == "H")

# Use Town polygon Size and Prenesting Range Quantiles to Specific E
# Load Town Boundaries Shapefile
lapply(c("dplyr", "sf"), require, character.only = TRUE)
townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  group_by(TOWN) %>%
  filter(TArea_KM2 == max(TArea_KM2))
E.H <- (quantile(townbound$TArea_KM2, .95)/pi)^(1/2) *1000


seasonalUDs <- st_read("E:/Maine Drive/Analysis/Home Range Analysis/seasonalUDs_18_20_polygon.shp") %>%
  filter(Season == "PreNest1") %>%
  filter(level == 0.99)

E.N <- (quantile(seasonalUDs$Area_km2, .95)/pi)^(1/2) *1000


### Rejection Sampling Function
reject.sample <- function(x){
  SetID <- as.integer(x[1])
  BirdID <- as.integer(x[2])
  HorN <- x[3]
  E <- x[4]
  
  #read csv
  simdata.raw <- read.csv(paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird",
                            BirdID, "Set", SetID, ".csv", sep = "_"))

  
  if(HorN == "N"){ #Rejection Sampling for Nests (Distance to Endpoint)
    #find minimum distance from end point
    simdata <- simdata.raw %>%
      group_by(Sim.ID) %>%
      summarize(MinDist = min(D2End, na.rm = T)) %>%
      mutate(Accept = ifelse(MinDist < E, "Y", "N")) %>%
      dplyr::select(-MinDist)

    #return Y/N and parameter values as row of data
    par.vals <- simdata.raw %>%
      group_by(Sim.ID) %>%
      slice(2) %>%
      dplyr::select(Sim.ID, OG.ID, p, rho, mu, k, rate, R)
    
    output <- merge(par.vals, simdata, by = "Sim.ID")
    
  }else{#Rejection Sampling for Harvests (Within Town of Harvest)
    #Create EndPoint
    endpoint <- st_point(as.numeric(simdata.raw[1,6:7]), dim = "XY")
  
    # Create Polygon with town of bird
    town <- townbound[endpoint,] %>% dplyr::select(PointIn = RastVal)
    
    #Convert Simdata to points
    checklocs <- st_as_sf(simdata.raw %>% dplyr::select(x,y), coords = c("x", "y"), crs = crs(town))
    checklocs <- st_join(checklocs, town, join = st_intersects)
    
    #Match Check with parameters
    output <- simdata.raw %>%
      dplyr::select(Sim.ID, OG.ID, p, rho, mu, k, rate, R) %>%
      mutate(Accept = checklocs$PointIn) %>%
      mutate(Accept = ifelse(is.na(Accept), "N", "Y")) %>%
      group_by(Sim.ID) %>%
      arrange(desc(Accept)) %>%
      slice(1)
  }
  
  return(output)
  
}


### Run Code
#Create Dataframe for all datasets you wish to summarize
fun.ids <- expand.grid(SetID = 1:10, BirdID = 1:101) %>%
  mutate(HorN = ifelse(BirdID %in% H.ID, "H", "N")) %>%
  mutate(E = ifelse(BirdID %in% H.ID, NA, E.N))

reject.results <- apply(fun.ids, MARGIN = 1, FUN = reject.sample)
reject.results.df <- as.data.frame(do.call(rbind.data.frame, reject.results))

accepted.df <- reject.results.df %>% 
  mutate(Accept = ifelse(Accept == "Y", 1,0)) %>%
           filter(Accept == 1)

write.csv(accepted.df, "Rejection Sampling Results.csv", row.names = F)

accepted.df <- read.csv("Rejection Sampling Results.csv")

#Which OG birds were fully rejected
'%notin%' <- Negate('%in%')
which(1:101 %notin% sort(unique(accepted.df$OG.ID)))
