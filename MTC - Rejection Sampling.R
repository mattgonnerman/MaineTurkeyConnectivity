lapply(c("dplyr", "sf"), require, character.only = T)


reject.sample <- function(x){
  SetID <- x[1]
  BirdID <- x[2]
  E <- x[3]
  #read csv
  simdata.raw <- read.csv(paste("E:/Maine Drive/Analysis/Dissertation Backup/TurkeyConnectivity/Simulations/CalSims_OGBird",
                            BirdID, "Set", SetID, ".csv", sep = "_"))
  
  #find minimum distance from end point
  simdata <- simdata.raw %>%
    group_by(Sim.ID) %>%
    summarize(MinDist = min(D2End, na.rm = T)) %>%
    mutate(Accept = ifelse(MinDist < E, "Y", "N"))
  
  #return Y/N and parameter values as row of data
  par.vals <- simdata.raw %>%
    group_by(Sim.ID) %>%
    slice(2) %>%
    dplyr::select(Sim.ID, OG.ID, p, rho, mu, k, rate, R)
    
  output <- merge(par.vals, simdata, by = "Sim.ID")
  return(output)
  
}

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

#Create Dataframe for all datasets you wish to summarize
fun.ids <- expand.grid(SetID = 1:10, BirdID = 1:101) %>%
  mutate(E = ifelse(BirdID %in% H.ID, E.H, E.N))

reject.results <- apply(fun.ids, MARGIN = 1, FUN = reject.sample)
reject.results.df <- do.call(rbind.data.frame, reject.results)

accepted.df <- reject.results.df %>% filter(Accept == "Y")

write.csv(accepted.df, "Rejection Sampling Results.csv", row.names = F)

#Which OG birds were fully rejected
'%notin%' <- Negate('%in%')
which(1:101 %notin% sort(unique(accepted.df$OG.ID)))
