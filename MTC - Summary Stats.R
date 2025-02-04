require(dplyr)
### Define Priors for weighting equation

AllPoints.ssf.df <- read.csv("SSF_SeasonalMove_inputs.csv") %>%
  filter(case_ == 1) %>%
  mutate(sl_ = sl_/0.00001) %>%
  filter(sl_ != 0)

#Summary of number of points per day
pointsperday <- AllPoints.ssf.df %>%
  group_by(BirdID, t1_) %>%
  summarize(Total = n())
summary(pointsperday$Total)

#Step Length Summary
summary(AllPoints.ssf.df$sl_)
summary(AllPoints.ssf.df$ta_)

#Min Observed distance to road/forest


max(AllPoints.ssf.df$DtR.cov)
summary(AllPoints.ssf.df$DtR.cov)
max(AllPoints.ssf.df$DtFE.cov)
summary(AllPoints.ssf.df$DtFE.cov)


#Define Step and Turning Angle Distributions
# install.packages("fitdistrplus")
lapply(c("ggplot2", "fitdistrplus", "CircStats"), require, character.only = TRUE)

# Density plot of Step Length
ggplot(data = AllPoints.ssf.df) +
  geom_density(aes(x = sl_/0.00001))

fitdist(AllPoints.ssf.df$sl_, "gamma")

test <- data.frame(x = rgamma(1000 , 0.84834637, 0.00311007))
ggplot(data = data.frame(x = test)) +
  geom_density(aes(x = x))
  
# Density plot of Turning Angle
ggplot(data = AllPoints.ssf.df) +
  geom_density(aes(x = ta_))

fitdist(AllPoints.ssf.df$ta_, "cauchy")

test <- data.frame(x = rcauchy(1000 , 0.08201666, exp(-0.93120316))) %>% filter(abs(x) <= pi)
ggplot(data = data.frame(x = test)) +
  geom_density(aes(x = x))

fitdist(AllPoints.ssf.df$ta_, "wrpcauchy", start = list(mu = 0,
                                                     rho = .9))

test <- data.frame(x = rwrpcauchy(1000 , 0.08464538, 0.26768487)) %>%
  mutate(x = ifelse(x > pi, -2*pi + x, x))
ggplot(data = data.frame(x = test)) +
  geom_density(aes(x = x))


# Estimate size of town polygon and prenesting range to determine appropriate rejection sampling distances
# Load Town Boundaries Shapefile
lapply(c("dplyr", "sf"), require, character.only = TRUE)
townbound <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  group_by(TOWN) %>%
  filter(TArea_KM2 == max(TArea_KM2))
(quantile(townbound$TArea_KM2, .95)/pi)^(1/2) *1000


seasonalUDs <- st_read("E:/Maine Drive/Analysis/Home Range Analysis/seasonalUDs_18_20_polygon.shp") %>%
  filter(Season == "PreNest1") %>%
  filter(level == 0.99)

(quantile(seasonalUDs$Area_km2, .95)/pi)^(1/2) *1000
