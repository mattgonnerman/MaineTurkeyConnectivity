# install.packages("devtools")
# devtools::install_github("echasnovski/pdqr")

lapply(c("dplyr", "pdqr", "sf"), require, character.only = T)

#Load rejection sampling data
rs.results <- read.csv("Rejection Sampling Results.csv")
#Need to know if Nest or Harvest to specify E
disperser.end <- st_read("./GIS/Disperser End.shp") %>%
  dplyr::select(ObsType, OG.ID = ID) %>% st_drop_geometry()
rs.results <- merge(rs.results, disperser.end, by = "OG.ID")
rs.results.M <- rs.results %>% filter(ObsType == "H")
rs.results.F <- rs.results %>% filter(ObsType == "N")

### SINGLE DISTRIBUTION FOR BOTH SEXES
# r_p <- new_r(rs.results$p, type = "continuous")
# r_rho <- new_r(rs.results$rho, type = "continuous")
# r_k <- new_r(rs.results$k, type = "continuous")
# r_rate <- new_r(rs.results$rate, type = "continuous")
# r_R <- new_r(rs.results$R, type = "continuous")


my_d_con <- new_d(rs.results.M$p, "continuous")
head(meta_x_tbl(my_d_con))
plot(my_d_con)


### SPLIT DISTRIBUTIONS FOR EACH SEX
#Males
r_p_M <- new_r(rs.results.M$p, type = "continuous")
r_rho_M <- new_r(rs.results.M$rho, type = "continuous")
r_k_M <- new_r(rs.results.M$k, type = "continuous")
r_rate_M <- new_r(rs.results.M$rate, type = "continuous")
r_R_M <- new_r(rs.results.M$R, type = "continuous")
#Females
r_p_F <- new_r(rs.results.F$p, type = "continuous")
r_rho_F <- new_r(rs.results.F$rho, type = "continuous")
r_k_F <- new_r(rs.results.F$k, type = "continuous")
r_rate_F <- new_r(rs.results.F$rate, type = "continuous")
r_R_F <- new_r(rs.results.F$R, type = "continuous")

# rs.prior <- data.frame(p = runif(10000, .1, 5),
#                    rho = runif(10000, 0.0000000000001, 0.26868487 + (20*0.01281980)),
#                    k = runif(10000, 0.84834637 - (20*1.744392e-02), 0.84834637 + (20*1.744392e-02)),
#                    rate = runif(10000, 0.00311007 - (20*7.517423e-05), 0.00311007 + (20*7.517423e-05))) %>% 
#   mutate(R = qgamma(.95, shape = k, scale = 1/rate)) 
# 
# rs.posterior <- data.frame(p = r_p(n = 10000),
#                 rho = r_rho(n = 10000),
#                 k = r_k(n = 10000),
#                 rate = r_rate(n = 10000),
#                 R = r_R(n = 10000))
# 
# 
# 
# ggplot() +
#   geom_density(data = rs.prior, aes(x = p), linetype = "dashed") +
#   geom_density(data = rs.results, aes(x = p, color = ObsType)) +
#   labs(title = "Habitat Specialization")
# 
# ggplot() +
#   geom_density(data = rs.prior, aes(x = rho), linetype = "dashed") +
#   geom_density(data = rs.results, aes(x = rho, color = ObsType)) +
#   labs(title = "Turning Angle - Wrapped Cauchy Rho")
# 
# ggplot() +
#   geom_density(data = rs.prior, aes(x = k), linetype = "dashed") +
#   geom_density(data = rs.results, aes(x = k, color = ObsType)) +
#   labs(title = "Step Length - Gamma K")
# 
# ggplot() +
#   geom_density(data = rs.prior, aes(x = rate), linetype = "dashed") +
#   geom_density(data = rs.results, aes(x = rate, color = ObsType)) +
#   labs(title = "Step Length - Gamma Rate")
