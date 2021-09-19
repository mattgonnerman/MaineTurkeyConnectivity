# install.packages("devtools")
# devtools::install_github("echasnovski/pdqr")

lapply(c("dplyr", "pdqr", "sf"), require, character.only = T)

#Load rejection sampling data
rs.results <- read.csv("Rejection Sampling Results.csv")
#Need to know if Nest or Harvest to specify E
disperser.end <- st_read("./GIS/Disperser End.shp") %>%
  dplyr::select(ObsType, OG.ID = ID) %>% st_drop_geometry()
rs.results <- merge(rs.results, disperser.end, by = "OG.ID")


r_p <- new_r(rs.results$p, type = "continuous")
r_rho <- new_r(rs.results$rho, type = "continuous")
r_k <- new_r(rs.results$k, type = "continuous")
r_rate <- new_r(rs.results$rate, type = "continuous")
r_R <- new_r(rs.results$R, type = "continuous")

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
