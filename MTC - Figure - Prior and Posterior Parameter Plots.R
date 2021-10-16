lapply(c("dplyr", "raster", "sf", "lubridate", "units", "CircStats", 'ggplot2'), require, character.only = TRUE)


prior.df <- data.frame(p = runif(10000, .1, 5),
                       rho = runif(10000, 0.0000000000001, 0.26868487 + (40*0.01281980)),
                       # mu = runif(N.simturk*length(sim.turkey.list), 0.08464538 - (2*0.04287019), 0.08464538 + (2*0.04287019)),
                       k = runif(10000, 0.84834637 - (40*1.744392e-02), 0.84834637 + (40*1.744392e-02)),
                       rate = runif(10000, 0.00311007 - (40*7.517423e-05), 0.00311007 + (40*7.517423e-05))
)

#Load rejection sampling data
rs.results <- read.csv("Rejection Sampling Results.csv")
#Need to know if Nest or Harvest to specify E
disperser.end <- st_read("./GIS/Disperser End.shp") %>%
  dplyr::select(ObsType, OG.ID = ID) %>% st_drop_geometry()
rs.results <- merge(rs.results, disperser.end, by = "OG.ID")

rs.posterior <- data.frame(p = r_p(n = 10000),
                           rho = r_rho(n = 10000),
                           k = r_k(n = 10000),
                           rate = r_rate(n = 10000))


p.plot <- ggplot() +
  geom_density(data = prior.df, aes(x = p), linetype = "solid", lwd = 1.4, color = "black") +
  geom_density(data = rs.results, aes(x = p), linetype = "dashed", lwd = 1.4, color = "red") +
  theme_classic(base_size = 25) +
  labs(x = expression(italic("p"))) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

rho.plot <- ggplot() +
  geom_density(data = prior.df, aes(x = rho), linetype = "solid", lwd = 1.4, color = "black") +
  geom_density(data = rs.results, aes(x = rho), linetype = "dashed", lwd = 1.4, color = "red") +
  theme_classic(base_size = 25) +
  labs(x = expression(italic("rho"))) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

k.plot <- ggplot() +
  geom_density(data = prior.df, aes(x = k), linetype = "solid", lwd = 1.4, color = "black") +
  geom_density(data = rs.results, aes(x = k), linetype = "dashed", lwd = 1.4, color = "red") +
  theme_classic(base_size = 25) +
  labs(x = expression(italic("k"))) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

rate.plot <- ggplot() +
  geom_density(data = prior.df, aes(x = rate), linetype = "solid", lwd = 1.4, color = "black") +
  geom_density(data = rs.results, aes(x = rate), linetype = "dashed", lwd = 1.4, color = "red") +
  theme_classic(base_size = 25) +
  labs(x = expression(italic("rate"))) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

require(patchwork)

combo.plot <- p.plot + rho.plot + k.plot + rate.plot +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 30), plot.tag.position = c(0.05,.95))

ggsave(combo.plot, file = "./Figures/Prior and Posterior Comparison.jpg",
       width = 15, height = 10)
