########################
### Common Data Prep ###
########################
# Create Start and End Location Shapefiles for each analysis
source("./MTC - Start and End Points.R")

# Calculate Necessary Summary Statistics for different analysis
source("./MTC - Summary Stats.R")


#######################################
### SSF Habitat Suitability Surface ###
#######################################
## SSF framework - Thurfjell et al 2014, 
## SSF results to habitat suitability - Keeley et al 2016, Trainor et al 2013

# Create Habitat Suitability Surface from SSF Analysis
source("./MTC - Habitat Suitability Surface.R")

# Load SSF Results
load(file = "./SSFResults.RData")
load(file = "./SSF Global Model.RData")


############################
### Dispersal Propensity ###
############################
# Estimate probability of leaving town boundaries via GLMM
source("./MTC - Dispersal Propensity.R")


#########################################
### IBM Seasonal Movement Simulations ###
#########################################
# Simulation Functions
source("./MTC - Simulation Functions.R")

# Simulate Seasonal Movement Tracks
source("./MTC - Simulate Seasonal Movement.R")



##########################
### Settling Decisions ###
##########################
# Estimate the probability that a turkey will end their movement in a town



##################################
### Final Connectivty Analysis ###
##################################
## Combine dispersal propensity, IBM simulations, and settling decisions to estimate conenctivty across Maine



###############################
### Create Maps and Figures ###
###############################
# Map showing observed movements between start and end locations
source("./MTC - Figure - Band Dispersal Map.R")

# Create maps from simulated movements
source("./MTC - Figure - Simulation Maps.R")





