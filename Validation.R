# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

### Heyder 1977
# Define the variables
eta <- 18.69e-6 # dynamic viscosity [pa.s], 1 pa.s = 1 g/(mm s)
#t <- 1.57 # mean residence time [s]
rp <- c(0.01, 0.03, 0.04, 0.07, 0.1, 0.3, 0.4, 0.5, 0.7, 0.8, 1, 3, 4, 5) / 10^3 # particle radius [micron] -> mm
g <- 9.81 * 1000 # gravitational acceleration [mm/s^2]
rho <- 0.001  # density of particles [g/mm^3]
v <- 2 * rp^2 * rho * g / (9 * eta)  # settling velocity [mm/s]
r <- 8 # tube radius [mm]
L = 1000 #[mm]
a <- pi*r^2 #[mm^2]
u <- 200000/a #[mm/s]
t <- L/u # mean residence time [s]
T <- v * t / (2 * r)  # dimensionless time
v*rp/eta # reynold number



# Define beta values in degrees and convert to radians for calculations
beta_values_deg <- c(0, 38, 60)  # angle in degrees
beta_values_rad <- beta_values_deg * (pi / 180)  # angle in radians

# Initialize data frame
data_combined <- data.frame()

# Loop through each beta value
for (i in 1:length(beta_values_rad)) {
  beta_value_rad <- beta_values_rad[i]
  beta_value_deg <- beta_values_deg[i]
  
  # Initialize temporary data frame
  temp_data <- data.frame()
  
  # Loop through each rp value
  for (j in 1:length(rp)) {
    rp_value <- rp[j]
    v_value <- 2 * rp_value^2 * rho * g / (9 * eta)
    T_value <- T[j]
    kappa <- 3 * T_value * cos(beta_value_rad / 4) # * e
    abbr1 <- sqrt(1 - kappa^(2 / 3))
    DE <- 2 / pi * (2 * kappa * abbr1 - kappa^(1 / 3) * abbr1 + asin(kappa^(1 / 3)))
    PE <- 1 - DE
    temp_data <- rbind(temp_data, data.frame(rp = rp_value * 1e3, kappa = kappa, DE = DE, PE = PE, T = T_value, beta = beta_value_deg))
  }
  
  # Combine temporary data with existing data
  data_combined <- rbind(data_combined, temp_data)
}

# Print the resulting data frame
print(data_combined)


data_combined$study <- 'Heyder 1977'

# Remove rows with NaN or NA values
data_combined <- data_combined[complete.cases(data_combined), ]


## Taulbee & Yu 1975 data
rho <- 0.001
dp <- c(0.01, 0.03, 0.04, 0.07, 0.1, 0.3, 0.4, 0.5, 0.7, 0.8, 1, 3, 4, 5)
mu <- 0.001
tau <- rho * dp^2 / (18 * mu)
c <- 0.001
g <- 9.81 * 1000
da <- 0.154
phi <- 0.69 * c * g * da^2 * tau 

# Simulation result
l <- 0.8
dt <- 0.01
beta <- 1 # Assuming some value for beta since it wasn't defined in your code
sim <- sin(beta) * rho * g * dp^2 * l / (18 * mu) * dt * 200 / 500 / 100

# Create initial data frame
data_yu <- data.frame(
  dp = dp,
  phi = phi,
  sim = sim
)

# Reshape the data to long format and add 'study' column
data_long <- data_yu %>%
  pivot_longer(cols = c(phi, sim), names_to = "study", values_to = "value") %>%
  mutate(study = ifelse(study == "phi", "Taulbee & Yu 1975", "Simulation"))


# Plot the data using ggplot
ggplot(data_combined, aes(x = rp, y = DE, shape =study)) +
  geom_point(size=2)+
  geom_point(data = data_long, aes(x=dp, y = value), size =2) +
  #geom_point(data = data_yu, aes(x=dp, y = sim),shape=4, size = 3) +
  #geom_smooth(se=F) +
  scale_shape_manual(values=c(1, 17, 0))+
  scale_x_log10() + #scale_y_log10() +
  facet_wrap(~ beta, ncol = 1, scales = "free") +  # Facet by T
  labs(
    title = "Sedimentation Deposition Efficiency vs Particle Size by inclined angle",
    x = "Particle Size [micron]",
    y = "Sedimentation Deposition Efficiency"
    #color = "Particle Size (microns)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


