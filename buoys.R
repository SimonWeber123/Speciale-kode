# Load necessary libraries
install.packages("rootSolve")
library(rootSolve)
library(ggplot2)

# Define the spherical cap volume function
spherical_cap_volume <- function(h, r, V) {
  # Exact spherical cap formula: V = (pi * h^2 / 3) * (3r - h)
  volume <- (pi * h^2 / 3) * (3 * r - h)
  return(volume - V)  # Return the difference (to solve for h)
}

# Define parameters
V1 <- 0.001266
V2 <- 0.001533 
r_values <- seq(0.1, 10, by = 0.01)  # Buoy radii from 0.1 m to 10 m (exclude very small radii)

# Initialize vectors to store results
h1_values <- numeric(length(r_values))  # Draft for 2.726 kg + suction force
h2_values <- numeric(length(r_values))  # Draft for 3.8 kg + suction force
sinkage_values <- numeric(length(r_values))  # Sinkage between the two weights

# Calculate drafts and sinkage for each buoy radius
for (i in 1:length(r_values)) {
  r <- r_values[i]
  
  # Solve for h1 (2.726 kg + suction force)
  result1 <- uniroot(
    spherical_cap_volume,
    interval = c(0, r),  # Search interval for h (0 to r)
    r = r,
    V = V1,
    tol = 1e-8  # Increase precision
  )
  h1_values[i] <- result1$root
  
  # Solve for h2 (3.8 kg + suction force)
  result2 <- uniroot(
    spherical_cap_volume,
    interval = c(0, r),  # Search interval for h (0 to r)
    r = r,
    V = V2,
    tol = 1e-8  # Increase precision
  )
  h2_values[i] <- result2$root
  
  # Calculate sinkage
  sinkage_values[i] <- h2_values[i] - h1_values[i]
}

# Convert drafts and sinkage to millimeters
h1_values_mm <- h1_values * 1000
h2_values_mm <- h2_values * 1000
sinkage_values_mm <- sinkage_values * 1000

# Create a data frame for plotting
data <- data.frame(
  BuoyRadius_m = r_values,
  Draft1_mm = h1_values_mm,
  Draft2_mm = h2_values_mm,
  Sinkage_mm = sinkage_values_mm
)

# Use interpolation to smooth the sinkage curve
smooth_data <- data.frame(
  BuoyRadius_m = seq(0.1, 10, by = 0.01)  # Smaller steps for smoother interpolation
)
smooth_data$Sinkage_mm <- approx(data$BuoyRadius_m, data$Sinkage_mm, xout = smooth_data$BuoyRadius_m)$y

# Plot the sinkage as a function of buoy radius
ggplot(smooth_data, aes(x = BuoyRadius_m, y = Sinkage_mm)) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    labels = seq(0, 10, by = 1),
    expand = c(0, 0)  # No padding on the x-axis
  ) +
  scale_y_continuous(
    limits = c(0, max(smooth_data$Sinkage_mm)),
    breaks = seq(0, max(smooth_data$Sinkage_mm), by = 1),
    expand = c(0, 0)  # No padding on the y-axis
  ) +
  labs(
    title = "Sinkage vs Buoy Size (Accounting for Air)",
    x = "Buoy Radius (m)",
    y = "Sinkage (mm)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


