# Load necessary library
library(ggplot2)

# Constants (divide total weight by 4 discs)
V1 <- 0.001266
V2 <- 0.001533 

# Define disc diameters (meters)
disc_diameters_m <- seq(0.5, 10, by = 0.1)  # From 0.5m to 10m
disc_radii_m <- disc_diameters_m / 2  # Convert to radius

# Calculate sinkage depth (h) in meters for both cases
sinkage_dry_m <- V1 / (pi * disc_radii_m^2)
sinkage_wet_m <- V2 / (pi * disc_radii_m^2)

# Convert sinkage to mm
sinkage_dry_mm <- sinkage_dry_m * 1000  
sinkage_wet_mm <- sinkage_wet_m * 1000  

# Calculate sinkage difference (Δh in mm)
sinkage_diff_mm <- sinkage_wet_mm - sinkage_dry_mm

# Create a data frame
data <- data.frame(Disc_Diameter_m = disc_diameters_m, Sinkage_Difference_mm = sinkage_diff_mm)

# Plot the results
ggplot(data, aes(x = Disc_Diameter_m, y = Sinkage_Difference_mm)) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(
    limits = c(0, max(data$Disc_Diameter_m, na.rm = TRUE)),
    breaks = seq(0, max(data$Disc_Diameter_m, na.rm = TRUE), by = 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(data$Sinkage_Difference_mm, na.rm = TRUE)),
    expand = c(0, 0)
  ) +
  labs(
    title = "Sinkage Difference (3.8kg - 2.8kg) vs. Diameter (4 Discs)",
    x = "Disc Diameter (m)",
    y = "Sinkage (mm)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(size = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )


