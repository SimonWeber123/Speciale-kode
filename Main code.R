install.packages("readxl")  # Install if you haven't already
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")  # Install ggpubr if not already installed
install.packages("ggpattern")
install.packages("knitr")      # For printing nice tables in RMarkdown or console
install.packages("kableExtra") # For enhanced table formatting (especially in RMarkdown or HTML)
library(knitr)
library(ggpubr)
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpattern)

Yes
# Define the file path
file_path <- "C:/Users/Simon pc1/Desktop/Speciale 3d model/Data2aitest.xlsx"  # Update with your actual file path

# Read all sheets
data_1_day <- read_excel(file_path, sheet = "1 dag")
data_3_days <- read_excel(file_path, sheet = "3 dage")
data_7_days <- read_excel(file_path, sheet = "7 dage")
data_pH <- read_excel(file_path, sheet = "pH")
# Convert all numeric columns to numeric type
data_1_day <- data_1_day %>% mutate(across(where(is.character), as.numeric))
data_3_days <- data_3_days %>% mutate(across(where(is.character), as.numeric))
data_7_days <- data_7_days %>% mutate(across(where(is.character), as.numeric))
data_pH <- data_pH %>% mutate(across(where(is.character), as.numeric))

data_1_day <- data_1_day[rowSums(is.na(data_1_day)) != ncol(data_1_day), ]
data_3_days <- data_3_days[rowSums(is.na(data_3_days)) != ncol(data_3_days), ]
data_7_days <- data_7_days[rowSums(is.na(data_7_days)) != ncol(data_7_days), ]
data_pH <- data_pH[rowSums(is.na(data_pH)) != ncol(data_pH), ]

data_1_day <- data_1_day[, colSums(is.na(data_1_day)) < nrow(data_1_day)]
data_3_days <- data_3_days[, colSums(is.na(data_3_days)) < nrow(data_3_days)]
data_7_days <- data_7_days[, colSums(is.na(data_7_days)) < nrow(data_7_days)]
data_pH <- data_pH[, colSums(is.na(data_pH)) < nrow(data_pH)]

data_1_day <- data_1_day %>% slice(1:12)
data_3_days <- data_3_days %>% slice(1:12)
data_7_days <- data_7_days %>% slice(1:12)
data_pH <- data_pH %>% slice(1:9)

data_1_day <- data_1_day %>% select(1:8)
data_3_days <- data_3_days %>% select(1:8)
data_7_days <- data_7_days %>% select(1:8)
data_pH <- data_pH %>% select(1:8)
# List of datasets
data_1_day <- data_1_day %>% mutate(Dag = "1")
data_3_days <- data_3_days %>% mutate(Dag = "3")
data_7_days <- data_7_days %>% mutate(Dag = "7")
data_pH <- data_pH %>% mutate(Dag = "pH")

full_data <- bind_rows(data_1_day, data_3_days, data_7_days, data_pH)



full_data <- full_data %>%
  mutate(`CFH12(g)` = as.numeric(`CFH12(g)`))
full_data <- full_data %>%
  mutate(`PF(g)` = as.numeric(`PF(g)`))



library(dplyr)

# Ensure `CFH12(g)` is numeric
full_data <- full_data %>%
  mutate(`CFH12(g)` = as.numeric(`CFH12(g)`))



# Define the target CFH12(g) values and the ±10% range
target_values <- c(0.02, 0.03, 0.04, 0.05)
tolerance <- 0.20  # ±20%

# Filter the data for each target value
filtered_datacfh12 <- full_data %>%
  filter(
    (`CFH12(g)` >= 0.02 * (1 - tolerance) & `CFH12(g)` <= 0.02 * (1 + tolerance)) |
      (`CFH12(g)` >= 0.03 * (1 - tolerance) & `CFH12(g)` <= 0.03 * (1 + tolerance)) |
      (`CFH12(g)` >= 0.04 * (1 - tolerance) & `CFH12(g)` <= 0.04 * (1 + tolerance)) |
      (`CFH12(g)` >= 0.05 * (1 - tolerance) & `CFH12(g)` <= 0.05 * (1 + tolerance))
  ) %>%
  mutate(g = case_when(
    `CFH12(g)` >= 0.02 * (1 - tolerance) & `CFH12(g)` <= 0.02 * (1 + tolerance) ~ "0.02",
    `CFH12(g)` >= 0.03 * (1 - tolerance) & `CFH12(g)` <= 0.03 * (1 + tolerance) ~ "0.03",
    `CFH12(g)` >= 0.04 * (1 - tolerance) & `CFH12(g)` <= 0.04 * (1 + tolerance) ~ "0.04",
    `CFH12(g)` >= 0.05 * (1 - tolerance) & `CFH12(g)` <= 0.05 * (1 + tolerance) ~ "0.05"
  ))
filtered_datacfh12 <- filtered_datacfh12 %>% select(1:4,9,12,-2,-3)
colnames(filtered_datacfh12)[2] <- "mg/g"

filtered_dataPF <- full_data %>%
  filter(
    (`PF(g)` >= 0.02 * (1 - tolerance) & `PF(g)` <= 0.02 * (1 + tolerance)) |
      (`PF(g)` >= 0.03 * (1 - tolerance) & `PF(g)` <= 0.03 * (1 + tolerance)) |
      (`PF(g)` >= 0.04 * (1 - tolerance) & `PF(g)` <= 0.04 * (1 + tolerance)) |
      (`PF(g)` >= 0.05 * (1 - tolerance) & `PF(g)` <= 0.05 * (1 + tolerance))
  ) %>%
  mutate(g = case_when(
    `PF(g)` >= 0.02 * (1 - tolerance) & `PF(g)` <= 0.02 * (1 + tolerance) ~ "0.02",
    `PF(g)` >= 0.03 * (1 - tolerance) & `PF(g)` <= 0.03 * (1 + tolerance) ~ "0.03",
    `PF(g)` >= 0.04 * (1 - tolerance) & `PF(g)` <= 0.04 * (1 + tolerance) ~ "0.04",
    `PF(g)` >= 0.05 * (1 - tolerance) & `PF(g)` <= 0.05 * (1 + tolerance) ~ "0.05"
  ))
filtered_dataPF <- filtered_dataPF %>% select(5:9,12,-6,-7)
colnames(filtered_dataPF)[2] <- "mg/g"


# Compute mean and standard deviation for CFH12(g)
mean_cfh12 <- filtered_datacfh12 %>%
  group_by(Dag, g) %>%
  summarize(
    mean_value = mean(`mg/g`, na.rm = TRUE),
    sd_value = sd(`mg/g`, na.rm = TRUE)
  )

# Compute mean and standard deviation for PF(g)
mean_pf <- filtered_dataPF %>%
  group_by(Dag, g) %>%
  summarize(
    mean_value = mean(`mg/g`, na.rm = TRUE),
    sd_value = sd(`mg/g`, na.rm = TRUE)
  )
# Ensure Dag is numeric in mean_cfh12
mean_cfh12$Dag <- as.numeric(mean_cfh12$Dag) 

# Ensure Dag is numeric in mean_pf
mean_pf$Dag <- as.numeric(mean_pf$Dag)

# Create new Day 0 data points (ensuring Dag is numeric)
day0_data_cfh12 <- data.frame(
  Dag = as.numeric(0),  # Force numeric type
  mean_value = 0,
  sd_value = 0,
  g = unique(mean_cfh12$g)  # Keep the same g values
)

# Append new rows to mean_cfh12
mean_cfh12 <- bind_rows(mean_cfh12, day0_data_cfh12)


# Create new Day 0 data points
day0_data_pf <- data.frame(
  Dag = as.numeric(0),
  mean_value = 0,
  sd_value = 0,
  g = unique(mean_pf$g)
)

# Append new rows to mean_pf
mean_pf <- bind_rows(mean_pf, day0_data_pf)

#convert from mg to g
mean_cfh12$g <- as.numeric(as.character(mean_cfh12$g)) * 1000
mean_pf$g <- as.numeric(as.character(mean_pf$g)) * 1000
mean_cfh12$Dag <- as.numeric(as.character(mean_cfh12$Dag))
mean_pf$Dag <- as.numeric(as.character(mean_pf$Dag))
max(mean_cfh12$Dag, na.rm = TRUE) + 1
mean_cfh12$g <- paste0(mean_cfh12$g, " mg")
mean_pf$g <- paste0(mean_pf$g, " mg")



Sort/grå figure
# Create a new column that alternates between two widths for every second row
mean_cfh12 <- mean_cfh12 %>%
  mutate(errorbar_width = ifelse(row_number() %% 2 == 0, 0.4, 0.2))  # Wider for even rows

# Plot with alternating error bar widths
ggplot(mean_cfh12, aes(x = as.numeric(Dag), y = mean_value, 
                       color = as.factor(g), linetype = as.factor(g), shape = as.factor(g))) +
  geom_point(size = 3) +  
  geom_line() +  
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, width = errorbar_width)) +  
  scale_color_grey(start = 0.3, end = 0.5) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  
  scale_shape_manual(values = c(16, 17, 18, 15)) +  
  
  # Convert Dag to numeric before using max()
  scale_x_continuous(limits = c(0, max(as.numeric(mean_pf$Dag), na.rm = TRUE) + 1), 
                     breaks = c(0, 1, 3, 7, max(as.numeric(mean_pf$Dag), na.rm = TRUE)), 
                     expand = c(0, 0)) +
  
  # Adjust y-axis to extend to 45
  scale_y_continuous(limits = c(0, 40), 
                     breaks = seq(0, 40, by = 5), 
                     expand = c(0, 0)) +
  
  labs(title = "Mean pf Over Days",  
       x = "Days", 
       y = "P uptake [mg P / g CFH-12]",
       color = "g", linetype = "g", shape = "g") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black", size = 1),  
    axis.ticks = element_line(color = "black", size = 1),  
    plot.margin = margin(0, 0, 0, 0, "cm")  
  )





#Create a new column that alternates between two widths for every second row
mean_pf <- mean_pf %>%
  mutate(errorbar_width = ifelse(row_number() %% 2 == 0, 0.4, 0.2))  # Wider for even rows

# Plot with alternating error bar widths
ggplot(mean_pf, aes(x = as.numeric(Dag), y = mean_value, 
                    color = as.factor(g), linetype = as.factor(g), shape = as.factor(g))) +
  geom_point(size = 3) +  
  geom_line() +  
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, width = errorbar_width)) +  # Use the new column for widths
  scale_color_grey(start = 0.3, end = 0.5) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  
  scale_shape_manual(values = c(16, 17, 18, 15)) +  
  
  # Convert Dag to numeric before using max()
  scale_x_continuous(limits = c(0, max(as.numeric(mean_pf$Dag), na.rm = TRUE) + 1), 
                     breaks = c(0, 1, 3, 7, max(as.numeric(mean_pf$Dag), na.rm = TRUE)), 
                     expand = c(0, 0)) +
  
  # Adjust y-axis to extend to 45
  scale_y_continuous(limits = c(0, 40), 
                     breaks = seq(0, 40, by = 5), 
                     expand = c(0, 0)) +
  
  labs(title = "Mean pf Over Days",  
       x = "Days", 
       y = "P uptake [mg P / g Phosflow]",
       color = "g", linetype = "g", shape = "g") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black", size = 1),  
    axis.ticks = element_line(color = "black", size = 1),  
    plot.margin = margin(0, 0, 0, 0, "cm")  
  )



# Add a column to identify the dataset type
filtered_datacfh12 <- filtered_datacfh12 %>% mutate(Type = "CFH12")
filtered_dataPF <- filtered_dataPF %>% mutate(Type = "PF")

# Combine both datasets
combined_data <- bind_rows(filtered_datacfh12, filtered_dataPF)
filtered_data$g <- as.numeric(as.character(filtered_data$g)) * 1000
filtered_data$g <- paste0(filtered_data$g, " mg")
#combined boxblot
library(dplyr)

# Remove rows where mg/g is 0
filtered_data <- combined_data %>%
  filter(`mg/g` > 0)

ggplot(filtered_data, aes(x = g, y = `mg/g`, fill = as.factor(Dag))) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.75)) +  # Adjust box width for clarity
  facet_wrap(~Type) +  # Separate CFH12 & PF into different panels
  labs(title = "CFH12 and PF mg/g Distribution", 
       x = "", 
       y = "P uptake [mg P / g Substrates]", 
       fill = "Day") +  # Label for color legend
  theme_minimal() +
  scale_fill_manual(values = c("red", "green", "blue")) +  # Assign custom colors for each day
  scale_y_continuous(breaks = seq(0, max(filtered_data$`mg/g`, na.rm = TRUE), by = 2.5)) +  # Set increments
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black", size = 1),  # Restore axis lines
    axis.ticks = element_line(color = "black", size = 1)   # Restore tick marks
  )




library(ggplot2)
library(dplyr)

mean_pf_filtered <- mean_pf %>% filter(mean_value != 0)  # Remove rows where mean_value is 0

ggplot(mean_pf_filtered, aes(x = as.numeric(Dag), y = mean_value)) +
  geom_point(size = 3, color = "black", alpha = 0.8) +  
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  
  stat_cor(method = "pearson", 
           label.x = max(as.numeric(mean_pf_filtered$Dag), na.rm = TRUE) * 0.8,  
           label.y = max(mean_pf_filtered$mean_value, na.rm = TRUE) * 1.05,  
           size = 5) +  
  labs(title = "Mean pf Over Days",  
       x = "Days", 
       y = "Mean mg/g") +  
  theme_minimal()

mean_cfh12_filtered <- mean_cfh12 %>% filter(mean_value != 0)  # Remove rows where mean_value is 0

ggplot(mean_cfh12_filtered, aes(x = as.numeric(Dag), y = mean_value)) +
  geom_point(size = 3, color = "black", alpha = 0.8) +  # Blue scatter points
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # Removed shade by setting se = FALSE
  stat_cor(method = "pearson", 
           label.x = max(as.numeric(mean_pf$Dag), na.rm = TRUE) * 0.8,  # Ensure numeric
           label.y = max(mean_pf$mean_value, na.rm = TRUE) * 1.05,  # Ensure numeric
           size = 5) +  
  labs(title = "Mean cfh12 Over Days",  # Removed "With Confidence Interval"
       x = "Days", 
       y = "Mean mg/g") +  
  theme_minimal()


# Compute IQR boundaries for each group (g, Dag, Type)
iqr_data <- combined_data %>%
  group_by(g, Dag, Type) %>%
  mutate(
    Q1 = quantile(`mg/g`, 0.25, na.rm = TRUE),
    Q3 = quantile(`mg/g`, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(`mg/g` < Lower_Bound | `mg/g` > Upper_Bound, "Outlier", "Within IQR")  # Flag outliers
  ) %>%
  ungroup()

#PH
library(dplyr)
library(tidyr)
# Select only the relevant columns and rename them for clarity
# Compute average mg/g and standard deviation for each pH level
library(dplyr)
library(ggplot2)
library(tidyr)

# Select only the relevant columns and rename them for clarity
cleaned_pH <- data_pH %>%
  select(CFH12_pH = 1, CFH12_mg_g = 4, PF_pH = 5, PF_mg_g = 8) %>%
  pivot_longer(cols = c(CFH12_pH, PF_pH), names_to = "Material", values_to = "pH") %>%
  mutate(mg_g = ifelse(Material == "CFH12_pH", CFH12_mg_g, PF_mg_g),  # Pair mg/g correctly
         Material = ifelse(Material == "CFH12_pH", "CFH12", "PF")) %>%
  select(Material, pH, mg_g) %>%
  filter(!is.na(pH) & !is.na(mg_g))  # Remove any missing values# Calculate the mean and standard deviation
mean_pH <- cleaned_pH %>%
  group_by(Material, pH) %>%
  summarise(mean_mg_g = mean(mg_g, na.rm = TRUE), 
            sd_mg_g = sd(mg_g, na.rm = TRUE), .groups = "drop")

ggplot(mean_pH, aes(x = as.factor(pH), y = mean_mg_g, fill = Material, pattern = Material)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(0.8), width = 0.7, 
                   pattern_density = 0.1,  # Very low density (fewer lines)
                   pattern_spacing = 0.08,  # Wider spacing (less aggressive)
                   pattern_fill = "white",  
                   pattern_colour = "black",  
                   pattern_alpha = 0.3,  # Very light transparency for soft effect
                   pattern_size = 0.3) +  # Thinner lines in the pattern
  
  geom_errorbar(aes(ymin = mean_mg_g - sd_mg_g, ymax = mean_mg_g + sd_mg_g), 
                width = 0.2, position = position_dodge(0.8), size = 0.8) +  
  
  # Assign even more subtle patterns
  scale_pattern_manual(values = c("CFH12" = "crosshatch", "PF" = "stripe")) +
  
  # Use lighter greys for an even softer look
  scale_fill_manual(values = c("CFH12" = "grey60", "PF" = "grey85")) +  
  
  labs(
    title = "Phosphorus Uptake in CFH12 & PF at Different pH Levels",
    x = expression("pH"),
    y = expression("P Uptake (mg P / g Substrate)"),
    fill = "Substrate",
    pattern = "Substrate"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1.2),  
    axis.ticks = element_line(color = "black", size = 1),  
    axis.text = element_text(size = 14, color = "black"),  
    axis.title = element_text(size = 16, face = "bold"),  
    legend.position = "top",  
    legend.text = element_text(size = 14),  
    legend.title = element_text(size = 15, face = "bold"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  
  )
cancel




library(dplyr)
library(ggpubr)

# Add 'Type' column to each dataset
filtered_datacfh12 <- filtered_datacfh12 %>% mutate(Type = "CFH12")
filtered_dataPF <- filtered_dataPF %>% mutate(Type = "PF")

# Combine both datasets into one
anova_data <- bind_rows(filtered_datacfh12, filtered_dataPF)

# Ensure 'Type' is a factor
anova_data <- anova_data %>% mutate(Type = as.factor(Type))

# Run One-Way ANOVA (Testing CFH12 vs. PF)
anova_oneway <- aov(`mg/g` ~ Type, data = anova_data)

# Display results
summary(anova_oneway)
# Ensure 'Dag' (Day) is a factor
anova_data <- anova_data %>% mutate(Dag = as.factor(Dag))

# Separate datasets for CFH12 and PF
cfh12_data <- anova_data %>% filter(Type == "CFH12")
pf_data <- anova_data %>% filter(Type == "PF")

#dag
anova_cfh12 <- aov(`mg/g` ~ Dag, data = cfh12_data)
summary(anova_cfh12)
TukeyHSD(anova_cfh12)

#dag
anova_pf <- aov(`mg/g` ~ Dag, data = pf_data)
summary(anova_pf)
TukeyHSD(anova_pf)

 #weight
anova_cfh12 <- aov(`mg/g` ~ g, data = cfh12_data)
summary(anova_cfh12)
TukeyHSD(anova_cfh12)

#weight
anova_pf <- aov(`mg/g` ~ g, data = pf_data)
summary(anova_pf)
TukeyHSD(anova_pf)


# Compute mean and standard deviation for each weight and day
summary_stats <- anova_data %>%
  group_by(Type, Dag, g) %>%
  summarise(
    mean_mgp_g = mean(`mg/g`, na.rm = TRUE),
    sd_mgp_g = sd(`mg/g`, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary statistics
print(summary_stats)


library(dplyr)

# Assuming dataset has Fe, Al, and P concentrations in mg/L or mg/kg
data_ratios <- anova_data %>%
  mutate(
    Fe_P_mass = Fe / P,  # Fe:P mass ratio
    Al_P_mass = Al / P,  # Al:P mass ratio
    
    Fe_P_molar = (Fe / 55.85) / (P / 30.97),  # Fe:P molar ratio
    Al_P_molar = (Al / 26.98) / (P / 30.97)   # Al:P molar ratio
  )

# Print first few rows to check calculations
head(data_ratios)



