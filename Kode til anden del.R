install.packages("readxl")  # Install if you haven't already

install.packages("dplyr")  # Install if not already installed
install.packages("purrr")
install.packages("ggplot2")
install.packages(c("ggplot2", "tibble", "rlang", "scales"))
install.packages("ggplot2", repos = "http://cran.rstudio.com/")
install.packages(c("gtable", "lifecycle", "vctrs"), dependencies = TRUE)
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggpubr")  # Install ggpubr if not already installed
install.packages("ggpattern")
library(ggpubr)  # Load ggpubr for statistical annotations
install.packages("rlang")
library(dplyr)  # Load dplyr# Load necessary package
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpattern)

Yes
# Define the file path
file_path <- "C:/Users/Simon pc1/Desktop/Speciale 3d model/Data2aitest.xlsx"  # Update with your actual file path

# Read all sheets
data_1_day <- read_excel(file_path, sheet = "1 dag (2)")
data_3_days <- read_excel(file_path, sheet = "3 dage (2)")
data_7_days <- read_excel(file_path, sheet = "7 dage (2)")
# Convert all numeric columns to numeric type
data_1_day <- data_1_day %>% mutate(across(where(is.character), as.numeric))
data_3_days <- data_3_days %>% mutate(across(where(is.character), as.numeric))
data_7_days <- data_7_days %>% mutate(across(where(is.character), as.numeric))

data_1_day <- data_1_day[rowSums(is.na(data_1_day)) != ncol(data_1_day), ]
data_3_days <- data_3_days[rowSums(is.na(data_3_days)) != ncol(data_3_days), ]
data_7_days <- data_7_days[rowSums(is.na(data_7_days)) != ncol(data_7_days), ]

data_1_day <- data_1_day[, colSums(is.na(data_1_day)) < nrow(data_1_day)]
data_3_days <- data_3_days[, colSums(is.na(data_3_days)) < nrow(data_3_days)]
data_7_days <- data_7_days[, colSums(is.na(data_7_days)) < nrow(data_7_days)]
data_1_day <- data_1_day %>% slice(1:4)
data_3_days <- data_3_days %>% slice(1:4)
data_7_days <- data_7_days %>% slice(1:7)

data_1_day <- data_1_day %>% select(1:8)
data_3_days <- data_3_days %>% select(1:8)
data_7_days <- data_7_days %>% select(1:8)

# List of datasets
data_1_day <- data_1_day %>% mutate(Dag = "1")
data_3_days <- data_3_days %>% mutate(Dag = "3")
data_7_days <- data_7_days %>% mutate(Dag = "7")

full_data <- bind_rows(data_1_day, data_3_days, data_7_days)



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
    (`CFH12(g)` >= 1* (1 - tolerance) & `CFH12(g)` <= 1 * (1 + tolerance)) |
      (`CFH12(g)` >= 1 * (1 - tolerance) & `CFH12(g)` <= 1 * (1 + tolerance)) |
      (`CFH12(g)` >= 1 * (1 - tolerance) & `CFH12(g)` <= 1 * (1 + tolerance)) |
      (`CFH12(g)` >= 1 * (1 - tolerance) & `CFH12(g)` <= 1 * (1 + tolerance))
  ) %>%
  mutate(g = case_when(
    `CFH12(g)` >= 1 * (1 - tolerance) & `CFH12(g)` <= 1 * (1 + tolerance) ~ "1",
  ))
filtered_datacfh12 <- filtered_datacfh12 %>% select(1:4,9, -2,-3)
colnames(filtered_datacfh12)[2] <- "p"

filtered_dataPF <- full_data %>%
  filter(
    (`PF(g)` >= 1 * (1 - tolerance) & `PF(g)` <= 1 * (1 + tolerance))
  ) %>%
  mutate(g = case_when(
    `PF(g)` >= 1 * (1 - tolerance) & `PF(g)` <= 1 * (1 + tolerance) ~ "1",
  ))
filtered_dataPF <- filtered_dataPF %>% select(5,8,9)
colnames(filtered_dataPF)[2] <- "p"


# Compute mean and standard deviation for CFH12(g)
mean_cfh12 <- filtered_datacfh12 %>%
  group_by(Dag) %>%
  summarize(
    mean_value = mean(`p`, na.rm = TRUE),
    sd_value = sd(`p`, na.rm = TRUE)
  )

# Compute mean and standard deviation for PF(g)
mean_pf <- filtered_dataPF %>%
  group_by(Dag) %>%
  summarize(
    mean_value = mean(`p`, na.rm = TRUE),
    sd_value = sd(`p`, na.rm = TRUE)
  )
# Ensure Dag is numeric in mean_cfh12
mean_cfh12$Dag <- as.numeric(mean_cfh12$Dag) 

# Ensure Dag is numeric in mean_pf
mean_pf$Dag <- as.numeric(mean_pf$Dag)

# Create new Day 0 data points (ensuring Dag is numeric)
day0_data_cfh12 <- data.frame(
  Dag = as.numeric(0),  # Force numeric type
  mean_value = 0,
  sd_value = 0
)

# Append new rows to mean_cfh12
mean_cfh12 <- bind_rows(mean_cfh12, day0_data_cfh12)


# Create new Day 0 data points
day0_data_pf <- data.frame(
  Dag = as.numeric(0),
  mean_value = 0,
  sd_value = 0
)

# Append new rows to mean_pf
mean_pf <- bind_rows(mean_pf, day0_data_pf)

# Add a column to identify the dataset source
mean_pf <- mean_pf %>% mutate(Type = "PF")
mean_cfh12 <- mean_cfh12 %>% mutate(Type = "CFH12")

# Combine both datasets
mean_data <- bind_rows(mean_pf, mean_cfh12)

# Convert Days to factor for better visualization
mean_data$Dag <- as.numeric(as.character(mean_data$Dag))

# The plot
ggplot(mean_data, aes(x = Dag, y = mean_value, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 3) +  # Plot points
  geom_line(size = 1.2) +  # Connect points with lines
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +  
  
  # Custom colors for each Type
  scale_color_manual(values = c("CFH12" = "black", "PF" = "grey40")) +
  
  # Custom line types
  scale_linetype_manual(values = c("CFH12" = "solid", "PF" = "dashed")) +
  
  # Custom point shapes
  scale_shape_manual(values = c("CFH12" = 16, "PF" = 17)) +
  
  # X-axis: specific ticks and zero expansion
  scale_x_continuous(
    breaks = c(0, 1, 3, 7),
    limits = c(0, 7.2),
    expand = c(0, 0)
  ) +
  
  # ✅ Y-axis: 0 to 100, no padding, no clipping
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    expand = c(0, 0)
  ) +
  
  # Labels & titles
  labs(
    title = "Phosphorus Absorption Over Time",
    x = "Days",
    y = "% P Absorbed",
    color = "Substrate",
    shape = "Substrate",
    linetype = "Substrate"
  ) +
  
  # Clean minimal theme with forced axis lines
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),  # Reset base axis.line
    axis.line.x.bottom = element_line(color = "black", size = 1.2),
    axis.line.y.left = element_line(color = "black", size = 1.2),
    axis.ticks = element_line(color = "black", size = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )


library(dplyr)
library(tidyr)

# Filter out Day 0
mean_data_filtered <- mean_data %>%
  filter(Dag %in% c(1, 3, 7)) %>%
  mutate(Dag = as.factor(Dag))

# Reshape to wide format
paired_data <- mean_data_filtered %>%
  pivot_wider(names_from = Type, values_from = mean_value) %>%
  rename(PF = `PF`, CFH12 = `CFH12`)

# Remove rows where either PF or CFH12 is missing
paired_data <- paired_data %>% drop_na(PF, CFH12)

# Run paired t-test only if enough data exists
t_test_results <- paired_data %>%
  group_by(Dag) %>%
  filter(n() >= 2) %>%  # Ensure at least 2 pairs exist
  summarise(
    t_test = list(t.test(PF, CFH12, paired = TRUE))
  )
library(dplyr)

# Ensure 'Type' column is added properly
filtered_datacfh12 <- filtered_datacfh12 %>% 
  mutate(Type = "CFH12") %>%   # Assign "CFH12" to this dataset
  filter(Dag %in% c(1, 3, 7))  # Exclude Day 0

filtered_datapf <- filtered_dataPF %>% 
  mutate(Type = "PF") %>%   # Assign "PF" to this dataset
  filter(Dag %in% c(1, 3, 7))  # Exclude Day 0

# Combine both datasets into one
anova_data <- bind_rows(filtered_datacfh12, filtered_datapf)

# Run One-Way ANOVA for each day
anova_results <- anova_data %>%
  group_by(Dag) %>%
  summarise(
    anova = list(aov(p ~ Type, data = cur_data()))
  )

# Print ANOVA summaries for each day
anova_results$anova %>% lapply(summary)

anova_summary <- anova_results %>%
  mutate(
    p_value = sapply(anova, function(x) summary(x)[[1]]["Type", "Pr(>F)"]),
    F_statistic = sapply(anova, function(x) summary(x)[[1]]["Type", "F value"]),
    mean_PF = sapply(anova, function(x) mean(anova_data$p[anova_data$Type == "PF" & anova_data$Dag == unique(anova_data$Dag)], na.rm = TRUE)),
    mean_CFH12 = sapply(anova, function(x) mean(anova_data$p[anova_data$Type == "CFH12" & anova_data$Dag == unique(anova_data$Dag)], na.rm = TRUE))
  )

# Print summarized results
print(anova_summary)
anova_results %>%
  mutate(
    tukey = lapply(anova, TukeyHSD)
  ) %>%
  pull(tukey)


#### DETTE UNDER BLIVER IKKE BRUGT, MEN HAR VÆRET UNDER OVERVEJELSE TIL AT TESTE PFO OG PSO

# Estimate qe from Day 7 value
qe_pf <- 100 
# Prepare dataset and remove invalid points
pf_first_order_data <- mean_pf %>%
  filter(Dag > 0) %>%
  mutate(
    qt = mean_value,
    diff = qe_pf - qt
  ) %>%
  filter(diff > 0) %>%  # Keep only positive differences
  mutate(
    log_diff = log10(diff)
  )

# Check data
print(pf_first_order_data)

# Fit model
pf_first_order_fit <- lm(log_diff ~ Dag, data = pf_first_order_data)

# Extract slope and compute k1
slope <- coef(pf_first_order_fit)[2]
k1 <- -slope * 2.303

# Output
cat("Pseudo-First-Order Model (PF)\n")
cat("Estimated q_e:", round(qe_pf, 2), "\n")
cat("Rate constant k1:", signif(k1, 3), "\n")
cat("R-squared:", round(summary(pf_first_order_fit)$r.squared, 3), "\n")

library(ggplot2)

ggplot(pf_first_order_data, aes(x = Dag, y = log_diff)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Pseudo-First-Order Kinetics (PF)",
       x = "Time (days)",
       y = "log(qe - qt)") +
  theme_minimal()

qe_cfh12 <- 100

# Prepare dataset and remove invalid points
cfh12_first_order_data <- mean_cfh12 %>%
  filter(Dag > 0) %>%
  mutate(
    qt = mean_value,
    diff = qe_cfh12 - qt
  ) %>%
  filter(diff > 0) %>%  # Keep only positive differences
  mutate(
    log_diff = log10(diff)
  )

# Check data
print(cfh12_first_order_data)

# Fit model
cfh12_first_order_fit <- lm(log_diff ~ Dag, data = cfh12_first_order_data)

# Extract slope and compute k1
slope <- coef(cfh12_first_order_fit)[2]
k1 <- -slope * 2.303

# Output
cat("Pseudo-First-Order Model (PF)\n")
cat("Estimated q_e:", round(qe_cfh12, 2), "\n")
cat("Rate constant k1:", signif(k1, 3), "\n")
cat("R-squared:", round(summary(cfh12_first_order_fit)$r.squared, 3), "\n")

library(ggplot2)

ggplot(cfh12_first_order_data, aes(x = Dag, y = log_diff)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Pseudo-First-Order Kinetics (PF)",
       x = "Time (days)",
       y = "log(qe - qt)") +
  theme_minimal()


# Filter PF data and remove Day 0
pf_lin_data <- mean_pf %>% filter(Dag > 0)

# Compute t/qt
pf_lin_data <- pf_lin_data %>%
  mutate(t = Dag,
         qt = mean_value,
         t_qt = t / qt)

# Perform linear regression: t/qt ~ t
pseudo2_linear_pf <- lm(t_qt ~ t, data = pf_lin_data)

# Extract slope and intercept
slope <- coef(pseudo2_linear_pf)[2]
intercept <- coef(pseudo2_linear_pf)[1]

# Compute q_e and k_2
q_e <- 1 / slope
k_2 <- 1 / (intercept * q_e^2)

# Output
cat("Linear Pseudo-Second-Order Model (PF)\n")
cat("q_e =", round(q_e, 2), "\n")
cat("k_2 =", signif(k_2, 3), "\n")
cat("R-squared =", round(summary(pseudo2_linear_pf)$r.squared, 3), "\n")


n <- nrow(pf_data_nls)
p <- 2  # parameters: q_e and k

Fe_pso <- sqrt(sum((pf_data_nls$mean_value - pf_data_nls$predicted)^2) / (n - p))
cat("Pseudo-Second-Order Fit Error (Fe):", round(Fe_pso, 3), "\n")


Fe_pfo <- sqrt(sum((pf_first_order_data$qt - pf_first_order_data$qt_predicted)^2) / (n - p))
cat("Pseudo-First-Order Fit Error (Fe):", round(Fe_pfo, 3), "\n")

