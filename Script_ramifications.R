##import packages
library(dplyr)
library(ggplot2)
library(MASS)

## Loading Data
data_table <- read.table("modelisation.csv", header=TRUE, sep=";", dec=",")

## Filter data for LUZELLE varieties exposed to light treatment C
luzelle_c <- data_table %>%
  filter(Var == "LUZELLE", LT == "C")

# Count number of branches per plant (ID)
ramifications_per_plant <- luzelle_c %>%
  group_by(ID) %>%
  summarise(Num_Ramifications = n())

# Count number of plants per number of ramifications
plants_per_ramifications <- ramifications_per_plant %>%
  group_by(Num_Ramifications) %>%
  summarise(Num_Plants = n())

# Fit a normal distribution to the data
fit_norm <- fitdistr(plants_per_ramifications$Num_Ramifications, "normal")

# Extract mean and standard deviation from the fitted distribution
mean_val <- fit_norm$estimate[1]
sd_val <- fit_norm$estimate[2]

# Display the estimated parameters
cat("Mean:", mean_val, "\n")
cat("Standard Deviation:", sd_val, "\n")


# Create a chart
ggplot(plants_per_ramifications, aes(x = Num_Ramifications, y = Num_Plants)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, LUZELLE variety exposed to light Control (C)")
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, by = 1), limits = c(0, 8))

  
## Filter data for LUZELLE varieties exposed to light treatment S
  luzelle_s <- data_table %>%
    filter(Var == "LUZELLE", LT == "S")
  
# Count number of branches per plant (ID)
  ramifications_per_plant_s <- luzelle_s %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
# Count number of plants per number of ramifications
  plants_per_ramifications_s <- ramifications_per_plant_s %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
# Fit a normal distribution to the data
  fit_norm_s <- fitdistr(plants_per_ramifications_s$Num_Ramifications, "normal")
  
# Extract mean and standard deviation from the fitted distribution
  mean_val_s <- fit_norm_s$estimate[1]
  sd_val_s <- fit_norm_s$estimate[2]
  
# Display the estimated parameters
  cat("Mean:", mean_val_s, "\n")
  cat("Standard Deviation:", sd_val_s, "\n")
  
# Create a chart
  ggplot(plants_per_ramifications_s, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, LUZELLE variety exposed to light Shadow (S)") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5, by = 1), limits = c(0, 5))

  
  ## Filter data for ANTANE varieties exposed to light treatment "C"
  antane_c <- data_table %>%
    filter(Var == "ANTANE", LT == "C")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_ac <- antane_c %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_ac <- ramifications_per_plant_ac %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_ac <- fitdistr(plants_per_ramifications_ac$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_ac <- fit_norm_ac$estimate[1]
  sd_val_ac <- fit_norm_ac$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_ac, "\n")
  cat("Standard Deviation:", sd_val_ac, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_ac, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, ANTANE variety exposed to light 'C'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, by = 1), limits = c(0, 8))
  
  
  ## Filter data for ANTANE varieties exposed to light treatment "S"
  antane_s <- data_table %>%
    filter(Var == "ANTANE", LT == "S")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_as <- antane_s %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_as <- ramifications_per_plant_as %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_as <- fitdistr(plants_per_ramifications_as$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_as <- fit_norm_as$estimate[1]
  sd_val_as <- fit_norm_as$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_as, "\n")
  cat("Standard Deviation:", sd_val_as, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_as, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, ANTANE variety exposed to light 'S'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, by = 1), limits = c(0, 6))
  
  ## Filter data for MARINA varieties exposed to light treatment "C"
  marina_c <- data_table %>%
    filter(Var == "MARINA", LT == "C")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_mc <- marina_c %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_mc <- ramifications_per_plant_mc %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_mc <- fitdistr(plants_per_ramifications_mc$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_mc <- fit_norm_mc$estimate[1]
  sd_val_mc <- fit_norm_mc$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_mc, "\n")
  cat("Standard Deviation:", sd_val_mc, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_mc, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, MARINA variety exposed to light 'C'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4, by = 1), limits = c(0, 4))
  
  ## Filter data for MARINA varieties exposed to light treatment "S"
  marina_s <- data_table %>%
    filter(Var == "MARINA", LT == "S")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_ms <- marina_s %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_ms <- ramifications_per_plant_ms %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_ms <- fitdistr(plants_per_ramifications_ms$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_ms <- fit_norm_ms$estimate[1]
  sd_val_ms <- fit_norm_ms$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_ms, "\n")
  cat("Standard Deviation:", sd_val_ms, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_ms, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, MARINA variety exposed to light 'S'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5, by = 1), limits = c(0, 5))
  
  ## Filter data for BARVINCE varieties exposed to light treatment "C"
  barvince_c <- data_table %>%
    filter(Var == "BARVINCE", LT == "C")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_bc <- barvince_c %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Create a chart
  ggplot(ramifications_per_plant_bc, aes(x = Num_Ramifications)) +
    geom_bar() +
    labs(x = "Number of ramifications per plant", y = "Count", title = "Number of ramifications per plant, BARVINCE variety exposed to light 'C'")
  
  ## Filter data for BARVINCE varieties exposed to light treatment "C"
  barvince_c <- data_table %>%
    filter(Var == "BARVINCE", LT == "C")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_bc <- barvince_c %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_bc <- ramifications_per_plant_bc %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_bc <- fitdistr(plants_per_ramifications_bc$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_bc <- fit_norm_bc$estimate[1]
  sd_val_bc <- fit_norm_bc$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_bc, "\n")
  cat("Standard Deviation:", sd_val_bc, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_bc, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, BARVINCE variety exposed to light 'C'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, by = 1), limits = c(0, 6))
  
  
  ## Filter data for BARVINCE varieties exposed to light treatment "S"
  barvince_s <- data_table %>%
    filter(Var == "BARVINCE", LT == "S")
  
  # Count number of branches per plant (ID)
  ramifications_per_plant_bs <- barvince_s %>%
    group_by(ID) %>%
    summarise(Num_Ramifications = n())
  
  # Count number of plants per number of ramifications
  plants_per_ramifications_bs <- ramifications_per_plant_bs %>%
    group_by(Num_Ramifications) %>%
    summarise(Num_Plants = n())
  
  # Fit a normal distribution to the data
  fit_norm_bs <- fitdistr(plants_per_ramifications_bs$Num_Ramifications, "normal")
  
  # Extract mean and standard deviation from the fitted distribution
  mean_val_bs <- fit_norm_bs$estimate[1]
  sd_val_bs <- fit_norm_bs$estimate[2]
  
  # Display the estimated parameters
  cat("Mean:", mean_val_bs, "\n")
  cat("Standard Deviation:", sd_val_bs, "\n")
  
  # Create a chart
  ggplot(plants_per_ramifications_bs, aes(x = Num_Ramifications, y = Num_Plants)) +
    geom_bar(stat = "identity") +
    labs(x = "Number of ramifications per plant", y = "Number of plants", title = "Number of plants by number of ramifications, BARVINCE variety exposed to light 'S'") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5, by = 1), limits = c(0, 5))
  
  
  