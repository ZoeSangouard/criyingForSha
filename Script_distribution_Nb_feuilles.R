##import packages
library(dplyr)
library(tidyr)
library(ggplot2)

## Loading Data
data_table <- read.table("modelisation.csv", header=TRUE, sep= ";", dec=",") 


## Filter data for LUZELLE varieties exposed to light treatment C
luzelle_c <- data_table %>%
  filter(Var == "LUZELLE", LT == "C")

# Count number of branches per number of leaves
ramifications_par_feuilles <- luzelle_c %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(luzelle_c$NbLeaves)
sd_val <- sd(luzelle_c$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(luzelle_c$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of ramifications with the same number of leaves, variety Luzelle exposed to light Control (C)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, by = 1), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 25, by = 1), limits = c(0, 25))

## Filter data for LUZELLE varieties exposed to light treatment S
luzelle_s <- data_table %>%
  filter(Var == "LUZELLE", LT == "S")

# Count number of branches per number of leaves
ramifications_par_feuilles <- luzelle_s %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(luzelle_s$NbLeaves)
sd_val <- sd(luzelle_s$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(luzelle_s$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of feuiles, variety Luzelle exposed to light Shadow (S)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20))

## Filter data for ANTANE varieties exposed to light treatment C
antane_c <- data_table %>%
  filter(Var == "ANTANE", LT == "C")

# Count number of branches per number of leaves
ramifications_par_feuilles <- antane_c %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(antane_c$NbLeaves)
sd_val <- sd(antane_c$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(antane_c$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of feuiles, variety Antane exposed to light Control (C)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 30, by = 1), limits = c(0, 30))



## Filter data for ANTANE varieties exposed to light treatment S
antane_s <- data_table %>%
  filter(Var == "ANTANE", LT == "S")

# Count number of branches per number of leaves
ramifications_par_feuilles <- antane_s %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(antane_s$NbLeaves)
sd_val <- sd(antane_s$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(antane_s$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of feuiles, variety Antane exposed to light Shadow (S)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 18, by = 1), limits = c(0, 18)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20))

## Filter data for MARINA varieties exposed to light treatment C
marina_c <- data_table %>%
  filter(Var == "MARINA", LT == "C")

# Count number of branches per number of leaves
ramifications_par_feuilles <- marina_c %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(marina_c$NbLeaves)
sd_val <- sd(marina_c$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(marina_c$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of feuiles, variety Marina exposed to light Control (C)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 18, by = 1), limits = c(0, 18)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 23, by = 1), limits = c(0, 23))


## Filter data for MARINA varieties exposed to light treatment S
marina_s <- data_table %>%
  filter(Var == "MARINA", LT == "S")

# Count number of branches per number of leaves
ramifications_par_feuilles <- marina_s %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(marina_s$NbLeaves)
sd_val <- sd(marina_s$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(marina_s$NbLeaves, "norm")
fit_norm$estimate


# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of feuiles, variety Marina exposed to light Shadow (S)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 20, by = 1), limits = c(0, 20))

## Filter data for BARVINCE varieties exposed to light treatment C
barvince_c <- data_table %>%
  filter(Var == "BARVINCE", LT == "C")

# Calculation of normal distribution parameters
mean_val <- mean(barvince_c$NbLeaves)
sd_val <- sd(barvince_c$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(barvince_c$NbLeaves, "norm")
fit_norm$estimate

# Count number of branches per number of leaves
ramifications_par_feuilles <- barvince_c %>%
  count(NbLeaves)

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of leaves, variety Barvince exposed to light Control (C)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 26, by = 1), limits = c(0, 26)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 22, by = 1), limits = c(0, 22))

## Filter data for BARVINCE varieties exposed to light treatment S
barvince_s <- data_table %>%
  filter(Var == "BARVINCE", LT == "S")

# Count number of branches per number of leaves
ramifications_par_feuilles <- barvince_s %>%
  count(NbLeaves)

# Calculation of normal distribution parameters
mean_val <- mean(barvince_s$NbLeaves)
sd_val <- sd(barvince_s$NbLeaves)

# Display of normal distribution parameters
fit_norm <- fitdist(barvince_s$NbLeaves, "norm")
fit_norm$estimate

# Create a chart
ggplot(ramifications_par_feuilles, aes(x = NbLeaves, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of leaves", y = "Number of ramifications", title = "Number of branches with the same number of leaves, variety Barvince exposed to light Shadow (S)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 16, by = 1), limits = c(0, 16)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 16, by = 1), limits = c(0, 16))



