# Import necessary packages
library(dplyr)
library(ggplot2)

# Loading Data
data_table <- read.table("modelisation.csv", header=TRUE, sep=";", dec=",")

# Filter data for LUZELLE varieties exposed to light treatment C
luzelle_c <- data_table %>%
  filter(Var == "LUZELLE", LT == "C")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_luzelle_c <- ggplot(luzelle_c, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var LUZELLE, LT C")

# Fit a linear regression model
linear_model_luzelle_c <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = luzelle_c)

# Get the coefficients of the linear regression model
coefficients_luzelle_c <- summary(linear_model_luzelle_c)$coefficients

# Add the regression line and its equation to the graph
graph_luzelle_c +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = luzelle_c) +
  geom_text(aes(label = paste("y =", round(coefficients_luzelle_c[2, 1], 2), "* x +", round(coefficients_luzelle_c[1, 1], 2))),
            x = min(luzelle_c$NbLeaves), y = max(luzelle_c$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_luzelle_c)$r.squared, 4))),
            x = min(luzelle_c$NbLeaves), y = max(luzelle_c$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)


# Filter data for LUZELLE varieties exposed to light treatment S
luzelle_s <- data_table %>%
  filter(Var == "LUZELLE", LT == "S")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_luzelle_s <- ggplot(luzelle_s, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var LUZELLE, LT S")

# Fit a linear regression model
linear_model_luzelle_s <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = luzelle_s)

# Get the coefficients of the linear regression model
coefficients_luzelle_s <- summary(linear_model_luzelle_s)$coefficients

# Add the regression line and its equation to the graph
graph_luzelle_s +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = luzelle_s) +
  geom_text(aes(label = paste("y =", round(coefficients_luzelle_s[2, 1], 2), "* x +", round(coefficients_luzelle_s[1, 1], 2))),
            x = min(luzelle_s$NbLeaves), y = max(luzelle_s$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_luzelle_s)$r.squared, 4))),
            x = min(luzelle_s$NbLeaves), y = max(luzelle_s$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)


# Filter data for ANTANE varieties exposed to light treatment C
antane_c <- data_table %>%
  filter(Var == "ANTANE", LT == "C")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_antane_c <- ggplot(antane_c, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var ANTANE, LT C")

# Fit a linear regression model
linear_model_antane_c <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = antane_c)

# Get the coefficients of the linear regression model
coefficients_antane_c <- summary(linear_model_antane_c)$coefficients

# Add the regression line and its equation to the graph
graph_antane_c +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = antane_c) +
  geom_text(aes(label = paste("y =", round(coefficients_antane_c[2, 1], 2), "* x +", round(coefficients_antane_c[1, 1], 2))),
            x = min(antane_c$NbLeaves), y = max(antane_c$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_antane_c)$r.squared, 4))),
            x = min(antane_c$NbLeaves), y = max(antane_c$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)



# Filter data for ANTANE varieties exposed to light treatment S
antane_s <- data_table %>%
  filter(Var == "ANTANE", LT == "S")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_antane_s <- ggplot(antane_s, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var ANTANE, LT S")

# Fit a linear regression model
linear_model_antane_s <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = antane_s)

# Get the coefficients of the linear regression model
coefficients_antane_s <- summary(linear_model_antane_s)$coefficients

# Add the regression line and its equation to the graph
graph_antane_s +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = antane_s) +
  geom_text(aes(label = paste("y =", round(coefficients_antane_s[2, 1], 2), "* x +", round(coefficients_antane_s[1, 1], 2))),
            x = min(antane_s$NbLeaves), y = max(antane_s$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_antane_s)$r.squared, 4))),
            x = min(antane_s$NbLeaves), y = max(antane_s$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)



# Filter data for MARINA varieties exposed to light treatment C
marina_c <- data_table %>%
  filter(Var == "MARINA", LT == "C")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_marina_c <- ggplot(marina_c, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var MARINA, LT C")

# Fit a linear regression model
linear_model_marina_c <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = marina_c)

# Get the coefficients of the linear regression model
coefficients_marina_c <- summary(linear_model_marina_c)$coefficients

# Add the regression line and its equation to the graph
graph_marina_c +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = marina_c) +
  geom_text(aes(label = paste("y =", round(coefficients_marina_c[2, 1], 2), "* x +", round(coefficients_marina_c[1, 1], 2))),
            x = min(marina_c$NbLeaves), y = max(marina_c$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_marina_c)$r.squared, 4))),
            x = min(marina_c$NbLeaves), y = max(marina_c$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)


# Filter data for MARINA varieties exposed to light treatment S
marina_s <- data_table %>%
  filter(Var == "MARINA", LT == "S")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_marina_s <- ggplot(marina_s, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var MARINA, LT S")

# Fit a linear regression model
linear_model_marina_s <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = marina_s)

# Get the coefficients of the linear regression model
coefficients_marina_s <- summary(linear_model_marina_s)$coefficients

# Add the regression line and its equation to the graph
graph_marina_s +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = marina_s) +
  geom_text(aes(label = paste("y =", round(coefficients_marina_s[2, 1], 2), "* x +", round(coefficients_marina_s[1, 1], 2))),
            x = min(marina_s$NbLeaves), y = max(marina_s$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_marina_s)$r.squared, 4))),
            x = min(marina_s$NbLeaves), y = max(marina_s$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)

# Filter data for BARVINCE varieties exposed to light treatment C
barvince_c <- data_table %>%
  filter(Var == "BARVINCE", LT == "C")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_barvince_c <- ggplot(barvince_c, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var BARVINCE, LT C")

# Fit a linear regression model
linear_model_barvince_c <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = barvince_c)

# Get the coefficients of the linear regression model
coefficients_barvince_c <- summary(linear_model_barvince_c)$coefficients

# Add the regression line and its equation to the graph
graph_barvince_c +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = barvince_c) +
  geom_text(aes(label = paste("y =", round(coefficients_barvince_c[2, 1], 2), "* x +", round(coefficients_barvince_c[1, 1], 2))),
            x = min(barvince_c$NbLeaves), y = max(barvince_c$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_barvince_c)$r.squared, 4))),
            x = min(barvince_c$NbLeaves), y = max(barvince_c$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)

# Filter data for BARVINCE varieties exposed to light treatment S
barvince_s <- data_table %>%
  filter(Var == "BARVINCE", LT == "S")

# Create a graph of NbLeaves vs MeanInternodeLengthRamif for each ramification
graph_barvince_s <- ggplot(barvince_s, aes(x = NbLeaves, y = MeanInternodeLengthRamif)) +
  geom_point() +
  labs(x = "NbLeaves", y = "MeanInternodeLengthRamif", title = "MeanInternodeLengthRamif according to NbLeaves for Var BARVINCE, LT S")

# Fit a linear regression model
linear_model_barvince_s <- lm(MeanInternodeLengthRamif ~ NbLeaves, data = barvince_s)

# Get the coefficients of the linear regression model
coefficients_barvince_s <- summary(linear_model_barvince_s)$coefficients

# Add the regression line and its equation to the graph
graph_barvince_s +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", data = barvince_s) +
  geom_text(aes(label = paste("y =", round(coefficients_barvince_s[2, 1], 2), "* x +", round(coefficients_barvince_s[1, 1], 2))),
            x = min(barvince_s$NbLeaves), y = max(barvince_s$MeanInternodeLengthRamif),
            color = "blue", hjust = 0, vjust = 1) +
  geom_text(aes(label = paste("R² =", round(summary(linear_model_barvince_s)$r.squared, 4))),
            x = min(barvince_s$NbLeaves), y = max(barvince_s$MeanInternodeLengthRamif) - 0.1,
            color = "blue", hjust = 0, vjust = 1)


