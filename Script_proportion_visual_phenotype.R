##import packages
library(dplyr)
library(tidyr)
library(ggplot2)

## Load data
data_table <- read.table("modelisation.csv", header=TRUE, sep= ";", dec=",") 

###By light treatment, no matter the variety

##Calcul of proportions

# Filter data for treatment "C"
data_C <- data_table %>%
  filter(LT == "C")

# Select unique rows for each plant ID
unique_plants_C <- data_C %>%
  distinct(ID, VisualPhenotype)

# Calculate proportions for each type of VisualPhenotype for "C" treatment
proportions_C <- unique_plants_C %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_C))

# Filter data for "S" processing
data_S <- data_table %>%
  filter(LT == "S")

# Select unique rows for each plant ID
unique_plants_S <- data_S %>%
  distinct(ID, VisualPhenotype)

# Calculate proportions for each type of VisualPhenotype for "S" treatment
proportions_S <- unique_plants_S %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_S))

# Show proportions for "S" and "C" treatment
print(proportions_S)
print(proportions_C)


# Create a dataframe with the proportions for each type of VisualPhenotype and light treatment
data_plot <- rbind(
  cbind(proportions_C, LT = "C"),
  cbind(proportions_S, LT = "S")
)

# Convert proportion column to numeric for graph
data_plot$proportion <- as.numeric(data_plot$proportion)


# Create a chart using ggplot2
ggplot(data = data_plot, aes(x = VisualPhenotype, y = proportion, fill = LT)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "black")) +
  labs(x = "Visual Phenotype", y = "Proportion", title = "Proportion of visual phenotypes (Elongated = E, Bushy = B and Mixed = M) according to light treatment") +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.05), limits = c(0,0.8)) 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.05))

###By variety, for light treatment "C"

## Filter data for LUZELLE varieties exposed to light treatment C
luzelle_C <- data_table %>%
  filter(Var == "LUZELLE", LT == "C")
  
# Select unique rows for each plant ID
unique_plants_luzelle_C <- luzelle_C %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for luzelle exposed to "C" treatment
proportions_luzelle_C <- unique_plants_luzelle_C %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_luzelle_C))  
  
## Filter data for Antane varieties exposed to light treatment C
antane_C <- data_table %>%
  filter(Var == "ANTANE", LT == "C")
  
# Select unique rows for each plant ID
unique_plants_antane_C <- antane_C %>%
  distinct(ID, VisualPhenotype)

# Calculate proportions for each type of VisualPhenotype for Antane exposed to "C" treatment
proportions_antane_C <- unique_plants_antane_C %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_antane_C))
  
## Filter data for MARINA variety exposed to light treatment C
marina_C <- data_table %>%
  filter(Var == "MARINA", LT == "C")
  
# Select unique rows for each plant ID
unique_plants_marina_C <- marina_C %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for MARINA exposed to "C" treatment
proportions_marina_C <- unique_plants_marina_C %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_marina_C))
  

## Filter data for BARVINCE variety exposed to light treatment C
barvince_C <- data_table %>%
  filter(Var == "BARVINCE", LT == "C")
  
# Select unique rows for each plant ID
unique_plants_barvince_C <- barvince_C %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for BARVINCE exposed to "C" treatment
proportions_barvince_C <- unique_plants_barvince_C %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_barvince_C))
  
# Show proportions for "C"
print(proportions_luzelle_C)
print(proportions_antane_C)
print(proportions_marina_C)
print(proportions_barvince_C) 

# Create a dataframe with the proportions for each type of VisualPhenotype for each variety under C
data_plot <- rbind(
  cbind(proportions_luzelle_C, Var = "LUZELLE"),
  cbind(proportions_antane_C, Var = "ANTANE"),
  cbind(proportions_marina_C, Var = "MARINA"),
  cbind(proportions_barvince_C, Var = "BARVINCE")
)

# Convert proportion column to numeric for graph
data_plot$proportion <- as.numeric(data_plot$proportion)

# Create a chart using ggplot2
ggplot(data = data_plot, aes(x = VisualPhenotype, y = proportion, fill = Var)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Visual Phenotype", y = "Proportion", title = "Proportion of Visual Phenotypes for each variety under light treatment C") +
  theme_minimal()

  
###By variety, for light treatment "S"

## Filter data for LUZELLE varieties exposed to light treatment S
luzelle_S <- data_table %>%
  filter(Var == "LUZELLE", LT == "S")
  
# Select unique rows for each plant ID
unique_plants_luzelle_S <- luzelle_S %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for luzelle exposed to "S" treatment
proportions_luzelle_S <- unique_plants_luzelle_S %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_luzelle_S))  

## Filter data for Antane varieties exposed to light treatment S
antane_S <- data_table %>%
  filter(Var == "ANTANE", LT == "S")
  
# Select unique rows for each plant ID
unique_plants_antane_S <- antane_S %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for Antane exposed to "S" treatment
proportions_antane_S <- unique_plants_antane_S %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_antane_S))
  
## Filter data for MARINA variety exposed to light treatment S
marina_S <- data_table %>%
  filter(Var == "MARINA", LT == "S")
  
# Select unique rows for each plant ID
unique_plants_marina_S <- marina_S %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for MARINA exposed to "S" treatment
proportions_marina_S <- unique_plants_marina_S %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_marina_S))
  
## Filter data for BARVINCE variety exposed to light treatment S
barvince_S <- data_table %>%
  filter(Var == "BARVINCE", LT == "S")
  
# Select unique rows for each plant ID
unique_plants_barvince_S <- barvince_S %>%
  distinct(ID, VisualPhenotype)
  
# Calculate proportions for each type of VisualPhenotype for BARVINCE exposed to "S" treatment
proportions_barvince_S <- unique_plants_barvince_S %>%
  group_by(VisualPhenotype) %>%
  summarise(proportion = n() / nrow(unique_plants_barvince_S))
  
# Show proportions for "S" 
print(proportions_luzelle_S)
print(proportions_antane_S)
print(proportions_marina_S)
print(proportions_barvince_S) 

# Create a dataframe with the proportions for each type of VisualPhenotype for each variety under S
data_plot <- rbind(
  cbind(proportions_luzelle_S, Var = "LUZELLE"),
  cbind(proportions_antane_S, Var = "ANTANE"),
  cbind(proportions_marina_S, Var = "MARINA"),
  cbind(proportions_barvince_S, Var = "BARVINCE")
)

# Convert proportion column to numeric for graph
data_plot$proportion <- as.numeric(data_plot$proportion)

# Create a chart using ggplot2
ggplot(data = data_plot, aes(x = VisualPhenotype, y = proportion, fill = Var)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Visual Phenotype", y = "Proportion", title = "Proportion of Visual Phenotypes for each variety under light treatment S") +
  theme_minimal()
