# Importing packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Importing CSV files for PCA
NotTreated <- read.table("Modelisation.csv", header = TRUE, sep = ";", dec = ",")

# Importing biomass values
Biomasses <- read.table("BiomasseFin.csv", header = TRUE, sep = ";", dec = ",")

# Importing LAI data for PCA
LAI_ACP <- read.table("LAI_ACP.csv", header = TRUE, sep = ";", dec = ",")

# Using group_by to group the data based on the ID column
Alfalfa <- NotTreated %>% group_by(ID)

# Calculating the mean length
MeanLen <- Alfalfa %>% summarise(MeanLen = mean(RamifLength))
Alfalfa <- left_join(Alfalfa, MeanLen, by = "ID")

# Calculating standard error for length
StandErrLen <- Alfalfa %>% summarise(StandErrLen = sd(RamifLength))
Alfalfa <- left_join(Alfalfa, StandErrLen, by = "ID")

# Calculating the mean number of leaves
MeanLeaves <- Alfalfa %>% summarise(MeanLeaves = mean(NbLeaves))
Alfalfa <- left_join(Alfalfa, MeanLeaves, by = "ID")

# Calculating the total number of branches
Nombre <- Alfalfa %>% summarise(Nombre = max(NbRamif))
Alfalfa <- left_join(Alfalfa, Nombre, by = "ID")

# Using group_by and summarise to count the number of B and C branches
comptage <- Alfalfa %>%
  group_by(ID, Origin) %>%
  summarise(Nombre = n()) %>%
  pivot_wider(names_from = Origin, values_from = Nombre, values_fill = 0)
print(comptage)
Alfalfa <- left_join(Alfalfa, comptage, by = "ID")

summary(Alfalfa)

# Adding biomass data
donneeACP <- merge(Alfalfa, Biomasses, by = "ID", all.x = TRUE)

head(donneeACP)

# Retrieving LAI data to merge with the data frame used for PCA
LAI_recup <- LAI_ACP[, c("ID", "LAI8", "LAI10", "LAI13", "LAI17", "LAI20", "LAI23")]

donneeACP <- merge(donneeACP, LAI_recup, by = "ID", all.x = TRUE)

head(donneeACP)

# New dataset with only the Shadows
AlfalfaShadow <- donneeACP[donneeACP$LT == "S",]
AlfalfaControl <- donneeACP[donneeACP$LT == "C",]

########################## Description of variables ##########################
# Boxplots for various variables and LT
boxplot(MaxLength ~ LT, Alfalfa)
boxplot(MeanLeaves ~ LT, Alfalfa)
boxplot(Nombre ~ LT, Alfalfa)
boxplot(MaxLength ~ Var, Alfalfa)
boxplot(MeanLeaves ~ Var, Alfalfa)
boxplot(Nombre ~ Var, Alfalfa)

########################### PCA on all data ##############################
# Selecting variables for PCA
variables_analyses <- donneeACP[, c("MaxLength", "Nombre", "StandErrLen", "BiomassFin", "LAI10", "LAI13", "LAI17")]

# Performing PCA
acp_result <- PCA(variables_analyses, scale.unit = TRUE, graph = FALSE)

# Adding PCA results to the original DataFrame
donneeACP$PC1 <- acp_result$ind$coord[, "Dim.1"]
donneeACP$PC2 <- acp_result$ind$coord[, "Dim.2"]
donneeACP$PC3 <- acp_result$ind$coord[, "Dim.3"]

# Creating a plot with ggplot2: LT color
graph <- ggplot(donneeACP, aes(PC1, PC2, color = LT, shape = LT)) +
  geom_point() +
  scale_color_manual(values = c("C" = "pink", "S" = "blue")) +
  scale_shape_manual(values = c("C" = 17, "S" = 15)) + 
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Individual Representation with PCA"
  )

print(graph)

# Creating additional PCA plots
grapha <- ggplot(donneeACP, aes(PC1, PC3, color = LT, shape = LT)) +
  geom_point() +
  scale_color_manual(values = c("C" = "pink", "S" = "blue")) +
  scale_shape_manual(values = c("C" = 17, "S" = 15)) +  # 17 = triangle, 15 = square
  labs(
    x = "1st Principal Component",
    y = "3rd Principal Component",
    title = "Représentation des individus avec ACP"
  )

print(grapha)

#
graphb <- ggplot(donneeACP, aes(PC2, PC3, color = LT, shape = LT)) +
  geom_point() +
  scale_color_manual(values = c("C" = "pink", "S" = "blue")) +
  scale_shape_manual(values = c("C" = 17, "S" = 15)) +  # 17 = triangle, 15 = square
  labs(
    x = "2nd Principal Component",
    y = "3rd Principal Component",
    title = "Représentation des individus avec ACP"
  )

print(graphb)

#########################
# Create a graph with ggplot2: Var color
graph1 <- ggplot(donneeACP, aes(PC1, PC2, color = Var)) +
  geom_point() +
  scale_color_manual(values = c("LUZELLE" = "red", "ANTANE" = "blue", "BARVINCE" = "green", "MARINA" = "orange")) +
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph1)

# Create a graph with ggplot2: Var color in PC1 and PC3
graph1v <- ggplot(donneeACP, aes(PC1, PC3, color = Var)) +
  geom_point() +
  scale_color_manual(values = c("LUZELLE" = "red", "ANTANE" = "blue", "BARVINCE" = "green", "MARINA" = "orange")) +
  labs(
    x = "1st Principal Component",
    y = "3rd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph1v)

# Create a graph with ggplot2: Dormancy color
donneeACP$Dormancy <- factor(donneeACP$Dormancy)

graph2 <- ggplot(donneeACP, aes(PC1, PC2, color = Dormancy)) +
  geom_point() +
  scale_color_manual(values = c("2" = "red", "7" = "purple")) +
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph2)

# Graph of vectors
graph_with_vectors <- fviz_pca_var(acp_result, col.var = "contrib", repel = TRUE, 
                                   col.ind = as.factor(Alfalfa$LT))

print(graph_with_vectors)

### PCA on Shadows
# Select variables for PCA
variables_analyses_shadow <- AlfalfaShadow[, c("MaxLength", "Nombre", "MeanLeaves", "StandErrLen", "BiomassFin", "LAI10", "LAI13", "LAI17")]

# Perform PCA
acp_result_shadow <- PCA(variables_analyses_shadow, scale.unit = TRUE, graph = FALSE)

# Add PCA results to the original DataFrame
AlfalfaShadow$PC1 <- acp_result_shadow$ind$coord[, "Dim.1"]
AlfalfaShadow$PC2 <- acp_result_shadow$ind$coord[, "Dim.2"]
AlfalfaShadow$PC3 <- acp_result_shadow$ind$coord[, "Dim.3"]

# Create a graph with ggplot2: LT color
graph <- ggplot(AlfalfaShadow, aes(PC1, PC2, color = LT, shape = LT)) +
  geom_point() +
  scale_color_manual(values = c("C" = "pink", "S" = "blue")) +
  scale_shape_manual(values = c("C" = 17, "S" = 15)) +
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph)

# Create a graph with ggplot2: Var color
graph1 <- ggplot(AlfalfaShadow, aes(PC1, PC2, color = Var)) +
  geom_point() +
  scale_color_manual(values = c("LUZELLE" = "red", "ANTANE" = "blue", "BARVINCE" = "green", "MARINA" = "orange")) +
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph1)

# Create a graph with ggplot2: Dormancy color
AlfalfaShadow$Dormancy <- factor(AlfalfaShadow$Dormancy)

graph2 <- ggplot(AlfalfaShadow, aes(PC1, PC2, color = Dormancy)) +
  geom_point() +
  scale_color_manual(values = c("2" = "red", "7" = "purple")) +
  labs(
    x = "1st Principal Component",
    y = "2nd Principal Component",
    title = "Representation of individuals with PCA"
  )

print(graph2)

# Graph of vectors
graph_with_vectors <- fviz_pca_var(acp_result, col.var = "contrib", repel = TRUE, 
                                   col.ind = as.factor(Alfalfa$LT))

print(graph_with_vectors)

#################################################################################
# PLSDA with LT
library(mixOmics)
X <- variables_analyses

# Outcome Y that will be internally coded as dummy:
Y <- donneeACP$LT

dim(X); length(Y)

plsda.LT <- plsda(X, Y, ncomp = 5)

set.seed(30) # For reproducibility with this handbook, remove otherwise
perf.plsda.LT <- perf(plsda.LT, validation = 'Mfold', folds = 3, 
                      progressBar = FALSE,  # Set to TRUE to track progress
                      nrepeat = 10)         # We suggest nrepeat = 50

plot(perf.plsda.LT, sd = TRUE)

final.plsda.LT <- plsda(X, Y, ncomp = 4)

plotIndiv(final.plsda.LT, ind.names = FALSE, legend = TRUE,
          comp = c(1,2), ellipse = TRUE, 
          title = 'PLS-DA on LT comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')

plotIndiv(final.plsda.LT, ind.names = FALSE, legend = TRUE,
          comp = c(1,3), ellipse = TRUE, 
          title = 'PLS-DA on LT comp 2-3',
          X.label = 'PLS-DA comp 2', Y.label = 'PLS-DA comp 3')

############################# PLSDA with Var for Shadow
X_var<-AlfalfaShadow[,c("MaxLength", "Nombre", "MeanLeaves","StandErrLen", "BiomassFin","LAI10", "LAI13", "LAI17")]
Y_var <- AlfalfaShadow$Var

dim(X_var); length(Y_var)

plsda.Var <- plsda(X_var,Y_var, ncomp = 5)

set.seed(30) # For reproducibility with this handbook, remove otherwise
perf.plsda.Var <- perf(plsda.Var, validation = 'Mfold', folds = 3, 
                       progressBar = FALSE,  # Set to TRUE to track progress
                       nrepeat = 10)         # We suggest nrepeat = 50

plot(perf.plsda.Var, sd = TRUE)

final.plsda.Var <- plsda(X_var,Y_var, ncomp = 4)

plotIndiv(final.plsda.Var, ind.names = FALSE, legend=TRUE,
          comp=c(1,2), ellipse = TRUE, 
          title = 'PLS-DA on Var comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')

plotIndiv(final.plsda.Var, ind.names = FALSE, legend=TRUE,
          comp=c(2,3), ellipse = TRUE, 
          title = 'PLS-DA on Var comp 1-3',
          X.label = 'PLS-DA comp 2', Y.label = 'PLS-DA comp 3')

