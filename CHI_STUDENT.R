# Import packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Import CSV file for ANOVA
NotTreated <- read.table("table_ANOVA.csv", sep = ',', dec = '.', header = TRUE, stringsAsFactors = TRUE)

# Import biomass values
Biomasses <- read.table("BiomasseFin.csv", header = TRUE, sep = ";", dec = ",")

# Use group_by to group the data by the ID column
Alfalfa <- NotTreated %>%
  group_by(ID)

# Calculate the mean length
MeanLen <- Alfalfa %>%
  summarise(MeanLen = mean(RamifLength))
Alfalfa <- left_join(Alfalfa, MeanLen, by = "ID")

StandErrLen <- Alfalfa %>%
  summarise(StandErrLen = sd(RamifLength))
Alfalfa <- left_join(Alfalfa, StandErrLen, by = "ID")

# Calculate the mean number of leaves
MeanLeaves <- Alfalfa %>%
  summarise(MeanLeaves = mean(NbLeaves))
Alfalfa <- left_join(Alfalfa, MeanLeaves, by = "ID")

# Calculate the total number of branches
Nombre <- Alfalfa %>%
  summarise(Nombre = max(NbRamif))
Alfalfa <- left_join(Alfalfa, Nombre, by = "ID")

# Create a new dataset with only Shadow data
AlfalfaShadow <- subset(Alfalfa, LT == "S")
AlfalfaControl <- subset(Alfalfa, LT == "C")

# Chi-square test on the data with LT = C
tablo <- xtabs(~Var + Origin, data = AlfalfaControl)
cat("\nFrequency table:\n")
print(tablo)
testo <- chisq.test(tablo, correct = FALSE)
print(testo)
cat("\nExpected counts:\n")
print(testo$expected)
cat("\nChi-square components:\n")
print(round(testo$residuals^2, 2))

# Chi-square test on the data with LT = S
tablo <- xtabs(~Var + Origin, data = AlfalfaShadow)
cat("\nFrequency table:\n")
print(tablo)
testo <- chisq.test(tablo, correct = FALSE)
print(testo)
cat("\nExpected counts:\n")
print(testo$expected)
cat("\nChi-square components:\n")
print(round(testo$residuals^2, 2))

# Student's t-test
Student_Maxlength <- t.test(AlfalfaShadow$MaxLength, AlfalfaControl$MaxLength)
print(Student_Maxlength)

Student_NbRamif <- t.test(AlfalfaShadow$NbRamif, AlfalfaControl$NbRamif)
print(Student_NbRamif)

Student_MeanInternode <- t.test(AlfalfaShadow$MeanInternodeLengthRamif, AlfalfaControl$MeanInternodeLengthRamif)
print(Student_MeanInternode)

Student_Len <- t.test(AlfalfaShadow$MeanLen, AlfalfaControl$MeanLen)
print(Student_Len)

Student_leaves <- t.test(AlfalfaShadow$MeanLeaves, AlfalfaControl$MeanLeaves)
print(Student_leaves)

