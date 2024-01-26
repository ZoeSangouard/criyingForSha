# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(car)
library(agricolae)
library(kableExtra)
library(knitr)
library(ggpubr)

# Read data from CSV file
anovatable <- read.table("table_ANOVA.csv", sep=',', dec='.', header=TRUE, stringsAsFactors = TRUE)

# Convert Dormancy column to factor
anovatable$Dormancy <- as.factor(anovatable$Dormancy)

# Create trait_id variable
anovatable$trait_id <- paste0(anovatable$Var, "-", anovatable$LT)

# Define colors for boxplot
rdre_couleurs <- c("lightyellow", "darkgray", "lightyellow", "darkgray", "lightyellow", "darkgray", "lightyellow", "darkgray")

# Perform ANOVA and boxplot for MaxLength
maxlength_anova <- lm(MaxLength ~ Var * LT, data = anovatable)
print(summary(aov(maxlength_anova)))
print(summary(maxlength_anova))
boxplot(MaxLength ~ trait_id, data = anovatable, col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Perform ANOVA and boxplot for NbRamif
nbramif_anova <- lm(NbRamif ~ Var * LT, data = anovatable)
print(summary(aov(nbramif_anova)))
print(summary(nbramif_anova))
boxplot(NbRamif ~ trait_id, data = anovatable, col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Perform ANOVA and boxplot for MeanInternodeLengthRamif
meaninternode_anova <- lm(MeanInternodeLengthRamif ~ Var * LT, data = anovatable)
print(summary(aov(meaninternode_anova)))
print(summary(meaninternode_anova))
boxplot(MeanInternodeLengthRamif ~ trait_id, data = anovatable, col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Create a summary table
summary_table_boxplot <- anovatable %>%
  group_by(trait_id) %>%
  summarise(
    Mean_MaxLength = mean(MaxLength),
    SD_MaxLength = sd(MaxLength),
    Mean_NbRamif = mean(NbRamif),
    SD_NbRamif = sd(NbRamif),
    Mean_Internode = mean(MeanInternodeLengthRamif),
    SD_Internode = sd(MeanInternodeLengthRamif),
    Count = n()
  )

# Print the summary table
print(summary_table_boxplot)

# Generate a table with kable
summary_table_html <- kable(summary_table_boxplot, "html") %>%
  kable_styling("striped", full_width = FALSE)

# Print the table
print(summary_table_html)

# Read data from another CSV file
anovatable2 <- read.table("DATA_LAI_BIOMASS.csv", sep=';', dec=',', header=TRUE, stringsAsFactors = TRUE)

# Group data by ID
anovatable2_group <- anovatable2 %>%
  group_by(ID)
# Remove NA values
anova2_NA <- anovatable2_group[complete.cases(anovatable2_group$BiomassFin), ]

# Plot histogram for BiomassFin
hist(anova2_NA$BiomassFin)

# Perform ANOVA on BiomassFin
library(lsmeans)

# Convert Var and Dormancy to factors
anova2_NA$Var <- as.factor(as.character(anova2_NA$Var))
anova2_NA$Dormancy <- as.factor(anova2_NA$Dormancy)

# Fit the linear model
anova_Biomass <- lm(log10(BiomassFin) ~ Var * LT + log10(BiomassCut), data = anova2_NA)

# Plot the data
plot(log10(anova2_NA$BiomassFin), log10(anova2_NA$BiomassCut))
hist(anova2_NA$BiomassCut)

# Display the ANOVA table
summary(aov(anova_Biomass))
print(summary(anova_Biomass))

# Perform pairwise comparisons
ok <- emmeans(anova_Biomass, ~Var)
pairs(ok)

# Boxplot for BiomassFin
boxplot(BiomassFin ~ trait_id, data = anova2_NA, xlab = "Variety-LT", ylab = "BiomassFin", col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Split data by LT
AlfalfaShadow <- subset(anova2_NA, LT == "S")
AlfalfaControl <- subset(anova2_NA, LT == "C")

# Perform Mann-Whitney U test
wilcox.test(AlfalfaControl$BiomassFin, AlfalfaShadow$BiomassFin)

# Perform Student's t-test
Student_biomass <- t.test(AlfalfaShadow$BiomassFin, AlfalfaControl$BiomassFin)
print(Student_biomass)

# Plot residuals
par(mfrow = c(2, 2))
plot(anova_Biomass)
par(mfrow = c(1, 1))

# Check normality and homogeneity
residus4 <- rstudent(anova_Biomass)
hist(residus4)
qqnorm(residus4, pch = 1, frame = FALSE)
qqline(residus4, col = "red", lwd = 2)
shapiro.test(residus4)

resultat_levene_biomass <- leveneTest(residus4, anova2_NA$Var)
resultat_levene_biomass2 <- leveneTest(residus4, anova2_NA$LT)

print(resultat_levene_biomass)
print(resultat_levene_biomass2)

