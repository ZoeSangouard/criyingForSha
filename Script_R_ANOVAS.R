library(ggplot2)
library(tidyr)
library(dplyr)
library(car)

# Read data from CSV file
anovatable <- read.table("table_ANOVA.csv", sep=',', dec='.', header=TRUE, stringsAsFactors = TRUE)

# Group data by ID
anovatable_group <- anovatable %>%
  group_by(ID)

##### MaxLength

# Plot histogram of MaxLength
hist(anovatable_group$MaxLength)

# Perform ANOVA on MaxLength
anova_maxlen <- lm(MaxLength ~ Var * LT, data = anovatable_group)
summary(aov(anova_maxlen))

# Create trait_id variable
anovatable_group$trait_id <- paste0(anovatable_group$Var, "-", anovatable_group$LT)

# Define colors for boxplot
rdre_couleurs <- c("lightyellow", "darkgray", "lightyellow", "darkgray", "lightyellow", "darkgray", "lightyellow", "darkgray")

# Create boxplot for MaxLength
boxplot(anovatable_group$MaxLength ~ anovatable_group$trait_id, xlab = "Variety-LT", ylab = "MaxLength", col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Plot residuals

par(mfrow = c(2, 2))
plot(anova_maxlen)

par(mfrow = c(1, 1))

# Check normality of residuals
residus <- rstudent(anova_maxlen)
hist(residus)
qqnorm(residus, pch = 1, frame = FALSE)
qqline(residus, col = "red", lwd = 2)
plot(residuals(anova_maxlen))

# Shapiro-Wilk normality test
shapiro.test(residus)

# Levene's test for homogeneity
resultat_levene <- leveneTest(residus, anovatable_group$Var)
resultat_levene2 <- leveneTest(residus, anovatable_group$LT)
print(resultat_levene)
print(resultat_levene2)

###### ANOVA for NbRamif

# Plot histogram for NbRamif
hist(anovatable_group$NbRamif)

# Perform ANOVA on NbRamif
anova_nbramif <- lm(NbRamif ~ Var * LT, data = anovatable_group)
summary(aov(anova_nbramif))

# Create boxplot for NbRamif
boxplot(anovatable_group$NbRamif ~ anovatable_group$trait_id, xlab = "Variety-LT", ylab = "NbRamif", col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Plot residuals

par(mfrow = c(2, 2))

plot(anova_nbramif)

par(mfrow = c(1, 1))

# Try with a Poisson transformation
ganova_nbramif <- glm(NbRamif ~ Var * LT, data = anovatable_group, family = "poisson")
summary(aov(ganova_nbramif))
boxplot(anovatable_group$NbRamif ~ anovatable_group$trait_id, las = 2, cex.axis = 0.5)

# Check normality of residuals
residus2 <- rstudent(anova_nbramif)
hist(residus2)
qqnorm(residus2, pch = 1, frame = FALSE)
qqline(residus2, col = "red", lwd = 2)

# Shapiro-Wilk normality test
shapiro.test(residus2)

# Levene's test for homogeneity
resultat_levene_Ramif <- leveneTest(residus2, anovatable_group$Var)
resultat_levene_Ramif2 <- leveneTest(residus2, anovatable_group$LT)
print(resultat_levene_Ramif)
print(resultat_levene_Ramif2)

##### ANOVA for MeanInternodeLengthRamif

# Plot histogram for MeanInternodeLengthRamif
hist(anovatable_group$MeanInternodeLengthRamif)

# Perform ANOVA on MeanInternodeLengthRamif
anova_internode <- lm(MeanInternodeLengthRamif ~ Var * LT, data = anovatable_group)
summary(aov(anova_internode))

# Create boxplot for MeanInternodeLengthRamif
boxplot(anovatable_group$MeanInternodeLengthRamif ~ anovatable_group$trait_id, xlab = "Variety-LT", ylab = "Internode Length", col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Plot residuals

par(mfrow = c(2, 2))

plot(anova_internode)

par(mfrow = c(1, 1))

# Check normality of residuals
residus3 <- rstudent(anova_internode)
hist(residus3)
qqnorm(residus3, pch = 1, frame = FALSE)
qqline(residus3, col = "red", lwd = 2)
shapiro.test(residus3)

# Levene's test for homogeneity
resultat_levene_internode <- leveneTest(residus3, anovatable_group$Var)
resultat_levene_internode2 <- leveneTest(residus3, anovatable_group$LT)
print(resultat_levene_internode)
print(resultat_levene_internode2)

##### ANOVA for BiomassFin

# Read data from another CSV file
anovatable2 <- read.table("DATA_LAI_BIOMASS.csv", sep=';', dec=',', header=TRUE, stringsAsFactors = TRUE)

# Group data by ID
anovatable2_group<- anovatable2%>%
  group_by(ID)

# Remove the NA

anova2_NA <- anovatable2_group[complete.cases(anovatable2_group$BiomassFin), ]

# Plot histogram for BiomassFin

hist(anova2_NA$BiomassFin)

# Perform ANOVA on BiomassFin
anova_Biomass <- lm(log10(BiomassFin) ~ Var + LT + log10(BiomassCut), data = anova2_NA)
summary(aov(anova_Biomass))

# Create a trait_id variable for boxplot
anova2_NA$trait_id = paste0(anova2_NA$Var, "-", anova2_NA$LT)

# Boxplot for BiomassFin
boxplot(anova2_NA$BiomassFin ~ anova2_NA$trait_id, xlab = "Variety-LT", ylab = "BiomassFin", col = rdre_couleurs, las = 2, cex.axis = 0.5)

# Plot residuals

par(mfrow = c(2, 2))

plot(anova_Biomass)

par(mfrow = c(1, 1))

# Normality
residus4 = rstudent(anova_Biomass)
hist(residus4)
qqnorm(residus4, pch = 1, frame = FALSE)
qqline(residus4, col = "red", lwd = 2)
shapiro.test(residus4)

# Homogeneity
resultat_levene_biomass <- leveneTest(residus4, anova2_NA$Var)
resultat_levene_biomass2 <- leveneTest(residus4, anova2_NA$LT)

print(resultat_levene_biomass)
print(resultat_levene_biomass2)

########################################################################

