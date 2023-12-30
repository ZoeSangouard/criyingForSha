library(pwr)
library(pwr2)

##########POWER for the ANOVA's on the morphological traits
# Specify the necessary values
effect_size = 0.35
alpha = 0.05
groups = 8
sample_size = 15

#observed power for the ANOVA
power <- pwr.anova.test(k=groups, n=sample_size, f=effect_size,sig.level=alpha,power=NULL)$power
print(paste("Observed power for ANOVA:", power))


##########POWER for the ANOVA on the LAI
# Specify the necessary values
effect_size = 0.35
alpha = 0.05
groups = 12
sample_size = 60

#observed power for the ANOVA
power <- pwr.anova.test(k=groups, n=sample_size, f=effect_size,sig.level=alpha,power=NULL)$power
print(paste("Observed power for ANOVA:", power))
