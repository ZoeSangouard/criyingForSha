
library(ggplot2)
library(tidyr)
library(dplyr)

anovatable <- read.table("table_ANOVA.csv", sep=',', dec='.', header=TRUE, stringsAsFactors = T)

Boxplot(MaxLength~LT, data=anovatable, id=list(method="y"))
scatterplot(MaxLength~Var | LT, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, by.groups=TRUE, data=anovatable)

plot(MaxLength ~ Var, data = anovatable, col = LT)

anova_maxlen<-lm(MaxLength~Var*LT, data=anovatable)
summary(anova_maxlen)


#Les coefficients fournissent des estimations des effets des différentes variables sur la variable dépendante MaxLength.
#Les intercepts pour chaque niveau de Var sont fournis : VarBARVINCE, VarLUZELLE, VarMARINA.
#La variable LTS a un coefficient de 1.6198.
#Il y a des termes d'interaction, tels que VarBARVINCE:LTS, VarLUZELLE:LTS, VarMARINA:LTS.

#La statistique R carré multiple (Multiple R-squared) est de 0.2929, ce qui indique que le modèle explique environ 29.29% de la variance totale dans MaxLength. 

#La distribution des résidus est importante pour vérifier les violations des hypothèses de la régression. Dans ce cas, la distribution des résidus semble asymétrique, avec une queue plus lourde du côté positif.

residus = rstudent(anova_maxlen)
hist(residus)
qqnorm(residus, pch = 1, frame = FALSE)
qqline(residus, col = "red", lwd = 2)
shapiro.test(residus)


plot(residuals(anova_maxlen))
#Test de normalit
shapiro.test(residuals(anova_maxlen))

#Plus la valeur de W est proche de 1, plus les données sont compatibles avec une distribution normale. Dans votre cas, W est relativement proche de 1 (0.98729), suggérant une certaine normalité dans les résidus.
# le p-value est très faible (2.202e-06, soit essentiellement zéro), ce qui suggère que l'hypothèse nulle de normalité des résidus est rejetée. 


#Test de Levene d'homogeneite

white.test(anova_maxlen)


#Puis test de comparaison multiple des moyennes
print(HSD.test(anova_maxlen,"MaxLen"))
hsd <- HSD.test(anova_maxlen,"MaxLen")
hsd$groups
print(SNK.test(anova_maxlen,"MaxLen"))

