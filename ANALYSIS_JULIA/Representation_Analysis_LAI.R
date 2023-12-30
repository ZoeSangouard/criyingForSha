#import packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpmisc)
library (FactoMineR)
library(factoextra)

#import data
LAItable<- read.table("LAI_pour_transfo_log.csv",header=TRUE,sep=";",dec=",")
LAI_sans_NA <- LAItable[!is.na(LAItable$LAI), ]
print(LAI_sans_NA)
summary(LAI_sans_NA)
LAI_sans_NA$NormLAI<-LAI_sans_NA$LAI/(LAI_sans_NA$MaxLAI)
LAI_sans_NA$NormLAI[LAI_sans_NA$NormLAI==1]=0.99

# Instead of 0.99, replace max values when they equal 1 with 99% of the max
LAI_sans_NA$NormLAI[LAI_sans_NA$NormLAI==1]=LAI_sans_NA$LAI/(0.99*LAI_sans_NA$MaxLAI)

Data_graph<- LAI_sans_NA%>%
  group_by(Time,Var,LT)%>%
  summarise(Moy=mean(NormLAI),
            Err=sd(NormLAI))

print(Data_graph)

#Graph : Mean of the Normalized LAI by time for different Varieties for two light treatments Control and Shadow
graph<-ggplot(Data_graph, aes(x = Time, y = Moy, color = Var)) +
  geom_errorbar(aes(ymin = Moy - Err, ymax = Moy + Err), width = 0.01) +
  geom_point(size = 2, fill = "white") +
  labs(x = "Time", y = "Mean of the Normalized LAI", color = "Variety") +
  ggtitle("Mean of the Normalized LAI by time for different Varieties for two light treatments Control and Shadow") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))+
  facet_wrap(~ LT, scales = "free")

#Graph : Mean of the Normalized LAI VS time for different Varieties for two light treatments Control and Shadow
graph1<-ggplot(Data_graph, aes(x = Time, y = Moy, color = Var, shape=LT)) +
  geom_errorbar(aes(ymin = Moy - Err, ymax = Moy + Err), width = 0.01) +
  geom_point(size = 2, fill = "white") +
  labs(x = "Time", y = "Mean of the Normalized LAI", color = "Variety", shape = "Light Treatment") +
  ggtitle("Mean of the Normalized LAI VS time") +
  theme_minimal( ) +
  scale_y_continuous(limits = c(0.3, 1))+
  facet_wrap(~ Var, scales = "free")
print(graph1)


model_summaries <- vector("list", length = nrow(Data_graph))

#Graph : Normalized LAI by time for different Varieties for two light treatments Control and Shadow
graph2<-ggplot(LAI_sans_NA[121:813,], aes(x = Time, y = NormLAI, color = Var, shape=LT))+
  geom_point(size = 2, fill = "white")+
  labs(x = "Time", y = "Normalized LAI", color = "Variety", shape = "Light Treatment")+
  ggtitle("Normalized LAI by time for different Varieties for two light treatments Control and Shadow")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 1))+
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE, color = "black", size = 0.5)+
  facet_wrap(~ Var+LT, scales = "free", nrow = 2)


#Graph : Normalized LAI by time for different Varieties for two light treatments Control and Shadow
graph3<-ggplot(data = LAI_sans_NA[121:813,], aes(x = Time, y = NormLAI, color = Var, shape=LT)) +
  geom_point(size = 2, fill = "white") +
  labs(x = "Time", y = "Normalized LAI", color = "Variety", shape = "Light Treatment") +
  ggtitle("Normalized LAI by time for different Varieties for two light treatments Control and Shadow") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))+
  geom_smooth(method = "glm", method.args = list(family ="gaussian"), se = FALSE, color = "black", size = 0.5)+
  facet_wrap(~ Var+LT, scales = "free", nrow = 2)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE,
               geom = "text",
               size = 2,
               label.x = "middle", label.y = "bottom",
               color="black")
print(graph)

print(graph2)
print(graph3)


# Representation graphique avec la transformation en ln (logistic transformation)
# Data transformation => ln(1+NormLAI)
LAI_sans_NA$TransfoLAI <- log(LAI_sans_NA$NormLAI / (1 - LAI_sans_NA$NormLAI))

# Graph: Normalized LAI Transformed by time for different Varieties for two light treatments Control and Shadow
ggplot(LAI_sans_NA, aes(x = Time, y = NormLAI, color = Var, shape = LT)) +
  geom_point(size = 2, fill = "white") +
  labs(x = "Time", y = "Normalized LAI", color = "Variety", shape = "Light Treatment") +
  ggtitle("Normalized LAI VS time") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE, color = "black", size = 0.5) +
  facet_wrap(~ Var + LT, scales = "free", nrow = 2)

# Summary of TransfoLAI and NormLAI columns
summary(LAI_sans_NA$TransfoLAI)
summary(LAI_sans_NA$NormLAI)

# Representation graphique avec la transformation en ln (logistic transformation)
# Data transformation => ln(NormLAI) / ln(1+NormLAI)
LAI_sans_NA$TransfoLAI <- log(LAI_sans_NA$NormLAI / (1 - LAI_sans_NA$NormLAI))

# Graph: Transformed Normalized LAI by time for different Varieties for two light treatments Control and Shadow
ggplot(LAI_sans_NA, aes(x = Time, y = TransfoLAI, color = Var, shape = LT)) +
  geom_point(size = 2, fill = "white") +
  labs(x = "Time", y = "TransfoLAI", color = "Variety", shape = "Light Treatment") +
  ggtitle("TransfoLAI VS time") +
  theme_minimal() +
  scale_y_continuous(limits = c(-2, 9.5)) +
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE, color = "black", size = 0.5) +
  facet_wrap(~ Var + LT, scales = "free", nrow = 2) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    geom = "text",
    size = 2.5,
    label.x = 15,
    label.y = 7.5,
    color = "black"
  )

# Subset the data for specific Variety (Var) and Light Treatment (LT)
AC <- subset(LAI_sans_NA, LAI_sans_NA$Var == "ANTANE" & LAI_sans_NA$LT == "C")

# Perform logistic transformation based on the coefficients a_AC and b_AC
x <- seq(8, 23, by = 0.1)
a_AC <- 0.161
b_AC <- -0.504
y <- exp(a_AC * x + b_AC) / (1 + exp(a_AC * x + b_AC))

# Plotting the data and the logistic transformation
plot(AC$Time, AC$NormLAI)
points(x, y, type = "l")

########################################Statistical Test
# Performing ANCOVA (Analysis of Covariance)
model <- lm(TransfoLAI ~ LT + Var + Time + Var * LT * Time, data = LAI_sans_NA)
summary(model)

# Converting Time variable into a categorical variable
LAI_sans_NA$Time <- as.factor(LAI_sans_NA$Time)

# Performing ANOVA (Analysis of Variance) for LT, Time, and their interaction
model <- lm(NormLAI ~ LT * Time, data = LAI_sans_NA)
summary(model)
plot(model)

## Creating Q-Q plots for each group based on Time (and ideally for LT, but we don't have points here for LT)
