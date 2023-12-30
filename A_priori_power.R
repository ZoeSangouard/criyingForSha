######### 1. Generate a response variable using the inputs. 
# Creation of a data.frame with the explanatory variables, 
# for the planned experimental design matrix
set.seed(1234)
var <- as.factor(c(rep('ant',30),rep('fau',30),rep('mar',30)))
light <- as.factor(rep(c(rep('C',10),rep('L100',10)),2))
matable <- data.frame(var,light)
# obtain the design matrix X from the data.frame
X <- model.matrix(~var+light+var*light)
# expected effect sizes: in a vector beta
beta <- c(41.9,0.5,-5.4,34.6,-4.1,-15)
# generation of the ALT level according to the assumed model, the planned design,
# the expected effect sizes and residual variance
expected_sdres <- 16.9
matable$LAI <- X%*%beta + rnorm(n=60,mean=0,sd=expected_sdres)
######### 2. Fit a model using `lm` on the generated response variable
# Here it will be adjusted on the generated response
m1 <- lm(LAI ~ var+light+var:light, data=matable)
summary(m1) # not exaclty what we expect
######### 3. Modify the model so that effect sizes have exactly the expected values 
coef(m1) <- c(41.9,0.5,-5.4,34.6,-4.1,-15)
######### A priori power
powerSim(m1, test=fixed("var:light","lr"), nsim=1000, progress=FALSE)

######### 1. Generate a response variable using the inputs. 
# Creation of a data.frame with the explanatory variables, 
# for the planned experimental design matrix
set.seed(1234)
sex <- as.factor(c(rep('F',150),rep('M',150)))
diet <- as.factor(rep(c(rep('C',50),rep('nonGMO',50),rep('GMO',50)),2))
matable <- data.frame(var,light)
# obtain the design matrix X from the data.frame
X <- model.matrix(~var+light+var*light)
# expected effect sizes: in a vector beta
beta <- c(41.9,0.5,-5.4,34.6,-4.1,-15)
# generation of the ALT level according to the assumed model, the planned design,
# the expected effect sizes and residual variance
expected_sdres <- 16.9
matable$LAI <- X%*%beta + rnorm(n=60,mean=0,sd=expected_sdres)
######### 2. Fit a model using `lm` on the generated response variable
# Here it will be adjusted on the generated response
m1 <- lm(LAI ~ diet+sex+diet:sex, data=matable)
summary(m1) # not exaclty what we expect
######### 3. Modify the model so that effect sizes have exactly the expected values 
coef(m1) <- c(41.9,0.5,-5.4,34.6,-4.1,-15)
######### A priori power
powerSim(m1, test=fixed("diet:sex","lr"), nsim=1000, progress=FALSE)

####### 2. Generate with the function pwr.f2
##power two-way ANOVA compare shadow with control
library (pwr)
#high
pwr.f2.test(u = 1,v = 196, f2 = 0.35, sig.level = 0.05, power = NULL)
#moderate
pwr.f2.test(u = 1,v = 196, f2 = 0.15, sig.level = 0.05, power = NULL)
#low
pwr.f2.test(u = 1,v = 196, f2 = 0.02, sig.level = 0.05, power = NULL)

##power two-way ANOVA compare shadow with control with an interraction
library (pwr)
#high
pwr.f2.test(u = 1,v = 56, f2 = 0.35, sig.level = 0.05, power = NULL)
#moderate
pwr.f2.test(u = 1,v = 56, f2 = 0.15, sig.level = 0.05, power = NULL)
#low
pwr.f2.test(u = 1,v = 56, f2 = 0.02, sig.level = 0.05, power = NULL)

##power three-way ANOVA compare 
library (pwr)
#high
pwr.f2.test(u = 2,v = 57, f2 = 0.35, sig.level = 0.05, power = NULL)
#moderate
pwr.f2.test(u = 2,v = 57, f2 = 0.15, sig.level = 0.05, power = NULL)
#low
pwr.f2.test(u = 2,v = 57, f2 = 0.02, sig.level = 0.05, power = NULL)