# PSYC 4330 - Mediation Example

# Let's explore whether "automatic negative 
# thoughts" (M) mediates the relationship 
# between "anxiety" (X) and "depression" (Y)

library(negligible)
d <- perfectionism
names(d)
head(d)


# ANX -> ATQ -> DEP
# With Bootstrapping
library(mediation)
med_model <- lm(atqpre.total ~ baipre.total, data = d)
tot_model <- lm(cesdpre.total ~ baipre.total + atqpre.total,
                data = d)
res <- mediate(model.m = med_model,
               model.y = tot_model,
               treat = 'baipre.total',
               mediator = 'atqpre.total',
               boot = TRUE, sims = 100)
summary(res)

# Indirect Effect (a*b, ACME)
coef(med_model)[2]*coef(tot_model)[3]
# Increasing BAI by one unit is expected to increase 
# CESD by .44, through ATQ

# Completely Standardized Indirect Effect
library(MBESS)
upsilon(x=d$baipre.total,
        mediator=d$atqpre.total,
        dv=d$cesdpre.total, B=25)

# Understanding Upsilon: 
# Product of Standardized Coefficients (a*b)
d$zbaipre.total<-scale(d$baipre.total)
d$zatqpre.total<-scale(d$atqpre.total)
d$zcesdpre.total<-scale(d$cesdpre.total)
za<-coef(lm(zatqpre.total ~ zbaipre.total, data=d))[2]
zb<-coef(lm(zcesdpre.total ~ zatqpre.total + zbaipre.total, data=d))[2]
za^2*zb^2  


# Plots
plot(y = d$cesdpre.total, x = d$baipre.total)
abline(lm(cesdpre.total ~ baipre.total, 
          data = d), col = "red")
library(car)
avPlots(tot_model,terms = "baipre.total",
        ylim=c(-10,30))

