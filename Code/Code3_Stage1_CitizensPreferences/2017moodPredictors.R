##############################################################################
# citizenPreferences2.R
# GR: December 6, 2017
# Follows 2017citizenMoodPlots.R
# INPUT: macroMoodData.txt
# OUTPUT: Bayes models of macro mood predicted by economic variables
#         that are reported in Chapter 2.
##############################################################################

library (pcse)
library (gtools)
library (stargazer)
library (MASS)

###############################################
# Set jags inputs
###############################################

# Read MacroData
MacroData <- read.table ("Datasets/OriginalDataFiles/macroMoodData.txt", sep="\t", header=TRUE)

#############################################
# OLS models
#############################################

# BASED ON STEVENSON 2000
# All include a lag of the DV
# All include lags of economic variables
# Growth models are estimated separately from inflation+unemployment models
# All economic variables are in rates
# All include country fixed effects
# Stevenson has ca. 290 observations in 11 countries
# We have 360 in 81 countries

MacroData$policy.mood <- I(MacroData$interpol.policy.mood*100)
MacroData$policy.mood.l1 <- I(MacroData$interpol.policy.mood.l1*100)
MacroData$log.inflation <- log (MacroData$inflation+(abs(min(MacroData$inflation, na.rm=T)))+0.001)
MacroData$log.inflation.l1 <- log (MacroData$inflation.l1+(abs(min(MacroData$inflation.l1, na.rm=T)))+0.001)

MacroData.sep$policy.mood <- I(MacroData.sep$interpol.policy.mood*100)
MacroData.sep$policy.mood.l1 <- I(MacroData.sep$interpol.policy.mood.l1*100)
MacroData.sep$log.inflation <- log (MacroData.sep$inflation+(abs(min(MacroData.sep$inflation, na.rm=T)))+0.001)
MacroData.sep$log.inflation.l1 <- log (MacroData.sep$inflation.l1+(abs(min(MacroData.sep$inflation.l1, na.rm=T)))+0.001)

# All pooled
growth.model <- lm (policy.mood~policy.mood.l1
                    + growth.l1, data=MacroData)
misery.model <- lm (policy.mood~policy.mood.l1
                    + log.inflation.l1 + unemploy.l1, data=MacroData)
both.model <- lm (policy.mood~policy.mood.l1
                    + growth.l1 + log.inflation.l1 + unemploy.l1, data=MacroData)

growth.model.pcse <- pcse(growth.model
                          , groupN = MacroData$pais[-growth.model$na.action]
                          , groupT = MacroData$year[-growth.model$na.action]
                          , pairwise = TRUE)
misery.model.pcse <- pcse(misery.model
              , groupN = MacroData$pais[-misery.model$na.action]
              , groupT = MacroData$year[-misery.model$na.action]
              , pairwise = TRUE)
both.model.pcse <- pcse(both.model
                          , groupN = MacroData$pais[-both.model$na.action]
                          , groupT = MacroData$year[-both.model$na.action]
                          , pairwise = TRUE)

names (growth.model.pcse$pcse)[grep("X.Intercept.", names (growth.model.pcse$pcse))] <- "(Intercept)"
names (misery.model.pcse$pcse)[grep("X.Intercept.", names (misery.model.pcse$pcse))] <- "(Intercept)"
names (both.model.pcse$pcse)[grep("X.Intercept.", names (both.model.pcse$pcse))] <- "(Intercept)"

stargazer (growth.model, misery.model, both.model
           , se=list(growth.model.pcse$pcse, misery.model.pcse$pcse, both.model.pcse$pcse)
           , type="text")



# Long-term effects of variables
sample.coefs <- mvrnorm (1000, mu=both.model$coef, Sigma=vcov (both.model))


lte.growth    <- sample.coefs[,3] / (1-sample.coefs[,2])
lte.inflation <- sample.coefs[,4] / (1-sample.coefs[,2])
lte.unemploy  <- sample.coefs[,5] / (1-sample.coefs[,2])

quantile (lte.growth, prob=c(0.025,0.975))
quantile (lte.inflation, prob=c(0.025,0.975)); mean (lte.inflation); sd(lte.inflation)
quantile (lte.unemploy, prob=c(0.05,0.95))


100 * mean (lte.inflation)/sd(MacroData$policy.mood, na.rm=T)

# With country fixed effects
growth.model.cfe <- lm (policy.mood~policy.mood.l1
                    + growth.l1 + pais, data=MacroData
                    , contrasts = list(pais = "contr.sum"))
misery.model.cfe <- lm (policy.mood~policy.mood.l1
                    + log.inflation.l1 + unemploy.l1 + pais, data=MacroData
                    , contrasts = list(pais = "contr.sum"))
both.model.cfe <- lm (policy.mood~policy.mood.l1
                  + growth.l1 + log.inflation.l1 + unemploy.l1 + pais, data=MacroData
                  , contrasts = list(pais = "contr.sum"))

growth.model.cfe.pcse <- pcse(growth.model.cfe
                          , groupN = MacroData$pais[-growth.model.cfe$na.action]
                          , groupT = MacroData$year[-growth.model.cfe$na.action]
                          , pairwise = TRUE)
misery.model.cfe.pcse <- pcse(misery.model.cfe
                          , groupN = MacroData$pais[-misery.model.cfe$na.action]
                          , groupT = MacroData$year[-misery.model.cfe$na.action]
                          , pairwise = TRUE)
both.model.cfe.pcse <- pcse(both.model.cfe
                        , groupN = MacroData$pais[-both.model.cfe$na.action]
                        , groupT = MacroData$year[-both.model.cfe$na.action]
                        , pairwise = TRUE)

names (growth.model.cfe.pcse$pcse)[grep("X.Intercept.", names (growth.model.cfe.pcse$pcse))] <- "(Intercept)"
names (misery.model.cfe.pcse$pcse)[grep("X.Intercept.", names (misery.model.cfe.pcse$pcse))] <- "(Intercept)"
names (both.model.cfe.pcse$pcse)[grep("X.Intercept.", names (both.model.cfe.pcse$pcse))] <- "(Intercept)"

stargazer (growth.model.cfe, misery.model.cfe, both.model.cfe
           , se=list(growth.model.cfe.pcse$pcse, misery.model.cfe.pcse$pcse, both.model.cfe.pcse$pcse)
           , type="text")


# Table, all models together
stargazer (growth.model, growth.model.cfe, misery.model, misery.model.cfe, both.model, both.model.cfe
           , type="latex", align=TRUE, digits=2)

