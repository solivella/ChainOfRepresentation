###################################################
# This script inputs, analyzes, and scales ALL
# raw indexes from Lora and Morley (the latter 
# via Escaith & Paunovic) 
#
# GR: September 20, 2016
###################################################

library (reshape)
library (ggplot2)
library (MCMCpack)
library (gtools)
library (mcmcplots)
library (runjags)
library (rjags)
library (stargazer)
library (ltm)
library (car)

#####################
#### Import data ####
#####################

graphicsPath <- "~/Dropbox/ChainOfResponsiveness/BookManuscript/Graphs/structuralReformPolicies/"

# Identify all indices that we need to import
setwd ("~/Dropbox/ChainOfResponsiveness/Versions of SPI/separatePolicyFiles/")
workingPath  <- getwd ()
datasets <- c("LoraFinancialIndex.txt","LoraLaborIndex.txt","LoraPrivatizationIndex.txt","LoraSRI.txt","LoraTaxIndex.txt","LoraTradeIndex.txt","MorleyAllindices.txt")

# Function to reshape the indices into wide format
meltCast <- function (data) {
  temp <- read.table (data, header=T, sep="\t")
  temp <- melt (temp, id.vars=c("Country","Variable")
                , measure.vars=colnames(temp)[grep("X", colnames(temp))]
                , na.rm=TRUE)
  colnames (temp) <- c("Country","Series","Year","value")
  temp$value <- as.numeric (temp$value)
  temp$Year <- as.numeric (substr (temp$Year, 2, 5))
  tempOut <- cast (temp, Country + Year ~ Series)
  tempOut$id <- paste (tempOut$Country, tempOut$Year, sep="-")
  return (tempOut)
}

# Reshaping into wide format, then merging all indices from Lora and Morley
allData <- list(length=7)
for (i in 1:length (datasets)){
  allData[[i]] <- meltCast (datasets[i])  
}
allIndex <- merge_recurse (allData) #Ignore warning

############################
#### Build full dataset ####
############################

# Building a dataset with standardized Lora scores and a set that excludes all standard scores
stdIndex <- allIndex[, c(1:3, grep ("Std", colnames (allIndex)))]
stdIndex <- stdIndex[stdIndex$Year > 1984,] # std variables are all in Lora's set, which starts in 1985
regIndex <- allIndex[, -grep("Std", colnames (allIndex))]

#####################
#### Plot graphs ####
#####################

# Function to plot longitudinal within-country variation
plotSeries <- function (data, pais, Series, nameSeries) { # data is a data.frame like regIndex or stdIndex, pais is as.character and Series is a vector of strings
  temp <- data[data$Country==pais, is.element (colnames(data), c(Series, "Year"))]
  temp.melted <- melt (temp, id="Year")
  ggplot(data=temp.melted, aes(x=Year, y=value, color=variable)) +
     coord_cartesian(ylim = c(0, 1)) +
    geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
     ggtitle(bquote(atop(.(pais), atop(.(nameSeries), "")))) #+ theme(plot.title = element_text(face="bold"))
}

paises <- as.character (levels (stdIndex$Country))
financeVars <- c("StdFinancialTransactionTax","StdBankSupervQuality","StdInterestRateLiberalization","StdReserveRequirement")
laborVars   <- c("StdHiringFlexibility","StdJobTerminationCost","StdMinimumWage","StdWorkHoursFlexibility","StdSocSecurityTax")
fiscalVars  <- c("StdCorporateIncomeTax","StdIncomeTaxProductivity","StdPersonalIncomeTax","StdVATproductivity","StdVATrate")
tradeVars   <- c("StdAverageTariff","StdTradeDispersion")
listVars <- list (financeVars, laborVars, fiscalVars, tradeVars)
names (listVars) <- c("Financial reform", "Labor market reform", "Tax reform", "Trade reform")

# Plot per country and per series (each series contains between 2 and 5 variables
# setwd (graphicsPath)
# for (i in 1:length(paises)) {
#    for (j in 1:4) {
#       ps <- paises[i]
#       sr <- names (listVars)[j]
#       pdf (paste (ps, "-", sr, ".pdf", sep=""), h=7, w=9)
#          print (plotSeries (data=stdIndex, pais=ps, Series=listVars[[j]], nameSeries=sr))
#       dev.off ()
#    }
# }

#############################################
#### Start plain-vanilla factor analysis ####
#############################################

# For factor analysis, we will use non-standardized indices from Lora (some of these are ordered categories)
# Later we might add with two indices from Morley (MPprivatizationIndex and MPcapitalIndex)

setwd (workingPath)
financeVars <- c("FinancialTransactionTax","BankSupervQuality","InterestRateLiberalization","ReserveRequirement") #Morley excludes BankSupervQuality, we'll do the same
laborVars   <- c("HiringFlexibility","JobTerminationCost","MinimumWage","WorkHoursFlexibility","SocSecurityTax")
fiscalVars  <- c("CorporateIncomeTax","IncomeTaxProductivity","PersonalIncomeTax","VATproductivity","VATrate")
tradeVars   <- c("AverageTariff","TradeDispersion")
listVars <- list (financeVars, laborVars, fiscalVars, tradeVars)
names (listVars) <- c("Financial reform", "Labor market reform", "Tax reform", "Trade reform")



################################
#### Financial Reform Index ####
################################
# Might need to check the FinancialTransactionTax; paper reports it should be dummy, but it's not.
# In reality, it is a bunch of 0s, then some decimal numbers, then a few 15s.
FinanceMatrix <- regIndex[,is.element(colnames(regIndex), financeVars)]
FinanceLoadSigns <- c(1,1,1,-1) #1 for loadings that should be positive, -1 for loadings that should be negative
# It is difficult to anticipate which loading signs we'll see, because there is a disparity between what Lora writes in the appendix about variable coding and what actually appears in the dataset. E.g, the FinancialTransactionTax variable should be dichotomous, with 1 corresponding to low financial transaction taxes. Instead, it is continuous with values between 0 and 1.

FinanceOmittedObs <- attr (na.omit(FinanceMatrix), "na.action")
FinanceMatrix <- na.omit (FinanceMatrix)

# Preliminary principal component run
FinanceFactor <- factanal (~FinancialTransactionTax + InterestRateLiberalization + ReserveRequirement
                   , factors=1
                   , rotation="varimax"
                   , scores="regression"
                   , na.action=na.exclude
                   , data=FinanceMatrix)

FinanceFactor$loadings[,1]
FinanceFactor$uniquenesses

# Uniquenesses on a one factor model are very high.

FinanceFactanal <- data.frame (country=regIndex$Country[-FinanceOmittedObs], year=regIndex$Year[-FinanceOmittedObs], finance.scores=FinanceFactor$scores)
colnames(FinanceFactanal)[grep("Factor1", colnames(FinanceFactanal))] <- "finance.scores"
FinanceFactanal$id <- paste (FinanceFactanal$country, FinanceFactanal$year, sep="-")

plotFinance <- data.frame (cbind (FinanceFactanal, FinanceMatrix))

ggplot(data=plotFinance, aes(x=year, y=finance.scores, color=country)) +
  geom_line()  # facet_grid(variable ~ .) + # use when values are not on same scale
  
  


###################################
#### Labor Market Reform Index ####
###################################
LaborMatrix <- regIndex[,is.element(colnames(regIndex), laborVars)]
LaborLoadSigns <- c(1,-1,-1,-1,1) #1 for loadings that should be positive, -1 for loadings that should be negative


LaborOmittedObs <- (1:nrow(LaborMatrix))[apply (LaborMatrix, 1, invalid)]
LaborMatrix <- LaborMatrix[-LaborOmittedObs,]

# Preliminary principal component run
LaborFactor <- factanal (~HiringFlexibility + JobTerminationCost + MinimumWage
                         + SocSecurityTax + WorkHoursFlexibility
                         , factors=1
                         , rotation="varimax"
                         , scores="regression"
                         , na.action=na.exclude
                         , data=LaborMatrix)

LaborFactor$loadings[,1]
LaborFactor$uniquenesses

# Same issues as before: High uniquenesses, loadings with wrong signals.

LaborFactanal <- data.frame (country=regIndex$Country[-LaborOmittedObs], year=regIndex$Year[-LaborOmittedObs], labor.scores=LaborFactor$scores)
colnames(LaborFactanal)[grep("Factor1", colnames(LaborFactanal))] <- "labor.scores"
LaborFactanal$id <- paste (LaborFactanal$country, LaborFactanal$year, sep="-")




#############################
#### Fiscal Reform Index ####
#############################
FiscalMatrix <- regIndex[,is.element(colnames(regIndex), fiscalVars)]
FiscalLoadSigns <- c(-1,1,-1,1,-1) #1 for loadings that should be positive, -1 for loadings that should be negative

FiscalOmittedObs <- (1:nrow(FiscalMatrix))[apply (FiscalMatrix, 1, invalid)]
FiscalMatrix <- FiscalMatrix[-FiscalOmittedObs,]

# Preliminary principal component run
FiscalFactor <- factanal (~CorporateIncomeTax + IncomeTaxProductivity + PersonalIncomeTax
                          + VATproductivity + VATrate
                          , factors=1
                          , rotation="varimax"
                          , scores="regression"
                          , na.action=na.exclude
                          , data=FiscalMatrix)

FiscalFactor$loadings[,1]
FiscalFactor$uniquenesses

# VATrate has the wrong loading sign, but the rest are fine. Uniqueness still too high

FiscalFactanal <- data.frame (country=regIndex$Country[-FiscalOmittedObs], year=regIndex$Year[-FiscalOmittedObs], fiscal.scores=FiscalFactor$scores)
colnames(FiscalFactanal)[grep("Factor1", colnames(FiscalFactanal))] <- "fiscal.scores"
FiscalFactanal$id <- paste (FiscalFactanal$country, FiscalFactanal$year, sep="-")



############################
#### Trade Reform Index ####
############################

TradeMatrix <- regIndex[,is.element(colnames(regIndex), tradeVars)]
TradeLoadSigns <- c(-1,1) #1 for loadings that should be positive, -1 for loadings that should be negative

TradeOmittedObs <- (1:nrow(TradeMatrix))[apply (TradeMatrix, 1, invalid)]
TradeMatrix <- TradeMatrix[-TradeOmittedObs,]

# Preliminary principal component run
TradeFactor <- princomp (~AverageTariff + TradeDispersion  # Factanal won't run with 2 variables
                         , scores=TRUE, data=TradeMatrix, na.action=na.exclude)

TradeFactor$loadings
# Here all looks good

TradeFactanal <- data.frame (country=regIndex$Country[-TradeOmittedObs], year=regIndex$Year[-TradeOmittedObs], trade.scores=TradeFactor$scores[,1])
TradeFactanal$id <- paste (TradeFactanal$country, TradeFactanal$year, sep="-")


##############################
#### All factors together ####
##############################
FactorScores <- merge (FinanceFactanal, LaborFactanal, by="id", all=TRUE)
FactorScores <- merge (FactorScores, FiscalFactanal, by="id", all=TRUE)
FactorScores <- FactorScores[,!is.element(colnames(FactorScores), c("country.x","country.y","year.x","year.y"))]
FactorScores <- merge (FactorScores, TradeFactanal, by="id", all=TRUE)
FactorScores <- FactorScores[,!is.element(colnames(FactorScores), c("country.x","country.y","year.x","year.y"))]

LoraIndexes <- regIndex[, is.element(colnames(regIndex), c("id","FinLiberalIndex","LaborIndex","PrivatizationIndex","StructReformIndex","TaxReformIndex","TradeIndex"))]

FactorScores <- merge (FactorScores, LoraIndexes, by="id", all.x=TRUE)
cor (FactorScores[,-1], use="p")


########################################
#### All subindexes thrown together ####
########################################
AllMatrix <- regIndex[,is.element(colnames(regIndex), c(financeVars, laborVars, fiscalVars, tradeVars, "PrivatizationIndex", "StructReformIndex"))]
AllOmittedObs <- attr (na.omit(AllMatrix), "na.action")
AllMatrix <- na.omit (AllMatrix)
# Correlation matrix
round (cor (AllMatrix, method="k"), 2)

# Correlation matrix based on continuous variables
nonContinuous <- c("BankSupervQuality", "InterestRateLiberalization","HiringFlexibility","WorkHoursFlexibility")
round (cor (AllMatrix[,!is.element(colnames(AllMatrix), nonContinuous)], method="k"), 2)

# Correlations based on limited set of continuous variables
smallSet <- c("FinancialTransactionTax","ReserveRequirement","MinimumWage","SocSecurityTax","CorporateIncomeTax","PersonalIncomeTax","TradeDispersion","PrivatizationIndex")
round (cor (AllMatrix[,is.element(colnames(AllMatrix), smallSet)], method="k"), 2)


SomeFactor <- factanal (~FinancialTransactionTax + ReserveRequirement 
                        + MinimumWage + SocSecurityTax
                        + CorporateIncomeTax + PersonalIncomeTax
                        + AverageTariff + PrivatizationIndex
                       , factors=1
                       , rotation="varimax"
                       , scores="regression"
                       , na.action=na.exclude
                       , data=AllMatrix)
SomeFactor$loadings


AllFactor <- factanal (~FinancialTransactionTax + InterestRateLiberalization
                       + ReserveRequirement
                       + HiringFlexibility + JobTerminationCost + MinimumWage
                       + SocSecurityTax + WorkHoursFlexibility + CorporateIncomeTax        
                       + PersonalIncomeTax + VATrate + AverageTariff + TradeDispersion
                       + PrivatizationIndex  
                          , factors=1
                          , rotation="varimax"
                          , scores="regression"
                          , na.action=na.exclude
                          , data=AllMatrix)

AllFactor$loadings
AllMatrix$factor.scores <- AllFactor$scores

##########################################
#### Trying MLE graded response model ####
##########################################

OmittedObs <- attr (na.omit(AllMatrix), "na.action") # Note that we are eliminating all incomplete obs. In the future, it will be best to eliminate ONLY those observations for which we have absolutely no information
AllMatrix$country <- regIndex[-OmittedObs,grep("Country", colnames(regIndex))]
AllMatrix$year <- regIndex[-OmittedObs,grep("Year", colnames(regIndex))]

AllMatrix <- AllMatrix[!is.element(AllMatrix$country, c("TrinidadTobago", "Jamaica")),]
AllMatrix$country <- factor (AllMatrix$country)

# Ordered variables are: BankSupervQuality, WorkHoursFlexibility, HiringFlexibility, InterestRateLiberalization
continuousVars <- c("FinancialTransactionTax","ReserveRequirement","JobTerminationCost","MinimumWage"
                    ,"SocSecurityTax","CorporateIncomeTax","IncomeTaxProductivity","PersonalIncomeTax"
                    ,"VATrate","AverageTariff","TradeDispersion","PrivatizationIndex")

# The following functions apply kmeans to each continuous variable, and produce plots of the ratio 
# of between SS to total SS (we want to maximize this ratio, I guess)
findKmeans <- function (x) {
  obj2return <- list ()
  for (i in 1:6) {
    obj2return[[i]] <- kmeans(x, centers=i, nstart=20)
  }
  return (obj2return)
}

screeLikePlot <- function (x) {
  tmp <- c()
  for (i in 1:length(x)) {
    tmp[i] <- 100*(x[[i]]$betweenss/x[[i]]$totss)
  }
  return (tmp)
}

par (mfrow=c(3,4))
for (i in 1:length(continuousVars)) {
  temp <- findKmeans (AllMatrix[,grep(continuousVars[i], colnames(AllMatrix))])
  plot (screeLikePlot (temp), type="b", main=continuousVars[i])
}

# The previous analysis shows that 3 categories suffice for most variables, except perhaps trade dispersion
# (then also AverageTariff) and privatizationIndex, which has 4
optimalBinning <- function (x, cats=3) {
  clus <- kmeans (x, centers=cats, nstart=20)
  mns <- sapply ( split (x, clus$cluster), function (x) mean (unlist(x)))
  outVar <- order(mns)[clus$cluster]
  return (outVar)
}


# change2ordered <- function (x, lo, hi, change.polar=FALSE) {
#   # y <- ifelse (x < quantile(x, prob=0.33), 1, ifelse (x > quantile(x, prob=0.66), 3, 2))
#   y <- ifelse (x < lo, 1, ifelse (x > hi, 3, 2))
#   return (y)
# }
# Lo's and Hi's determined by visual inspection of density plots. I try to find three modes
# We want to code items so that higher values correspond to less government intrusion
# We need a better, more principled "binning" solution for these continuous values

# ResRequire   <- change2ordered (AllMatrix$ReserveRequirement, lo=20, hi=40)
# JobTermCost  <- change2ordered (AllMatrix$JobTerminationCost, lo=2, hi=4)
# MinWage      <- change2ordered (AllMatrix$MinimumWage, lo=0.5, hi=1)
# SocSecurity  <- change2ordered (AllMatrix$SocSecurityTax, lo=0.28, hi=0.45)
# CorporateTax <- ifelse (AllMatrix$CorporateIncomeTax < 0.25, 1,
#                         ifelse (AllMatrix$CorporateIncomeTax < 0.30 & AllMatrix$CorporateIncomeTax >= 0.25, 2,
#                                 ifelse (AllMatrix$CorporateIncomeTax >= 0.30 & AllMatrix$CorporateIncomeTax < 0.34, 3, 4)))
# PersonalTax  <- ifelse (AllMatrix$PersonalIncomeTax < 0.25, 1,
#                         ifelse (AllMatrix$PersonalIncomeTax < 0.30 & AllMatrix$PersonalIncomeTax >= 0.25, 2,
#                                 ifelse (AllMatrix$PersonalIncomeTax >= 0.30 & AllMatrix$PersonalIncomeTax < 0.35, 3, 4)))
# CorporateTax <- change2ordered (AllMatrix$CorporateIncomeTax, lo=0.2, hi=0.4)
# PersonalTax  <- change2ordered (AllMatrix$PersonalIncomeTax, lo=0.2, hi=0.4)
# IncomeTaxProd <- change2ordered (AllMatrix$IncomeTaxProductivity) # Should not be part of reform index
# VATprod      <- change2ordered (AllMatrix$VATproductivity)
# VATrate      <- change2ordered (AllMatrix$VATrate, lo=0.15, hi=0.25)
# avgTariff    <- change2ordered (AllMatrix$AverageTariff, lo=20, hi=60)
# tradeDisp    <- change2ordered (AllMatrix$TradeDispersion, lo=10, hi=20)

# ALERT HERE! FIND OUT WHY optimalBinning DOES NOT ALWAYS RETURN CORRECT ORDER
# Make sure that the plots return correct ordering
# Sign next to code suggests how item correlates with pro-state end
ResRequire   <- optimalBinning (AllMatrix$ReserveRequirement); plot (ResRequire ~ AllMatrix$ReserveRequirement) #+ (more and more used as macroprudential countercyclical tools; perhaps this is the best justification to aggregate them in broad categories)
JobTermCost  <- optimalBinning (AllMatrix$JobTerminationCost); plot (JobTermCost ~ AllMatrix$JobTerminationCost) #+
MinWage      <- optimalBinning (AllMatrix$MinimumWage); plot (MinWage ~ AllMatrix$MinimumWage) #+
SocSecurity  <- optimalBinning (AllMatrix$SocSecurityTax); plot (SocSecurity ~ AllMatrix$SocSecurityTax) #+
CorporateTax <- optimalBinning (AllMatrix$CorporateIncomeTax); plot (CorporateTax ~ AllMatrix$CorporateIncomeTax) #+
PersonalTax  <- optimalBinning (AllMatrix$PersonalIncomeTax); plot (PersonalTax ~ AllMatrix$PersonalIncomeTax) #+
VATrate      <- optimalBinning (AllMatrix$VATrate); plot (VATrate ~ AllMatrix$VATrate) #+
avgTariff    <- optimalBinning (AllMatrix$AverageTariff, cats=4); plot (avgTariff ~ AllMatrix$AverageTariff) #+
tradeDisp    <- optimalBinning (AllMatrix$TradeDispersion, cats=4); plot (tradeDisp ~ AllMatrix$TradeDispersion) #+
FinTrans     <- optimalBinning (AllMatrix$FinancialTransactionTax); plot (FinTrans ~ AllMatrix$FinancialTransactionTax) #+
Privatize    <- optimalBinning (AllMatrix$PrivatizationIndex, cats=4); plot (Privatize ~ AllMatrix$PrivatizationIndex) #-
Privatize    <- recode (Privatize, "1=4; 2=3; 3=2; 4=1") #+

BankSuperv <- recode (AllMatrix$BankSupervQuality, "0=1; 1=2; 2=3")
IntRate    <- recode (AllMatrix$InterestRateLiberalization, "0=4; 1=3; 3=1") #+ (flipped from original direction)
HireFlex   <- recode (AllMatrix$HiringFlexibility, "1=3; 3=1") #+  (flipped from original direction)
WorkFlex   <- AllMatrix$WorkHoursFlexibility #+ (unclear; it appears that larger numbers correspond to extra cost of allowing flexibility in working hours)

OrderData <- data.frame (ResRequire, JobTermCost, MinWage, SocSecurity, CorporateTax, PersonalTax
            , VATrate, avgTariff, tradeDisp, IntRate, FinTrans, Privatize, HireFlex, WorkFlex)  #BankSuperv, 
round (cor (OrderData, method="k"), 2)

GRModel <- grm(OrderData, IRT.param=TRUE)
NewOrderData <- factor.scores (GRModel, resp.patterns=OrderData, method="EB", return.MIvalues=TRUE)

data2plot <- data.frame (policy.score=NewOrderData$score.dat$z1*(-1), country=AllMatrix$country, year=AllMatrix$year)

setwd (graphicsPath)
# pdf ("grm1.pdf", h=7, w=9)
ggplot(data=subset (data2plot, is.element(country, c("Argentina","Bolivia","Brasil","Chile","Colombia","CostaRica"))), aes(x=year, y=policy.score, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")
# dev.off ()

# pdf ("grm2.pdf", h=7, w=9)
ggplot(data=subset (data2plot, is.element(country, c("DominicanRep","Ecuador","ElSalvador","Guatemala","Honduras"))), aes(x=year, y=policy.score, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")
# dev.off ()

# pdf ("grm3.pdf", h=7, w=9)
ggplot(data=subset (data2plot, is.element(country, c("Mexico","Nicaragua","Paraguay","Peru","Uruguay","Venezuela"))), aes(x=year, y=policy.score, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")
# dev.off ()

#######################
#### Try Bayes GRM ####
#######################
setwd (workingPath)
source ("BayesGRMmodel.R")

Y <- as.matrix (OrderData) # Matrix with ordered responses to policy items

N <- nrow(OrderData)   # Number of country-year observations
J <- ncol(OrderData)   # Number of items
K <- as.numeric (apply (Y, 2, max)) # Number of categories in each item j

# Build Kappa matrix to hold values
# Items may have different number of categories, so we need to fix at 0 all cutpoints that won't be needed
# For example, if the maximum number of categories is 5 (needing 4 cutpoints), but an item only has 3 categories
# (needing 2 cutpoints), the row for the 3-category item should be NA, NA, 0, 0 (the NAs will store the MCMC
# iterations)

kappa.matrix <- matrix (NA, nrow=J, ncol=(max(K)-1))
for (i in 1:length(K)){
   if (K[i] > ncol(kappa.matrix)) { next } else {
      kappa.matrix[i,K[i]:ncol(kappa.matrix)] <- 0
   }
}

# Priors
s.alpha <- 4
s.kappa <- 4
m.alpha <- 0
m.kappa <- 0

jags.parameters <- c("theta","deviance","alpha","kappa")

Data=list(Y=Y, J=J, N=N, K=K
          , s.alpha=s.alpha, s.kappa=s.kappa
          , m.alpha=m.alpha, m.kappa=m.kappa
          , kappa=kappa.matrix)
jags.data <- dump.format(Data) # 

build.kappa.inits <- function (input.mat=kappa.matrix, n.cat=K) { 
  # input.mat contains the matrix that we will pass as data, following advice in Curtis 2010, p.9
  # n.cat is a vector with the number of categories for each item
  mat.inits <- matrix (NA, ncol=ncol(input.mat), nrow=nrow(input.mat))
  for (i in 1:nrow(mat.inits)) {
    mat.inits[i,1:(K[i]-1)] <- rnorm (K[i]-1,0,1) 
  }
  return (mat.inits)
}


jags.inits <- function()
{
  dump.format(
    list(
      theta=rnorm(N,0,2)
      , alpha=runif(J,-2,0)  # To coincide with negative priors
      , kappa.star=build.kappa.inits ()
       #, beta=rnorm(1,0,2)
    ) )
}

jags.model <- run.jags( model=BayesGRM
                        , monitor=jags.parameters
                        , inits=list(jags.inits(), jags.inits())
                        , n.chains=2
                        , method="parallel"
                        , data=jags.data
                        , adapt=1000
                        , thin=20, burnin=20000, sample=500
                        #, thin=1, burnin=10, sample=50
                        , summarise=FALSE
                        , plots=FALSE )

PoolChains <- mcmc.list (as.mcmc (jags.model[[1]][[1]]), as.mcmc (jags.model[[1]][[2]]))
# save (PoolChains, file="BayesGRMmcmcChains.RData")
load ("BayesGRMmcmcChains.RData")

# Traceplots of a random set of parameters
mcmcplot (PoolChains, dir="~/Dropbox/ChainOfResponsiveness/Versions of SPI/traceplots/"
          , random=10
          , parms=jags.parameters
          , filename="traceplotsBayesGRM")

print (gelman.diag  (PoolChains, multivariate=FALSE))



Theta <- rbind (PoolChains[[1]][,grep("theta",  colnames (PoolChains[[1]]))],
                PoolChains[[2]][,grep("theta",  colnames (PoolChains[[2]]))])

Kappa <- rbind (PoolChains[[1]][,grep("kappa",  colnames (PoolChains[[1]]))],
                PoolChains[[2]][,grep("kappa",  colnames (PoolChains[[2]]))])

Alpha <- rbind (PoolChains[[1]][,grep("alpha",  colnames (PoolChains[[1]]))],
                PoolChains[[2]][,grep("alpha",  colnames (PoolChains[[2]]))])

Deviance <- c (PoolChains[[1]][,grep("deviance",  colnames (PoolChains[[1]]))],
               PoolChains[[2]][,grep("deviance",  colnames (PoolChains[[2]]))])

# Plot discrimination coefficients
Alpha.Quants <- matrix (NA, nrow=5, ncol=ncol (Alpha))
for (r in 1:ncol(Alpha)){
  Alpha.Quants[,r] <- quantile (Alpha[,r], prob=c(0.025,0.1,0.5,0.9,0.975))
}
colnames (Alpha.Quants) <- colnames (Alpha)

etiquetas <- colnames (OrderData)

#setwd (graphicsPath)
#pdf ( paste(graphPath, "unmodeledFixedInflation.pdf", sep=""), h=5, w=7)
  par (las=1, mar=c(3.5,9,2,1), mfrow=c(1,1), cex.main=1)
  lo.x <- min (Alpha.Quants) - 0.05
  hi.x <- 0 
  plot (c(lo.x, hi.x)
        , c(1, ncol(Alpha.Quants))
        , type="n", xlab=""
        , ylab="", axes=F)
  axis(1, cex=0.9)
  mtext (1, line=2, text="Coefficient (80% and 95% HPDs)")
  # mtext (3, line=0, text="Fixed coefficients")
  mtext (text=etiquetas, side=2, at=c(1:ncol(Alpha.Quants)), cex=0.9)
  points (xy.coords(Alpha.Quants[3,], 1:ncol(Alpha.Quants)), pch=19)
  segments (x0=Alpha.Quants[2,]
            , x1=Alpha.Quants[4,]
            , y0=seq(1, ncol(Alpha.Quants), by=1)
            , y1=seq(1, ncol(Alpha.Quants), by=1), lwd=6)
  segments (x0=Alpha.Quants[1,]
            , x1=Alpha.Quants[5,]
            , y0=seq(1, ncol(Alpha.Quants), by=1)
            , y1=seq(1, ncol(Alpha.Quants), by=1), lwd=2)
  #dev.off ()


# Plot reform scores, by country, including uncertainty bounds
# Also obtain point estimates of reform scores, to compare against plain-vanilla graded response model
Theta.Quants <- matrix (NA, nrow=5, ncol=ncol (Theta))
for (r in 1:ncol(Theta)){
  Theta.Quants[,r] <- quantile (Theta[,r], prob=c(0.025,0.1,0.5,0.9,0.975))
}
colnames (Theta.Quants) <- paste (AllMatrix$country, AllMatrix$year, sep="_")
data2plot$Bayes.policy.scores <- Theta.Quants[3,]
data2plot$Bayes.policy.lo <- Theta.Quants[2,]
data2plot$Bayes.policy.hi <- Theta.Quants[4,]
data2plot$factor.scores <- AllMatrix$factor.scores*(-1)
data2plot$factor.scores <- (data2plot$factor.scores - mean(data2plot$factor.scores)) / sd(data2plot$factor.scores)
data2plot$StructReformIndex <- (AllMatrix$StructReformIndex - mean(AllMatrix$StructReformIndex)) / sd(AllMatrix$StructReformIndex)

cor (data2plot[,!is.element(colnames(data2plot), c("country","year"))]) # correlations are extremely high.

labelPais <- c("Argentina","Bolivia","Brazil","Chile"
               ,"Colombia","Costa Rica","Dominican Republic","Ecuador"
               ,"El Salvador","Guatemala","Honduras","Jamaica","Mexico"
               ,"Nicaragua","Paraguay","Peru","Uruguay","Venezuela")
yearLabels <- c(1985:2009)

# Lora includes Jamaica and TyT (I already got rid of TyT), and fail to include Panama
par (mar=c(4,3,1,1), las=3, mfrow=c(2,3))
for (i in 1:length (unique (data2plot$country))) {
   pais <- unique (data2plot$country)[i]
   dat  <- data2plot[data2plot$country==pais,]
   #pdf (paste (graphicsPath, "policyScores", pais, ".pdf", sep=""), h=5, w=9)
   # par (mar=c(4,3,1,1), las=3, mfrow=c(1,1))
   plot (c(1, length(yearLabels)), c(-3.5, 3.5)
         , type="n", axes=F, ylab="", xlab="", main=labelPais[i])
   axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
   axis (2)
   #mtext (side=2, line=2, text="Pro Market <--- Mood ---> Pro State")
   points (xy.coords (dat$year-1984, dat$Bayes.policy.scores), pch=19)
   points (xy.coords (dat$year-1984, dat$factor.scores), pch=4)
   points (xy.coords (dat$year-1984, dat$StructReformIndex), pch=19, col="red")
   points (xy.coords (dat$year-1984.3, dat$policy.score), pch=19, col="blue")
      segments (y0=dat$Bayes.policy.lo
                , y1=dat$Bayes.policy.hi
                , x0=dat$year-1984
                , x1=dat$year-1984
                , lwd=3)
   legend ("bottomleft", legend=c("Bayes GRM","ML GRM", "Lora", "FA")
           , pch=c(19, 19, 19, 4), col=c("black","blue","red","black")
           , bty="n")
   #dev.off()
}











#### Old Code ####
# Change AllMatrix data into "change" data
source ("~/Dropbox/New2Dropbox/research/R_Latex_help/function_lags_leads.R")

lags <- paneldata.lags (AllMatrix, "country", "year", lagvars=colnames (AllMatrix)[-c(17:18)], lags=1)
vars2diff <- colnames(AllMatrix)[-c(grep("country", colnames(AllMatrix)), grep("year", colnames(AllMatrix)))]

Changes <- data.frame (matrix (NA, ncol=ncol(lags), nrow=nrow(lags)))
colnames (Changes) <- paste (vars2diff, "ch", sep=".")
for (i in 1:length(vars2diff)) {
  indic <- vars2diff[i]
  Changes[,i] <- AllMatrix[ ,grep (indic, colnames(AllMatrix))] - lags[,grep (indic, colnames(lags))]
}
Changes$country <- AllMatrix$country
Changes$year <- AllMatrix$year

Changes <- na.omit (Changes)
cor (Changes[,-c(17,18)])

ChangeFactor <- factanal (~BankSupervQuality.ch + FinancialTransactionTax.ch +
                         InterestRateLiberalization.ch + ReserveRequirement.ch +
                         HiringFlexibility.ch + JobTerminationCost.ch + 
                         MinimumWage.ch + SocSecurityTax.ch + 
                         WorkHoursFlexibility.ch + CorporateIncomeTax.ch +
                         IncomeTaxProductivity.ch + PersonalIncomeTax.ch +
                         VATproductivity.ch + VATrate.ch + AverageTariff.ch +
                         TradeDispersion.ch  
                       , factors=3
                       , rotation="varimax"
                       , scores="regression"
                       , na.action=na.exclude
                       , data=Changes)

ChangeFactor$loadings






  ggplot(data=subset (TradeScores, is.element(country, c("Argentina","Bolivia","Brasil","Chile","Colombia","CostaRica"))), aes(x=year, y=tradeScores, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")

ggplot(data=subset (FinanceScores, is.element(country, c("DominicanRep","Ecuador","ElSalvador","Guatemala","Honduras"))), aes(x=year, y=financeScores, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")

ggplot(data=subset (LaborScores, is.element(country, c("Mexico","Nicaragua","Paraguay","Peru","Uruguay","Venezuela"))), aes(x=year, y=laborScores, color=country)) +
  geom_line() + # facet_grid(variable ~ .) + # use when values are not on same scale
  ggtitle("Reform scores")



#####################################
#### Start Bayes factor analysis ####
#####################################

# Quinn's priors:
#``The continuous variables are standardized to have mean 0 and standard deviation 1. As result I constrain the elements of lambda_1 corresponding to these variables to 0. The prior mean of each element of Lambda is assumed to be 0 (before any truncation) and the prior precision is assumed to be 0.25 (again, before any truncation). To help identify the model I constrain the element of lambda_2 for the independent judiciary variable to be negative. This implies that an independent judiciary is negatively associated with political-economic risk. To complete the prior specification, I assume that a0_{black-market} = a0_{productivity} = 0.001 and b0_{black-market} = b0_{productivity} = 0.001. This is a relatively uninformative prior for the error variances.''


################################
#### Financial Reform Index ####
################################
# Declare variables as ordered factors
FinanceMatrix$BankSupervQuality <- as.ordered(FinanceMatrix$BankSupervQuality)
FinanceMatrix$InterestRateLiberalization <- as.ordered(FinanceMatrix$InterestRateLiberalization)
FinanceMatrix$FinancialTransactionTax <- (FinanceMatrix$FinancialTransactionTax - mean(FinanceMatrix$FinancialTransactionTax)) / sd (FinanceMatrix$FinancialTransactionTax)
FinanceMatrix$ReserveRequirement <- (FinanceMatrix$ReserveRequirement - mean(FinanceMatrix$ReserveRequirement)) / sd (FinanceMatrix$ReserveRequirement)
# Note that we have standardized continuous variables, which then requires to fix the first parameter in the model (the difficulty parameter) at 0

FinanceMixFactor1 <- MCMCmixfactanal (~BankSupervQuality + FinancialTransactionTax + InterestRateLiberalization + ReserveRequirement
                                      , data=FinanceMatrix, factors=1
                                      , lambda.constraints=list( FinancialTransactionTax=list(1,0),
                                                                 ReserveRequirement=list(1,0),#
                                                                 InterestRateLiberalization=list(2,"+"))
                                      , lambda.start=NA
                                      , l0=0
                                      , L0=.25
                                      , a0=1
                                      , b0=0.5
                                      , tune=.25
                                      , store.scores=TRUE
                                      , std.mean=TRUE
                                      , std.var=TRUE
                                      , seed=2016
                                      , verbose=500
                                      , burnin=100000
                                      , mcmc=100000
                                      , thin=100)

FinanceMixFactor2 <- MCMCmixfactanal (~BankSupervQuality + FinancialTransactionTax + InterestRateLiberalization + ReserveRequirement
                                      , data=FinanceMatrix, factors=1
                                      , lambda.constraints=list( FinancialTransactionTax=list(1,0),
                                                                 ReserveRequirement=list(1,0),#
                                                                 InterestRateLiberalization=list(2,"+"))
                                      , lambda.start=NA
                                      , l0=0
                                      , L0=.25
                                      , a0=1
                                      , b0=0.5
                                      , tune=.25
                                      , store.scores=TRUE
                                      , std.mean=TRUE
                                      , std.var=TRUE
                                      , seed=1016
                                      , verbose=500
                                      , burnin=100000
                                      , mcmc=100000
                                      , thin=100)

FinanceChains <- list (FinanceMixFactor1, FinanceMixFactor2)
# save (FinanceChains, file="financeMCMCfactanal.RData")
load ("financeMCMCfactanal.RData")

gelman.diag  (FinanceChains, multivariate=TRUE)
mcmcplot (FinanceChains, dir="~/Downloads/"
          , filename="traceplotsMCMCfinance"
          , regex=c("Lambda","Psi","gamma"))

FinanceMixFactor <- combine.mcmc(FinanceChains)
summary (FinanceMixFactor)

FinanceExtraParams <- FinanceMixFactor[,-grep("phi", colnames(FinanceMixFactor))]
# For analysis of extra params:
# phi are the latent scores
# Lambda are factor loadings
# Psi are uniquenesses
# gamma are cutpoints
scores <- colMeans (FinanceMixFactor[,grep("phi", colnames(FinanceMixFactor))]) # Keeping only point estimate
FinanceScores <- data.frame (country=regIndex$Country[-FinanceOmittedObs], year=regIndex$Year[-FinanceOmittedObs], scores=scores)
FinanceScores$FinanceFactanal <- FinanceFactor$scores
FinanceScores <- FinanceScores[!is.element(FinanceScores$country, c("Jamaica", "TrinidadTobago")),]
colnames (FinanceScores)[grep ("Factanal", colnames(FinanceScores))] <- "financeFactanal"
colnames (FinanceScores)[grep ("scores", colnames(FinanceScores))] <- "financeScores"
FinanceScores$id <- paste (FinanceScores$country, FinanceScores$year, sep="-")

# summary representations of all other parameters
tableFinance <- t(apply (FinanceExtraParams, 2, quantile, prob=c(0.5,0.05,0.95)))
stargazer (tableFinance, title="Financial Structural Reform index, ancillary parameters"
           , label="T:financeParams"
           , style="ajps"
           , column.labels=c("Parameter","Median","5th perc.","95th perc.")
           , notes="Statistics are median and 90\\% highest posterior density interval"
           , notes.align="l"
           , align=TRUE
           , digits=2)

###################################
#### Labor Market Reform Index ####
###################################
# Declare variables as ordered factors
LaborMatrix$HiringFlexibility <- as.ordered(LaborMatrix$HiringFlexibility)
LaborMatrix$WorkHoursFlexibility <- as.ordered(LaborMatrix$WorkHoursFlexibility)
LaborMatrix$JobTerminationCost <- (LaborMatrix$JobTerminationCost - mean(LaborMatrix$JobTerminationCost)) / sd(LaborMatrix$JobTerminationCost)
LaborMatrix$MinimumWage <- (LaborMatrix$MinimumWage - mean(LaborMatrix$MinimumWage, na.rm=T)) / sd (LaborMatrix$MinimumWage, na.rm=T)
LaborMatrix$SocSecurityTax <- (LaborMatrix$SocSecurityTax - mean (LaborMatrix$SocSecurityTax)) / sd(LaborMatrix$SocSecurityTax)

LaborMixFactor1 <- MCMCmixfactanal (~HiringFlexibility + JobTerminationCost + MinimumWage
                                    + SocSecurityTax + WorkHoursFlexibility
                                    , data=LaborMatrix, factors=1
                                    , lambda.constraints=list(
                                      HiringFlexibility=list(2,"+")
                                      , JobTerminationCost=list(1,0)
                                      , MinimumWage=list(1,0)
                                      , SocSecurityTax=list(1,0))
                                    , lambda.start=NA
                                    , l0=0
                                    , L0=.25
                                    , a0=1
                                    , b0=0.5
                                    , tune=.25
                                    , store.scores=TRUE
                                    , std.mean=TRUE
                                    , std.var=TRUE
                                    , seed=2016
                                    , verbose=500
                                    , burnin=100000
                                    , mcmc=100000
                                    , thin=100)

LaborMixFactor2 <- MCMCmixfactanal (~HiringFlexibility + JobTerminationCost + MinimumWage
                                    + SocSecurityTax + WorkHoursFlexibility
                                    , data=LaborMatrix, factors=1
                                    , lambda.constraints=list(
                                      HiringFlexibility=list(2,"+")
                                      , JobTerminationCost=list(1,0)
                                      , MinimumWage=list(1,0)
                                      , SocSecurityTax=list(1,0))
                                    , lambda.start=NA
                                    , l0=0
                                    , L0=.25
                                    , a0=1
                                    , b0=0.5
                                    , tune=.25
                                    , store.scores=TRUE
                                    , std.mean=TRUE
                                    , std.var=TRUE
                                    , seed=1016
                                    , verbose=500
                                    , burnin=100000
                                    , mcmc=100000
                                    , thin=100)


LaborChains <- list (LaborMixFactor1, LaborMixFactor2)
# save (LaborChains, file="laborMCMCfactanal.RData")
load (file="laborMCMCfactanal.RData")

gelman.diag  (LaborChains, multivariate=TRUE)
mcmcplot (LaborChains, dir="~/Downloads/"
          , filename="traceplotsMCMClabor"
          , regex=c("Lambda","Psi","Gamma"))

LaborMixFactor <- combine.mcmc(LaborChains)
summary (LaborMixFactor)

LaborExtraParams <- LaborMixFactor[,-grep("phi", colnames(LaborMixFactor))]
scores <- colMeans (LaborMixFactor[,grep("phi", colnames(LaborMixFactor))]) # Keeping only point estimate
LaborScores <- data.frame (country=regIndex$Country[-LaborOmittedObs], year=regIndex$Year[-LaborOmittedObs], scores=scores)
LaborScores$LaborFactanal <- LaborFactor$scores
LaborScores <- LaborScores[!is.element(LaborScores$country, c("Jamaica", "TrinidadTobago")),]
colnames (LaborScores)[grep ("Factanal", colnames(LaborScores))] <- "laborFactanal"
colnames (LaborScores)[grep ("scores", colnames(LaborScores))] <- "laborScores"
LaborScores$id <- paste (LaborScores$country, LaborScores$year, sep="-")

# summary representations of all other parameters
tableLabor <- t(apply (LaborExtraParams, 2, quantile, prob=c(0.5,0.05,0.95)))
stargazer (tableLabor, title="Labor Structural Reform index, ancillary parameters"
           , label="T:laborParams"
           , style="ajps"
           , column.labels=c("Parameter","Median","5th perc.","95th perc.")
           , notes="Statistics are median and 90\\% highest posterior density interval"
           , notes.align="l"
           , align=TRUE
           , digits=2)




#############################
#### Fiscal Reform Index ####
#############################
FiscalMixFactor1 <- MCMCmixfactanal (~CorporateIncomeTax + IncomeTaxProductivity + PersonalIncomeTax
                                     + VATproductivity + VATrate
                                     , data=FiscalMatrix, factors=1
                                     , lambda.constraints=list(
                                       CorporateIncomeTax=list(2,"-")
                                       , PersonalIncomeTax=list(2,"-"))
                                     , lambda.start=NA
                                     , l0=0
                                     , L0=.25
                                     , a0=1
                                     , b0=0.5
                                     , tune=.25
                                     , store.scores=TRUE
                                     , std.mean=TRUE
                                     , std.var=TRUE
                                     , seed=2016
                                     , verbose=500
                                     , burnin=100000
                                     , mcmc=100000
                                     , thin=100)

FiscalMixFactor2 <- MCMCmixfactanal (~CorporateIncomeTax + IncomeTaxProductivity + PersonalIncomeTax
                                     + VATproductivity + VATrate
                                     , data=FiscalMatrix, factors=1
                                     , lambda.constraints=list(
                                       CorporateIncomeTax=list(2,"-")
                                       , PersonalIncomeTax=list(2,"-"))
                                     , l0=0
                                     , L0=.25
                                     , a0=1
                                     , b0=0.5
                                     , tune=.25
                                     , lambda.start=NA
                                     , store.scores=TRUE
                                     , std.mean=TRUE
                                     , std.var=TRUE
                                     , seed=1016
                                     , verbose=500
                                     , burnin=100000
                                     , mcmc=100000
                                     , thin=100)

# This one needs way more scans, and more thinned, but it is working
FiscalChains <- list (FiscalMixFactor1, FiscalMixFactor2)
# save (FiscalChains, file="fiscalMCMCfactanal.RData")
gelman.diag  (FiscalChains, multivariate=TRUE)
mcmcplot (FiscalChains, dir="~/Downloads/"
          , filename="traceplotsMCMCfiscal"
          , regex=c("Lambda","Psi","Gamma"))

FiscalMixFactor <- combine.mcmc(FiscalChains)
summary (FiscalMixFactor)

FiscalExtraParams <- FiscalMixFactor[,-grep("phi", colnames(FiscalMixFactor))]
scores <- colMeans (FiscalMixFactor[,grep("phi", colnames(FiscalMixFactor))]) # Keeping only point estimate
FiscalScores <- data.frame (country=regIndex$Country[-FiscalOmittedObs], year=regIndex$Year[-FiscalOmittedObs], scores=scores)
FiscalScores$FiscalFactanal <- FiscalFactor$scores
FiscalScores <- FiscalScores[!is.element(FiscalScores$country, c("Jamaica", "TrinidadTobago")),]
colnames (FiscalScores)[grep ("Factanal", colnames(FiscalScores))] <- "fiscalFactanal"
colnames (FiscalScores)[grep ("scores", colnames(FiscalScores))] <- "fiscalScores"

FiscalScores$id <- paste (FiscalScores$country, FiscalScores$year, sep="-")

# summary representations of all other parameters
tableFiscal <- t(apply (FiscalExtraParams, 2, quantile, prob=c(0.5,0.05,0.95)))
stargazer (tableFiscal, title="Fiscal Structural Reform index, ancillary parameters"
           , label="T:fiscalParams"
           , style="ajps"
           , column.labels=c("Parameter","Median","5th perc.","95th perc.")
           , notes="Statistics are median and 90\\% highest posterior density interval"
           , notes.align="l"
           , align=TRUE
           , digits=2)




############################
#### Trade Reform Index ####
############################

TradeMatrix$AverageTariff <- (TradeMatrix$AverageTariff - mean(TradeMatrix$AverageTariff, na.rm=T))/ sd(TradeMatrix$AverageTariff, na.rm=T)
TradeMatrix$TradeDispersion <- (TradeMatrix$TradeDispersion - mean(TradeMatrix$TradeDispersion, na.rm=T))/ sd(TradeMatrix$TradeDispersion, na.rm=T)

TradeMixFactor1 <- MCMCfactanal (~AverageTariff + TradeDispersion
                                 , data=TradeMatrix, factors=1
                                 , lambda.constraints=list(
                                   AverageTariff=list(1,"-"))
                                 , lambda.start=NA
                                 , l0=0
                                 , L0=.25
                                 , a0=1
                                 , b0=0.5
                                 , tune=.25
                                 , store.scores=TRUE
                                 , std.var=TRUE
                                 , seed=2016
                                 , verbose=500
                                 , burnin=100000
                                 , mcmc=100000
                                 , thin=100)

TradeMixFactor2 <- MCMCfactanal (~AverageTariff + TradeDispersion
                                 , data=TradeMatrix, factors=1
                                 , lambda.constraints=list(
                                   AverageTariff=list(1,"-"))
                                 , lambda.start=NA
                                 , l0=0
                                 , L0=.25
                                 , a0=1
                                 , b0=0.5
                                 , tune=.25
                                 , store.scores=TRUE
                                 , std.var=TRUE
                                 , seed=1016
                                 , verbose=500
                                 , burnin=100000
                                 , mcmc=100000
                                 , thin=100)


TradeChains <- list (TradeMixFactor1, TradeMixFactor2)
# save (TradeChains, file="tradeMCMCfactanal.RData")
load (file="tradeMCMCfactanal.RData")

gelman.diag  (TradeChains, multivariate=TRUE)
mcmcplot (TradeChains, dir="~/Downloads/"
          , filename="traceplotsMCMCtrade"
          , regex=c("Lambda","Psi"))
TradeMixFactor <- combine.mcmc(TradeChains)
summary (TradeMixFactor)


TradeExtraParams <- TradeMixFactor[,-grep("phi", colnames(TradeMixFactor))]
scores <- colMeans (TradeMixFactor[,grep("phi", colnames(TradeMixFactor))]) # Keeping only point estimate
obs <- unlist (strsplit (names(scores), split="_"))
remove <- c("phi","1")
obs <- c(1, as.numeric (obs[! obs %in% remove]))


TradeScores <- data.frame (country=regIndex$Country[obs], year=regIndex$Year[obs], scores=scores)
TradeScores <- TradeScores[!is.element(TradeScores$country, c("Jamaica", "TrinidadTobago")),]
colnames (TradeScores)[grep ("scores", colnames(TradeScores))] <- "tradeScores"
TradeScores$id <- paste (TradeScores$country, TradeScores$year, sep="-")

# summary representations of all other parameters
tableTrade <- t(apply (TradeExtraParams, 2, quantile, prob=c(0.5,0.05,0.95)))
stargazer (tableTrade, title="Labor Structural Reform index, ancillary parameters"
           , label="T:laborParams"
           , style="ajps"
           , column.labels=c("Parameter","Median","5th perc.","95th perc.")
           , notes="Statistics are median and 90\\% highest posterior density interval"
           , notes.align="l"
           , align=TRUE
           , digits=2)




Scores <- merge (FinanceScores, LaborScores, by="id", all=TRUE)
Scores <- merge (Scores, FiscalScores, by="id", all=TRUE)
Scores <- Scores[,!is.element(colnames(Scores), c("country.x","country.y","year.x","year.y"))]
Scores <- merge (Scores, TradeScores, by="id", all=TRUE)
Scores <- Scores[,!is.element(colnames(Scores), c("country.x","country.y","year.x","year.y"))]
cor (Scores[,-1], use="p")


##############################################
#### Add original Morley and Lora indexes ####
##############################################

ScoresExp <- merge (regIndex[, c(1:3, grep ("Index", colnames (regIndex)))], Scores, by="id", all=TRUE)

# Print a series of correlations
# Financial reform indices
cor (ScoresExp[, is.element (colnames(ScoresExp), c("FinLiberalIndex","MPfinancialIndex","financeScores","financeFactanal"))], use="p")
# Labor market reform index
cor (ScoresExp[, is.element (colnames(ScoresExp), c("LaborIndex","laborScores","laborFactanal"))], use="p")
# Privatization index
cor (ScoresExp[, is.element (colnames(ScoresExp), c("PrivatizationIndex","MPprivatizationIndex"))], use="p")
# Tax reform index
cor (ScoresExp[, is.element (colnames(ScoresExp), c("TaxReformIndex","MPtaxIndex","fiscalScores","fiscalFactanal"))], use="p")
# Trade reform index
cor (ScoresExp[, is.element (colnames(ScoresExp), c("TradeIndex","MPtradeIndex","tradeScores"))], use="p")






