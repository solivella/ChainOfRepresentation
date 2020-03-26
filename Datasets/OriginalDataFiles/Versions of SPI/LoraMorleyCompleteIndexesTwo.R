###################################################
# This script inputs, analyzes, and scales ALL
# raw indexes from the updated version of Lora
# and Morley, plus additions from Fernandez, Klein,
# Rebucci, Schindler, Uribe (2015).
# These updated and expanded policy dataset was
# prepared by Yang Zhang (October 29, 2016)
#
# GR: October 30, 2016
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
setwd ("~/Dropbox/ChainOfResponsiveness/Versions of SPI/LoraSEPI/updateddataset/")
workingPath  <- getwd ()

AllData <- read.table ("LoraIndicesUpdated.txt", sep="\t", header=TRUE
                       , fileEncoding="UTF-16")

colnames (AllData)[grep("kaopen", colnames(AllData))] <- "CapitalOpenness"
colnames (AllData)[grep("TariffDispersion", colnames(AllData))] <- "TradeDispersion"
colnames (AllData)[grep("TransactionTax", colnames(AllData))] <- "FinancialTransactionTax"
colnames (AllData)[grep("^VAT$", colnames(AllData))] <- "VATrate"
colnames (AllData)[grep("Privatization1", colnames(AllData))] <- "PrivatizationIndex"
colnames (AllData)[grep("Privatization2", colnames(AllData))] <- "PrivatizationIndex2"
#colnames (AllData)[grep("FiringCost", colnames(AllData))] <- "JobTerminationCost"
colnames (AllData)[grep("NewFiringCost", colnames(AllData))] <- "JobTerminationCost"
colnames (AllData)[grep("SocialSecurityCost", colnames(AllData))] <- "SocSecurityTax"
colnames (AllData)[grep("WorkingHoursFlexibility", colnames(AllData))] <- "WorkHoursFlexibility"

# Reserve requirement: We have two different sources, Lora, and another paper found by Yang.
# They do not fully overlap, and they correlate only at 0.25. Before turning this into a 
# categorical variable, we could simply use predicted scores from principal components

pc <- princomp (~NewRequiredReserve + RequiredReserve, data=AllData, scale=FALSE, na.action=na.exclude)
z <- (AllData$RequiredReserve-pc$center[grep("^RequiredReserve", names(pc$center))]) * pc$loadings[grep("^RequiredReserve", rownames(pc$loadings)),1]
y <- (AllData$NewRequiredReserve - pc$center[grep("NewRequiredReserve", names(pc$center))]) * pc$loadings[grep("NewRequiredReserve", rownames(pc$loadings)),1]
plot (z~y)
# This does not work that well. Instead, we have used data from Federico, Vegh, Vulletin for:
# Arg, Brz, Chi, Col, Costa Rica, Ecu, Pan, Per, DomRep, Uru, Ven.
# We have used the original Lora data for:
# Bol, El Salvador, Guatemala, Honduras, Mex, Nic, Par
colnames (AllData)[grep("FinalRequiredReserve", colnames(AllData))] <- "ReserveRequirement"

# The following items are from IMF's AREAER. The problem is that they exist only
# since 1995 in their current form. AREAER has data from before this period, but
# they collect data differently. In the end, we'll use Chinn-Itto's kaopen (now
# renamed CapitalOpenness) as a single measure of capital openness in lieu of all of
# this
AllData$Equity  <- AllData$EquityInflow + AllData$EquityOutflow
AllData$Bond  <- AllData$BondInflow + AllData$BondOutflow
AllData$MoneyMkt  <- AllData$MoneyMktInflow + AllData$MoneyMktOutflow 
AllData$CollInvest  <- AllData$CollInvestInflow + AllData$CollInvestOutflow
AllData$Derivatives  <- AllData$DerivativesInflow + AllData$DerivativesOutflow
AllData$Credit  <- AllData$CreditInflow + AllData$CreditOutflow
AllData$FinancialCredit  <- AllData$FinancialCreditInflow + AllData$FinancialCreditOutflow
AllData$Guarantees  <- AllData$GuaranteesInflow + AllData$GuaranteesOutflow
AllData$DirectInvest  <- AllData$DirectInvestInflow + AllData$DirectInvestOutflow
AllData$RealEstate  <- AllData$RealEstateInflow + AllData$RealEstateOutflow
# Here we reduce all of the AREAER data into only three categories
AllData$CapitalControl <- AllData$Equity + AllData$Bond + AllData$MoneyMkt + AllData$CollInvest + AllData$Derivatives + AllData$DualExchangeRate
AllData$Credits <- AllData$Credit + AllData$FinancialCredit 
AllData$Invest <- AllData$Guarantees + AllData$DirectInvest + AllData$RealEstate

pdf (paste(graphicsPath, "resRequirementExample.pdf", sep=""))
labs <- c("ARG","BOL","BRZ","CHI","COL","CRI","DOM","ECU","SAL","GUA","HON","MEX","NIC","PAN","PAR","PER","URU","VEN")
par (mfrow=c(1,1), mar=c(3,3,1,1))
plot (AllData$ReserveRequirement, axes=F
      , xlim=c(0,nrow(AllData))
      , ylim=c(0,100)
      , main=""
      , xlab="", ylab="Reserve requirement rate", pch=19, cex=0.8)
axis (1, at=c(0, nrow(AllData)), labels=c(NA,NA))
axis (2)
lineHere <- rep (NA, 540);
for (i in 2:540){
  lineHere[i] <- ifelse (!identical(AllData$country[i], AllData$country[i-1]), 1, 0)
}
abline (v=which (lineHere==1) - 1.5)
lineHere[1] <- 1
text (y=100, x=which (lineHere==1) + 12, labels=labs, cex=0.8)
# abline (h=c(20,40), lty=2)
dev.off ()

###################################
#### MLE graded response setup ####
###################################

# Ordered variables are: BankSupervQuality, WorkHoursFlexibility, HiringFlexibility, InterestRateLiberalization
continuousVars <- c("AverageTariff","TradeDispersion"
                    ,"ReserveRequirement","FinancialTransactionTax"
                    ,"VATrate","PersonalIncomeTax","CorporateIncomeTax"
                    ,"PrivatizationIndex","PrivatizationIndex2","JobTerminationCost"
                    ,"SocSecurityTax","NewMinimumWage","Equity","Bond","MoneyMkt" # Switched NewMinimumWage for MinimumWage
                    ,"CollInvest","Derivatives","Credit"
                    ,"RealEstate","CapitalOpenness")

orderedVars <- c("InterestRateLiberalization","BankSupervision","^FinancialCredit$"
                 ,"HiringFlexibility","WorkHoursFlexibility","Guarantees","DirectInvest"
                 , colnames(AllData)[grep("Inflow", colnames(AllData))]
                 , colnames(AllData)[grep("Outflow", colnames(AllData))])

# The following functions apply kmeans to each continuous variable, and produce plots of the ratio 
# of between SS to total SS (we want to maximize this ratio, I guess)
findKmeans <- function (x) {
   obj2return <- list ()
   for (i in 1:6) {
      obj2return[[i]] <- kmeans(na.omit(x), centers=i, nstart=20)
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
   temp <- findKmeans (AllData[,grep(continuousVars[i], colnames(AllData))])
   plot (screeLikePlot (temp), type="b", main=continuousVars[i])
}

# The previous analysis shows that 3 categories suffice for most variables,
# except perhaps trade dispersion
# (then also AverageTariff) and privatizationIndex and minimumwage, which has 4
optimalBinning <- function (x, cats=3) {
   clus <- kmeans (na.omit(x), centers=cats, nstart=20)
   y <- sort(clus$centers)[-cats] + diff(sort(clus$centers))/2
   if (length (y)==2) {
      mns <- ifelse ( is.na(x), NA, ifelse ( x <= y[1], 1, ifelse (x >= y[2], 3, 2)))
   } else {
      mns <- ifelse ( is.na(x), NA, ifelse ( x <= y[1], 1, ifelse ( y[1] < x & x <= y[2], 2, ifelse ( x > y[3], 4, 3))))
   }
   return (mns)
}

# Make sure that the plots return correct ordering
# Sign next to code suggests how item correlates with pro-state end;
# note, however, that we then go on and recode everything so that higher 
# values correspond to more market orientation.
par (mfrow=c(3,4))
capitalopenness <- optimalBinning (AllData$CapitalOpenness); plot (capitalopenness ~ AllData$CapitalOpenness)
equity <- optimalBinning (AllData$Equity)
equity <- recode (equity, "1=3; 3=1"); plot (equity ~ AllData$Equity) #+
bond <- optimalBinning (AllData$Bond)
bond <- recode (bond, "1=3; 3=1"); plot (bond ~ AllData$Bond) #+
moneymkt <- optimalBinning (AllData$MoneyMkt)
moneymkt <- recode (moneymkt, "1=3; 3=1"); plot (moneymkt ~ AllData$MoneyMkt) #+
collinvest <- optimalBinning (AllData$CollInvest)
collinvest <- recode (collinvest, "1=3; 3=1"); plot (collinvest ~ AllData$CollInvest) #+
derivatives <- optimalBinning (AllData$Derivatives)
derivatives <- recode (derivatives, "1=3; 3=1"); plot (derivatives ~ AllData$Derivatives) #+
credit <- optimalBinning (AllData$Credit)
credit <- recode (credit, "1=3; 3=1"); plot (credit ~ AllData$Credit) #+
financecredit <- optimalBinning (AllData$FinancialCredit)
financecredit <- recode (financecredit, "1=3; 3=1"); plot (financecredit ~ AllData$FinancialCredit) #+
directinvest <- optimalBinning (AllData$DirectInvest)
directinvest <- recode (directinvest, "1=3; 3=1"); plot (directinvest ~ AllData$DirectInvest) #+
guarantees <- optimalBinning (AllData$Guarantees)
guarantees <- recode (guarantees, "1=3; 3=1"); plot (guarantees ~ AllData$Guarantees) #+
realestate <- optimalBinning (AllData$RealEstate)
realestate <- recode (realestate, "1=3; 3=1"); plot (realestate ~ AllData$RealEstate) #+
capitalcontrol <- optimalBinning (AllData$CapitalControl)
capitalcontrol <- recode (capitalcontrol, "1=3; 3=1"); plot (capitalcontrol ~ AllData$CapitalControl) #+
credits <- optimalBinning (AllData$Credits)
credits <- recode (credits, "1=3; 3=1"); plot (credits ~ AllData$Credits) #+
invest <- optimalBinning (AllData$Invest)
invest <- recode (invest, "1=3; 3=1"); plot (invest ~ AllData$Invest) #+

ResRequire   <- optimalBinning (AllData$ReserveRequirement)
#(more and more used as macroprudential countercyclical tools; perhaps this is the best justification to aggregate them in broad categories)
ResRequire <- recode (ResRequire, "1=3; 3=1"); plot (ResRequire ~ AllData$ReserveRequirement) #+ 
JobTermCost  <- optimalBinning (AllData$JobTerminationCost)
JobTermCost  <- recode (JobTermCost, "1=3; 3=1"); plot (JobTermCost ~ AllData$JobTerminationCost) #+
SocSecurity  <- optimalBinning (AllData$SocSecurityTax)
SocSecurity <- recode (SocSecurity, "1=3; 3=1"); plot (SocSecurity ~ AllData$SocSecurityTax) #+
CorporateTax <- optimalBinning (AllData$CorporateIncomeTax)
CorporateTax <- recode (CorporateTax, "1=3; 3=1"); plot (CorporateTax ~ AllData$CorporateIncomeTax) #+
PersonalTax  <- optimalBinning (AllData$PersonalIncomeTax)
PersonalTax  <- recode (PersonalTax, "1=3; 3=1"); plot (PersonalTax ~ AllData$PersonalIncomeTax) #+
VATrate      <- optimalBinning (AllData$VATrate)
VATrate      <- recode (VATrate, "1=3; 3=1"); plot (VATrate ~ AllData$VATrate) #+
avgTariff    <- optimalBinning (AllData$AverageTariff, cats=4)
avgTariff    <- recode (avgTariff, "1=4; 2=3; 3=2; 4=1"); plot (avgTariff ~ AllData$AverageTariff) #+
tradeDisp    <- optimalBinning (AllData$TradeDispersion, cats=4)
tradeDisp    <- recode (tradeDisp, "1=4; 2=3; 3=2; 4=1"); plot (tradeDisp ~ AllData$TradeDispersion) #+
FinTrans     <- optimalBinning (AllData$FinancialTransactionTax); plot (FinTrans ~ AllData$FinancialTransactionTax) #+
MinWage      <- optimalBinning (AllData$NewMinimumWage, cats=4)
MinWage      <- recode (MinWage, "1=4; 2=3; 3=2; 4=1"); plot (MinWage ~ AllData$NewMinimumWage) #+
Privatize    <- optimalBinning (AllData$PrivatizationIndex, cats=4); plot (Privatize ~ AllData$PrivatizationIndex) #-
Privatize2   <- optimalBinning (AllData$PrivatizationIndex2, cats=4); plot (Privatize2 ~ AllData$PrivatizationIndex2) #-

BankSuperv <- recode (AllData$BankSupervision, "0=1; 1=2; 2=3"); plot (BankSuperv ~ AllData$BankSupervision) # 
IntRate    <- recode (AllData$InterestRateLiberalization, "0=1; 1=2; 2=3; 3=4"); plot (IntRate ~ AllData$InterestRateLiberalization) #
HireFlex   <- AllData$HiringFlexibility; plot (HireFlex ~ AllData$HiringFlexibility) #  (flipped from original direction)
WorkFlex   <- AllData$WorkHoursFlexibility; plot (WorkFlex ~ AllData$WorkHoursFlexibility) #+ (unclear; it appears that larger numbers correspond to extra cost of allowing flexibility in working hours)

OrderData <- data.frame (ResRequire, JobTermCost, MinWage, HireFlex, SocSecurity, CorporateTax
                         , PersonalTax, VATrate, avgTariff, tradeDisp, IntRate
                         , FinTrans, Privatize, Privatize2, capitalopenness, directinvest)
                         # , capitalcontrol, credits, invest
                         # , Privatize2, WorkFlex
                         # , equity, bond, moneymkt, collinvest
                         # , derivatives, credit, financecredit
                         # , guarantees, directinvest, realestate)  #HireFlex only has two categories after dropping Jamaica, T&T; BankSuperv may not be helpful
round (cor (OrderData, method="k", use="p"), 2)

stargazer (data.frame (AllData$country, as.character (AllData$year), apply(OrderData, 2, as.character)), summary=FALSE, type="latex")

######################################
#### Overview of progress by area ####
######################################

Trade   <- with (OrderData, rowMeans (cbind (avgTariff, tradeDisp), na.rm=T))
Finance <- with (OrderData, rowMeans (cbind (IntRate,FinTrans,ResRequire), na.rm=T))
Privat  <- with (OrderData, rowMeans (cbind (Privatize,Privatize2), na.rm=T))
Fiscal  <- with (OrderData, rowMeans (cbind (CorporateTax,PersonalTax,VATrate), na.rm=T))
Labor   <- with (OrderData, rowMeans (cbind (JobTermCost,MinWage,SocSecurity,HireFlex), na.rm=T))
IntlFin <- with (OrderData, rowMeans (cbind (capitalopenness,directinvest), na.rm=T))

TradeQuants   <- as.numeric (by(Trade, AllData$year, mean, na.rm=T))
FinanceQuants <- as.numeric (by(Finance, AllData$year, mean, na.rm=T))
PrivatQuants  <- as.numeric (by(Privat, AllData$year, mean, na.rm=T))
FiscalQuants  <- as.numeric (by(Fiscal, AllData$year, mean, na.rm=T))
LaborQuants   <- as.numeric (by(Labor, AllData$year, mean, na.rm=T))
IntlFinQuants <- as.numeric (by(IntlFin, AllData$year, mean, na.rm=T))

####################################
#### Graphics of aggregate data ####
####################################
setwd (graphicsPath)
# Trade
pdf ("TradeSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(Trade, na.rm=T), max(Trade, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Trade[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), TradeQuants, type="l", lwd=3)
dev.off()

# Finance
pdf ("FinanceSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(Finance, na.rm=T), max(Finance, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Finance[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), FinanceQuants, type="l", lwd=3)
dev.off()

# Privatization
pdf ("PrivatizationSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(Privat, na.rm=T), max(Privat, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Privat[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), PrivatQuants, type="l", lwd=3)
dev.off()

# Tax Policy
pdf ("FiscalSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(Fiscal, na.rm=T), max(Fiscal, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Fiscal[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), FiscalQuants, type="l", lwd=3)
dev.off()

# Labor Policy
pdf ("LaborSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(Labor, na.rm=T), max(Labor, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Labor[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), LaborQuants, type="l", lwd=3)
dev.off()

# International capital Policy
pdf ("IntlFinSummary.pdf", h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1985, 2014), c(min(IntlFin, na.rm=T), max(IntlFin, na.rm=T)), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)))
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], IntlFin[AllData$country==pais]), col="grey", type="l")
}
points (sort (unique (AllData$year)), IntlFinQuants, type="l", lwd=3)
dev.off()

setwd (workingPath)

#############################################
# Find within-country within-year variation #
#############################################
tempData <- data.frame (country=AllData$country, year=AllData$year, trade=Trade
			, finance=Finance, privatize=Privat, fiscal=Fiscal, labor=Labor, intlfinance=IntlFin)

fit1 <- lm (trade ~ as.factor(year), data=tempData)
fit2 <- lm (finance ~ as.factor(year), data=tempData)
fit3 <- lm (privatize ~ as.factor(year), data=tempData)
fit4 <- lm (fiscal ~ as.factor(year), data=tempData)
fit5 <- lm (labor ~ as.factor(year), data=tempData)
fit6 <- lm (intlfinance ~ as.factor(year), data=tempData)

# Print, for each area, within-year variance first, then between-year variance
round (anova(fit1)["Residuals", "Mean Sq"], 2); round (anova(fit1)["as.factor(year)", "Mean Sq"], 2) 
round (anova(fit2)["Residuals", "Mean Sq"], 2); round (anova(fit2)["as.factor(year)", "Mean Sq"], 2) 
round (anova(fit3)["Residuals", "Mean Sq"], 2); round (anova(fit3)["as.factor(year)", "Mean Sq"], 2) 
round (anova(fit4)["Residuals", "Mean Sq"], 2); round (anova(fit4)["as.factor(year)", "Mean Sq"], 2) 
round (anova(fit5)["Residuals", "Mean Sq"], 2); round (anova(fit5)["as.factor(year)", "Mean Sq"], 2) 
round (anova(fit6)["Residuals", "Mean Sq"], 2); round (anova(fit6)["as.factor(year)", "Mean Sq"], 2) 

#######################
#### Try Bayes GRM ####
#######################
setwd ("~/Dropbox/ChainOfResponsiveness/Versions of SPI/separatePolicyFiles/")

# We will run the model twice, once with constrained, once with unconstrained
# parameters
source ("BayesGRMmodel.R") # Discrimination parameters constrained to be positive
# source ("BayesGRMmodelUnconstrained.R") # Unconstrained discrimination parameters


Y <- as.matrix (OrderData) # Matrix with ordered responses to policy items

N <- nrow(OrderData)   # Number of country-year observations
J <- ncol(OrderData)   # Number of items
K <- as.numeric (apply (Y, 2, max, na.rm=T)) # Number of categories in each item j

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
         , alpha=runif(J,0,2)  # To coincide with positive priors
         , kappa.star=build.kappa.inits ()
         #, beta=rnorm(1,0,2)
      ) )
}

jags.model <- run.jags( model=BayesGRM
                        , monitor=jags.parameters
                        , inits=list(jags.inits(), jags.inits())
                        , n.chains=2
                        # , inits=jags.inits(), n.chains=1
                        , method="parallel"
                        , data=jags.data
                        , adapt=1000
                        , thin=10, burnin=5000, sample=500
                        # , adapt=100, thin=1, burnin=10, sample=50
                        , summarise=FALSE
                        , plots=FALSE )


PoolChains <- mcmc.list (as.mcmc (jags.model[[1]][[1]]), as.mcmc (jags.model[[1]][[2]]))
# save (PoolChains, file="BayesGRMmcmcChainsMoreItemsUnconstrained.RData")
save (PoolChains, file="BayesGRMmcmcChainsMoreItems.RData")
# load ("BayesGRMmcmcChainsMoreItems.RData")

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

etiquetas2 <- c("Reserve Requirement", "Job Termination", "Minimum Wage", "Hiring Flexibility", "Social Security"
                , "Corporate Tax", "Personal Tax", "VAT rate", "Average Tariff", "Trade Tax Dispersion"
                , "Interest Rate Lib.", "Financial Transaction Tax", "Privatization 1", "Privatization 2"
                , "Capital Openness", "FDI Control")

pdf ( paste(graphicsPath, "BayesGRMdiscriminationParameters.pdf", sep=""), h=5, w=7)
par (las=1, mar=c(3.5,9,2,1), mfrow=c(1,1), cex.main=1)
lo.x <- min (Alpha.Quants) - 0.05
hi.x <- max (Alpha.Quants) + 0.05 # hi.x <- 0 
plot (c(lo.x, hi.x)
      , c(1, ncol(Alpha.Quants))
      , type="n", xlab=""
      , ylab="", axes=F)
axis(1, cex=0.9)
mtext (1, line=2, text="Coefficient (80% and 95% HPDs)")
# mtext (3, line=0, text="Fixed coefficients")
mtext (text=etiquetas2, side=2, at=c(1:ncol(Alpha.Quants)), cex=0.9)
points (xy.coords(Alpha.Quants[3,], 1:ncol(Alpha.Quants)), pch=19)
segments (x0=Alpha.Quants[2,]
          , x1=Alpha.Quants[4,]
          , y0=seq(1, ncol(Alpha.Quants), by=1)
          , y1=seq(1, ncol(Alpha.Quants), by=1), lwd=6)
segments (x0=Alpha.Quants[1,]
          , x1=Alpha.Quants[5,]
          , y0=seq(1, ncol(Alpha.Quants), by=1)
          , y1=seq(1, ncol(Alpha.Quants), by=1), lwd=2)
abline (h=1:ncol(Alpha.Quants), lty=3)
abline (v=0, lty=3)
dev.off ()


# Plot reform scores, by country, including uncertainty bounds
# Also obtain point estimates of reform scores, to compare against plain-vanilla graded response model
Theta.Quants <- matrix (NA, nrow=5, ncol=ncol (Theta))
for (r in 1:ncol(Theta)){
   Theta.Quants[,r] <- quantile (Theta[,r], prob=c(0.025,0.1,0.5,0.9,0.975))
}
colnames (Theta.Quants) <- paste (AllData$country, AllData$year, sep="_")
Bayes.policy.scores <- Theta.Quants[3,]
Bayes.policy.lo <- Theta.Quants[2,]
Bayes.policy.hi <- Theta.Quants[4,]

labelPais <- c("Argentina","Bolivia","Brazil","Chile"
               ,"Colombia","Costa Rica","Dominican Republic","Ecuador"
               ,"El Salvador","Guatemala","Honduras","Mexico"
               ,"Nicaragua","Paraguay","Panama","Peru","Uruguay","Venezuela")
yearLabels <- c(1985:2014)

par (mar=c(4,3,1,1), las=3, mfrow=c(2,3))
for (i in 1:length (labelPais)) {
   pais <- unique (AllData$country)[i]
   pdf (paste (graphicsPath, "FinalPolicyScores", pais, ".pdf", sep=""), h=5, w=9)
   par (mar=c(4,3,1,1), las=3, mfrow=c(1,1))
   plot (c(1, length(yearLabels)), c(-3.5, 3.5)
         , type="n", axes=F, ylab="", xlab="", main=labelPais[i])
   axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
   axis (2)
   #mtext (side=2, line=2, text="Pro Market <--- Mood ---> Pro State")
   points (xy.coords (yearLabels-1984, Bayes.policy.scores[AllData$country==pais]), pch=19)
   segments (y0=Bayes.policy.lo[AllData$country==pais]
             , y1=Bayes.policy.hi[AllData$country==pais]
             , x0=yearLabels-1984
             , x1=yearLabels-1984
             , lwd=3)
   # legend ("bottomleft", legend=c("Bayes GRM","ML GRM", "Lora", "FA")
   #         , pch=c(19, 19, 19, 4), col=c("black","blue","red","black")
   #         , bty="n")
   dev.off()
}

Policy.Index <- data.frame (country=AllData$country, year=AllData$year, policy.index=apply (Theta, 2, quantile, prob=0.5))
# write.table (Policy.Index, file="PolicyIndexUnconstrained.txt", sep="\t", row.names=FALSE)
write.table (Policy.Index, file="PolicyIndex.txt", sep="\t", row.names=FALSE)




