############################################################
# BuildingPolicyIndexes.R
# This script inputs, analyzes, and scales ALL
# raw indexes from the updated version of Lora
# and Morley, plus additions from Fernandez, Klein,
# Rebucci, Schindler, Uribe (2015) and Chinn-Itto (2016)
# These updated and expanded policy dataset was
# prepared by Yang Zhang (October 29, 2016) and
# Guillermo Rosas (December-January 2017)
#
# GR: October 30, 2016
############################################################

library (ltm)
library (xtable)
library (ggplot2)
library (MCMCpack)
library (gtools)
library (mcmcplots)
library (runjags)
library (rjags)
library (stargazer)
library (car)
library (dplyr)
library (reshape2)
library (rstan)

#### Load mcmc object for later ####
load("Datasets/FinishedOutput/sharedItemParameterData.RData")
load (file="Datasets/FinishedOutput/JointIRT_Flexible_1990.RData")
load (file="Datasets/OriginalDataFiles/itemsPerYear.RData")

## Prepare data
finalData$ActorCtyYr <- as.integer(as.factor(with(finalData,
                                                  paste(type, country, year))))
finalData <- finalData[order(finalData$ActorCtyYr),]
finalData <- filter(finalData, year >= 1990)

finalData$QuestionID <- as.integer(as.factor(with(finalData,
                                                  paste(country, QuestionName))))

##pm-country-years
pmct <- finalData %>% filter(type!="Government")
##p-country-years
pct1 <- finalData %>%
   filter(type=="Government" & 
             QuestionName %in% c("privatization.beneficial",
                                 "capital.mobility.good")) %>%
   mutate_at("value", function(x)case_when(x < 3 ~ 0, TRUE ~ 1))
pct2 <- finalData %>%
   filter(type=="Government" & 
             !(QuestionName %in% c("privatization.beneficial",
                                   "capital.mobility.good")))

# Get type, country, year
actor <- with (finalData, tapply (type, ActorCtyYr, unique))
country <- with (finalData, tapply (country, ActorCtyYr, unique))
year <- with (finalData, tapply (year, ActorCtyYr, unique))

actor.type <- actor[actor == "Government"]
pais <- country[actor == "Government"]
anyo <- year[actor == "Government"]


# Correlation function to be used below
cor.test.plus <- function(x) {
   list(x, 
        Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

#####################
#### Import data ####
#####################

# Set path where graphics should be kept
graphicsPath <- "Graphs/GraphsSept2019/structuralReformPolicies/"

# Identify all indices that we need to import

AllData <- read.csv ("Datasets/FinishedOutput/PublicPolicyIndicesUpdated.csv", header=TRUE)

colnames (AllData)[grep("kaopen", colnames(AllData))] <- "CapitalOpenness"
colnames (AllData)[grep("TariffDispersion", colnames(AllData))] <- "TradeDispersion"
colnames (AllData)[grep("TransactionTax", colnames(AllData))] <- "FinancialTransactionTax"
colnames (AllData)[grep("^VAT$", colnames(AllData))] <- "VATrate"
colnames (AllData)[grep("Privatization1", colnames(AllData))] <- "PrivatizationIndex"
colnames (AllData)[grep("Privatization2", colnames(AllData))] <- "PrivatizationIndex2"
colnames (AllData)[grep("NewFiringCost", colnames(AllData))] <- "JobTerminationCost"
colnames (AllData)[grep("SocialSecurityCost", colnames(AllData))] <- "SocSecurityTax"
colnames (AllData)[grep("WorkingHoursFlexibility", colnames(AllData))] <- "WorkHoursFlexibility"

## Reserve requirement: We have two different sources, Lora, and another paper found by Yang.
## They do not fully overlap, and they correlate only at 0.25. 
colnames (AllData)[grep("FinalRequiredReserve", colnames(AllData))] <- "ReserveRequirement"

# The following items are from IMF's AREAER. The problem is that they exist only
# since 1995 in their current form. AREAER has data from before this period, but
# they collect data differently. In the end, we'll use Chinn-Itto's kaopen (now
# renamed CapitalOpenness) as a single measure of capital openness in lieu of all of
# this, so the following lines can be skipped without consequence
# All of these are coded so that higher numbers mean more rules
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

# As an alternative, we add all of the IMF AREAER data, then do optimal binning
AllData$ArearIndex <- rowMeans (cbind (AllData$CapitalOpenness, AllData$Equity, AllData$Bond, AllData$MoneyMkt,
									   AllData$CollInvest, AllData$Derivatives, AllData$Credit, AllData$FinancialCredit, AllData$DirectInvest,
									   AllData$Guarantees, AllData$RealEstate, AllData$CapitalControl, AllData$Credits, AllData$Invest))


# Graphic with reserve requirement example
meanReserve <- c()
for (i in 1:length(unique(AllData$country))){
   meanReserve[i] <- mean (AllData$ReserveRequirement[AllData$country==unique(AllData$country)[i]], na.rm=T)
}
pdf (paste(graphicsPath, "resRequirementExample.pdf", sep=""), h=7, w=10)
labs <- c("ARG","BOL","BRZ","CHI","COL","CRI","DOM","ECU","SAL","GUA","HON","MEX","NIC","PAN","PAR","PER","URU","VEN")
par (mfrow=c(1,1), mar=c(3,3,1,1))
plot (AllData$ReserveRequirement, axes=F
      , xlim=c(0,nrow(AllData))
      , ylim=c(0,100)
      , main=""
      , xlab="", ylab="Reserve requirement rate"
      , pch=19
      , col="gray"
      , cex=0.8)
axis (1, at=c(0, nrow(AllData)), labels=c(NA,NA))
axis (2)
lineHere <- rep (NA, 540);
for (i in 2:540){
  lineHere[i] <- ifelse (!identical(AllData$country[i], AllData$country[i-1]), 1, 0)
}
abline (v=which (lineHere==1) - 1.5)
lineHere[1] <- 1
text (y=100, x=which (lineHere==1) + 12, labels=labs, cex=0.7)
points (xy.coords (which (lineHere==1) + 12, meanReserve), pch=4)
# abline (h=c(20,40), lty=2)
dev.off ()

###################################
#### MLE graded response setup ####
###################################

continuousVars <- c("AverageTariff","TradeDispersion"
                    ,"ReserveRequirement","FinancialTransactionTax"
                    ,"VATrate","PersonalIncomeTax","CorporateIncomeTax"
                    ,"PrivatizationIndex","PrivatizationIndex2","JobTerminationCost"
                    ,"SocSecurityTax","NewMinimumWage","Equity","Bond","MoneyMkt" 
					# changed names of NewMinimumWage for MinimumWage
                    ,"CollInvest","Derivatives","Credit"
                    ,"RealEstate","CapitalOpenness","ArearIndex")

# Ordered variables are: BankSupervQuality, WorkHoursFlexibility, HiringFlexibility,
# InterestRateLiberalization
orderedVars <- c("InterestRateLiberalization","BankSupervision","^FinancialCredit$"
                 ,"HiringFlexibility","WorkHoursFlexibility","Guarantees","DirectInvest"
                 , colnames(AllData)[grep("Inflow", colnames(AllData))]
                 , colnames(AllData)[grep("Outflow", colnames(AllData))])

# We need to aggregate continuous variables into ordered categories.
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
   out <- rbind (continuousVars[i]
          , round (temp[[3]]$betweenss / temp[[3]]$totss, 2)
          , round (temp[[4]]$betweenss / temp[[4]]$totss, 2))
   print (out)
}


# The previous analysis shows that 3 categories suffice for most variables,
# except perhaps trade dispersion
# (then also AverageTariff) and privatizationIndex and minimumwage, which has 4
optimalBinning <- function (x, cats=4) {
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
# Note that we recode everything so that higher 
# values correspond to more market orientation.
# Capital Openness
capitalopenness <- optimalBinning (AllData$CapitalOpenness)
plot (capitalopenness ~ AllData$CapitalOpenness) # More openness (chinn-ito) is pro-market

# Reserve requirement
ResRequire   <- optimalBinning (AllData$ReserveRequirement)
#(more and more used as macroprudential countercyclical tools
#; perhaps this is the best justification to aggregate them in broad categories)
ResRequire <- car::recode (ResRequire, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower reserve requirements are pro-market
plot (ResRequire ~ AllData$ReserveRequirement)

# Job termination cost
JobTermCost  <- optimalBinning (AllData$JobTerminationCost) # NewFiringCost in original dataset
JobTermCost  <- car::recode (JobTermCost, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower cost is pro-market
plot (JobTermCost ~ AllData$JobTerminationCost) 

# Social security
SocSecurity  <- optimalBinning (AllData$SocSecurityTax)
SocSecurity <- car::recode (SocSecurity, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower tax is pro-market
plot (SocSecurity ~ AllData$SocSecurityTax) #+

# Corporate tax rate
CorporateTax <- optimalBinning (AllData$CorporateIncomeTax)
CorporateTax <- car::recode (CorporateTax, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower corporate tax rate is pro-market
plot (CorporateTax ~ AllData$CorporateIncomeTax)

# Personal tax rate
PersonalTax  <- optimalBinning (AllData$PersonalIncomeTax)
PersonalTax  <- car::recode (PersonalTax, "1=4; 2=3; 3=2; 4=1")  # Recoded so that lower personal income tax rate is pro-market
plot (PersonalTax ~ AllData$PersonalIncomeTax) 

# Value-added tax rate
VATrate      <- optimalBinning (AllData$VATrate)
VATrate      <- car::recode (VATrate, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower VAT rate is pro-market
plot (VATrate ~ AllData$VATrate) 

# Average trade tariff
avgTariff    <- optimalBinning (AllData$AverageTariff)
avgTariff    <- car::recode (avgTariff, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower tariffs are pro-market
plot (avgTariff ~ AllData$AverageTariff)

# Dispersion of trade tariffs
tradeDisp    <- optimalBinning (AllData$TradeDispersion)
tradeDisp    <- car::recode (tradeDisp, "1=4; 2=3; 3=2; 4=1")  # Recoded so that lower dispersion is pro-market
plot (tradeDisp ~ AllData$TradeDispersion)

# Financial transaction tax
FinTrans     <- optimalBinning (AllData$FinancialTransactionTax)
FinTrans     <- car::recode (FinTrans, "1=4; 2=3; 3=2; 4=1")  # Recoded so that lower financial tax is pro-market
plot (FinTrans ~ AllData$FinancialTransactionTax) #+

# Minimum wage (value)
MinWage      <- optimalBinning (AllData$NewMinimumWage)
MinWage      <- car::recode (MinWage, "1=4; 2=3; 3=2; 4=1") # Recoded so that lower minimum wage (as %GDP) is pro-market
plot (MinWage ~ AllData$NewMinimumWage) 

# Privatization
Privatize    <- optimalBinning (AllData$PrivatizationIndex)
plot (Privatize ~ AllData$PrivatizationIndex) # Original coding; higher privatization is pro-market

# Privatization minus nationalization
Privatize2   <- optimalBinning (AllData$PrivatizationIndex2)
plot (Privatize2 ~ AllData$PrivatizationIndex2) # Original coding; higher privatization is pro-market

# Interest rate liberalization
IntRate <- car::recode (AllData$InterestRateLiberalization, "0=1; 1=2; 2=3; 3=4")
plot (IntRate ~ AllData$InterestRateLiberalization) # Original coding: more liberalization is pro-market

# Hiring flexibility
HireFlex   <- AllData$HiringFlexibility
plot (HireFlex ~ AllData$HiringFlexibility) #  Original coding: larger numbers are pro-market

# Working hours flexibility
WorkFlex   <- AllData$WorkHoursFlexibility
plot (WorkFlex ~ AllData$WorkHoursFlexibility) # Original coding: larger numbers are pro-market


# BankSuperv <- recode (AllData$BankSupervision, "0=1; 1=2; 2=3") # We do not end up using this one
# plot (BankSuperv ~ AllData$BankSupervision) # Original coding: more (better,really) supervization is pro-market

OrderData <- data.frame (ResRequire, JobTermCost, WorkFlex, HireFlex                          
                         , CorporateTax, PersonalTax, FinTrans
                         , capitalopenness, avgTariff, VATrate
                         , tradeDisp, MinWage, IntRate
                         , Privatize, Privatize2, SocSecurity)
OrderData$paisYear <- paste(AllData$country, AllData$year, sep="-")

# Correlation table
round (cor (OrderData, method="k", use="p"), 2)
descriptiveStats <- stargazer (OrderData, title="Summary statistics for policy input data", digits=2,
		   label="T:statisticsPolicy",
		   covariate.labels=c("Reserve requirement", "Job termination cost", "Minimum wage"
		   		   , "Working flexibility", "Hiring flexibility", "Social security tax"
		   		   , "Corporate tax", "Personal tax", "Value added tax", "Average tariff"
		   		   , "Trade dispersion", "Interest rate"
		   		   , "Financial transaction", "Privatization"
		   		   , "Privatized assets", "Capital openness"))


reducedYearData <- data.frame (country=AllData$country, year=as.numeric(AllData$year)
                               , apply(OrderData, 2, as.character))
reducedYearData <- reducedYearData[reducedYearData$year >= 1990,]

fullData <- stargazer (reducedYearData[,-ncol(reducedYearData)]
         , title="Categorical codification of all policy outcomes. 
		   Higher numbers correspond to heavier state intervention.", label="T:appendixCategoricalData"
		   , summary=FALSE, type="latex", rownames=FALSE
		   , covariate.labels=c("Country","Year","Reserve requirement", "Job termination cost", "Minimum wage"
		                        , "Working flexibility", "Hiring flexibility", "Social security tax"
		                        , "Corporate tax", "Personal tax", "Value added tax", "Average tariff"
		                        , "Trade dispersion", "Interest rate"
		                        , "Financial transaction", "Privatization"
		                        , "Privatized assets", "Capital openness"))


######################################
#### Overview of progress by area ####
######################################
OrderData <- OrderData[as.numeric(AllData$year) >= 1990,]
Trade   <- with (OrderData, rowMeans (cbind (avgTariff, tradeDisp), na.rm=T))
Finance <- with (OrderData, rowMeans (cbind (IntRate,FinTrans,ResRequire), na.rm=T))
Privat  <- with (OrderData, rowMeans (cbind (Privatize,Privatize2), na.rm=T))
Fiscal  <- with (OrderData, rowMeans (cbind (CorporateTax,PersonalTax,VATrate), na.rm=T))
Labor   <- with (OrderData, rowMeans (cbind (JobTermCost,MinWage,SocSecurity,HireFlex,WorkFlex), na.rm=T))
IntlFin <- capitalopenness[as.numeric(AllData$year) >= 1990]

TradeQuants   <- as.numeric (by(Trade, reducedYearData$year, mean, na.rm=T))
FinanceQuants <- as.numeric (by(Finance, reducedYearData$year, mean, na.rm=T))
PrivatQuants  <- as.numeric (by(Privat, reducedYearData$year, mean, na.rm=T))
FiscalQuants  <- as.numeric (by(Fiscal, reducedYearData$year, mean, na.rm=T))
LaborQuants   <- as.numeric (by(Labor, reducedYearData$year, mean, na.rm=T))
IntlFinQuants <- as.numeric (by(IntlFin, reducedYearData$year, mean, na.rm=T))

average.structural.index <- rowMeans (cbind (Trade, Finance, Privat, Fiscal, Labor, IntlFin), na.rm=T)

####################################
#### Graphics of aggregate data ####
####################################
AllData <- AllData[as.numeric(AllData$year) >= 1990,]
# Preliminary info
n=18  # number of countries/colors
small.jitter <- seq(-0.1,0.1, length=n)  # to avoid line overlap
c.labs <- as.character (unique(AllData$country)) # labels
c.labs[grep("CostaRica", c.labs)] <- "Costa Rica"
c.labs[grep("ElSalvador", c.labs)] <- "El Salvador"
c.labs[grep("DominicanRep", c.labs)] <- "Dom. Rep."



par (mfrow=c(1,1))
# In names of pdfs, omit "Col" to recover black and white pdfs

# Trade
pdf (paste0(graphicsPath,"TradeSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(Trade, na.rm=T)-0.12, max(Trade, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Trade[AllData$country==pais]+small.jitter[i]), col="grey", type="l")
}
points (sort (unique (AllData$year)), TradeQuants, type="l", lwd=3)
dev.off()

# Finance
pdf (paste0(graphicsPath,"FinanceSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(Finance, na.rm=T)-0.12, max(Finance, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	points (xy.coords(AllData$year[AllData$country==pais], Finance[AllData$country==pais]+small.jitter[i]), col="gray", type="l")
}
points (sort (unique (AllData$year)), FinanceQuants, type="l", lwd=3)
dev.off()

# Privatization
pdf (paste0(graphicsPath,"PrivatizationSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(Privat, na.rm=T)-0.12, max(Privat, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	# points (xy.coords(AllData$year[AllData$country==pais], Privat[AllData$country==pais]+small.jitter[i]), col=rainbow(n)[i], type="l")  #col="grey"
	points (xy.coords(AllData$year[AllData$country==pais], Privat[AllData$country==pais]+small.jitter[i]), col="grey", type="l")  #col="grey"
}
points (sort (unique (AllData$year)), PrivatQuants, type="l", lwd=3)
# legend ("topleft", bty="n", legend=c.labs, lwd=1, col=rainbow(n), cex=0.8)
dev.off()

# Tax Policy
pdf (paste0(graphicsPath,"FiscalSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(Fiscal, na.rm=T)-0.12, max(Fiscal, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	# points (xy.coords(AllData$year[AllData$country==pais], Fiscal[AllData$country==pais]+small.jitter[i]), col=rainbow(n)[i], type="l")
	points (xy.coords(AllData$year[AllData$country==pais], Fiscal[AllData$country==pais]+small.jitter[i]), col="grey", type="l")
}
points (sort (unique (AllData$year)), FiscalQuants, type="l", lwd=3)
#legend ("topright", bty="n", legend=c.labs, lwd=1, col=rainbow(n), cex=0.8)
dev.off()

# Labor Policy
pdf (paste0(graphicsPath,"LaborSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(Labor, na.rm=T)-0.12, max(Labor, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	# points (xy.coords(AllData$year[AllData$country==pais], Labor[AllData$country==pais]+small.jitter[i]), col=rainbow(n)[i], type="l")
	points (xy.coords(AllData$year[AllData$country==pais], Labor[AllData$country==pais]+small.jitter[i]), col="grey", type="l")
}
points (sort (unique (AllData$year)), LaborQuants, type="l", lwd=3)
#legend ("topleft", bty="n", legend=c.labs, lwd=1, col=rainbow(n), cex=0.8)
dev.off()

# International capital Policy
pdf (paste0(graphicsPath,"IntlFinSummary.pdf"), h=5, w=7)
par (las=2, mar=c(4,3,1,1))
plot (c(1990, 2014), c(min(IntlFin, na.rm=T)-0.12, max(IntlFin, na.rm=T)+0.12), type="n", axes=F, xlab="", ylab="")
axis (1, at=sort (unique (AllData$year)), cex=0.8)
axis (2)
for (i in 1:length (unique(AllData$country))) {
	pais <- unique(AllData$country)[i]
	# points (xy.coords(AllData$year[AllData$country==pais], IntlFin[AllData$country==pais]+small.jitter[i]), col=rainbow(n)[i], type="l")
	points (xy.coords(AllData$year[AllData$country==pais], IntlFin[AllData$country==pais]+small.jitter[i]), col="grey", type="l")
}
points (sort (unique (AllData$year)), IntlFinQuants, type="l", lwd=3)
#legend ("topleft", bty="n", legend=c.labs, lwd=1, col=rainbow(n), cex=0.8)
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






##########################
#### Import Bayes GRM ####
##########################

# Load MCMC object
chainsConv <- rstan::As.mcmc.list(common_model)

# Summaries of posterior distribution of policy orientations
Theta <- rstan::extract (common_model, "mu_g")[[1]]

#######################################
#### Plots for policy orientations ####
#######################################

# Plot reform scores, by country, including uncertainty bounds
Theta.Quants <- matrix (NA, nrow=5, ncol=ncol (Theta))
for (r in 1:ncol(Theta)){
	Theta.Quants[,r] <- quantile (Theta[,r], prob=c(0.025,0.1,0.5,0.9,0.975))
}
colnames (Theta.Quants) <- paste (AllData$country, AllData$year, sep="_")
Bayes.policy.scores <- Theta.Quants[3,]
Bayes.policy.lo <- Theta.Quants[2,]
Bayes.policy.hi <- Theta.Quants[4,]

# Rebuild policy index, from unconstrained data
Policy.Index <- data.frame (country=AllData$country, year=AllData$year, policy.index=apply (Theta, 2, quantile, prob=0.5))

# Check correlation between policy.index and Bayes.policy.scores: should be 1
cor (Bayes.policy.scores, Policy.Index$policy.index)

labelPais <- c("Argentina","Bolivia","Brazil"
               ,"Chile","Colombia","Costa Rica"
               ,"Ecuador","El Salvador","Guatemala"
               ,"Honduras","Mexico","Nicaragua"
               ,"Panama","Paraguay","Peru"
               ,"Dominican Republic","Uruguay","Venezuela")
yearLabels <- unique (anyo)

for (i in 1:length (labelPais)) {
   ps <- unique (Policy.Index$country)[i]
	pdf (paste (graphicsPath, "FinalPolicyScores", ps, ".pdf", sep=""), h=5, w=9)
	par (mar=c(4,3,1,1), las=3, mfrow=c(1,1))
	plot (c(1, length(yearLabels)), c(-1, 3)
		  , type="n", axes=F, ylab="", xlab="", main="")
	axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
	axis (2)
	mtext (side=2, line=2, text="State <- Policy orientation -> Market")
	points (xy.coords (yearLabels-1989, Policy.Index$policy.index[Policy.Index$country==ps]), pch=19)
	segments (y0=Bayes.policy.lo[Policy.Index$country==ps]
			  , y1=Bayes.policy.hi[Policy.Index$country==ps]
			  , x0=yearLabels-1989
			  , x1=yearLabels-1989
			  , lwd=3)
	dev.off()
}


#### Correlate policy orientation scores with simple average.structural.index ####
# overall correlation
cor.test.plus(cor.test (x=average.structural.index, y=Policy.Index$policy.index))

# country-by-country
correlTable <- c()
for (i in 1:18) {
   ps <- levels (Policy.Index$country)[i]
   print (ps)
   tmp <- cor.test.plus (cor.test (x=average.structural.index[Policy.Index$country==ps]
                            , y=Policy.Index$policy.index[Policy.Index$country==ps]))
   correlTable <- rbind (correlTable, c (tmp[[1]]$estimate, tmp[[2]]))
}
correlTable <- data.frame (Country=as.character(levels(Policy.Index$country))
                           , cor.estimate=correlTable[,1]
                           , cor.SE=correlTable[,2])

xtable (correlTable)

#### Correlate policy orientation scores with Lora's structural reform index ####

SPI <- read.table ("Datasets/Versions of SPI/separatePolicyFiles/LoraSRI.txt"
                   , header=T
                   , sep="\t")
SPI <- SPI[,-grep("Variable", colnames(SPI))]

SPI <- melt (SPI, id.vars="Country"
      , value.name="spi.index"
      , variable.name="year")

SPI$year <- as.numeric (substr(SPI$year, start=2, stop=5))

# Merge with Policy.Index
Policy.Index$id <- paste(Policy.Index$country, Policy.Index$year, sep="-")
Policy.Index$no.cons <- c(1:nrow(Policy.Index))
SPI$id <- paste(SPI$Country, SPI$year, sep="-")

Policy.Index <- merge (Policy.Index, SPI, all.x=T, by="id")
Policy.Index <- Policy.Index[order(Policy.Index$no.cons),]


# A more complete correlation table
correlTable2 <- c()
for (i in 1:18) {
   ps <- levels (Policy.Index$country)[i]
   print (ps)
   tmp1 <- cor.test.plus (cor.test (x=average.structural.index[Policy.Index$country==ps]
                                   , y=Policy.Index$policy.index[Policy.Index$country==ps]))
   if (invalid(Policy.Index$spi.index[Policy.Index$country==ps])) {
      correlTable2 <- rbind (correlTable2, c (tmp1[[1]]$estimate, tmp1[[2]], NA, NA))
   } else {
      tmp2 <- cor.test.plus (cor.test (x=Policy.Index$spi.index[Policy.Index$country==ps]
                                       , y=Policy.Index$policy.index[Policy.Index$country==ps]))
      correlTable2 <- rbind (correlTable2, c (tmp1[[1]]$estimate, tmp1[[2]], tmp2[[1]]$estimate, tmp2[[2]]))
   }
}
correlTable2 <- data.frame (Country=as.character(levels(Policy.Index$country))
                           , cor.categories=correlTable2[,1]
                           , cor.SE.categories=correlTable2[,2]
                           , cor.spi=correlTable2[,3]
                           , cor.SE.spi=correlTable2[,4])

xtable (correlTable2)

# overall correlation
cor.test.plus(cor.test (x=Policy.Index$spi.index, y=Policy.Index$policy.index))




## Plots
for (i in 1:length (labelPais)) {
   ps <- unique (Policy.Index$country)[i]
   setEPS()
   postscript(paste ("Graphs/RevisedGraphs/EPS format/", ps, ".eps", sep=""), height=5, width=9)
   par (mar=c(4,3,1,1), las=3, mfrow=c(1,1))
   plot (c(1, length(yearLabels)), c(-1, 3)
         , type="n", axes=F, ylab="", xlab="", main="")
   axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
   axis (2)
   mtext (side=2, line=2, text="State <- Policy orientation -> Market")
   points (xy.coords (yearLabels-1989, Bayes.policy.scores[Policy.Index$country==ps])
           , type="p"
           , lwd=3, pch=19)
   segments (y0=Bayes.policy.lo[Policy.Index$country==ps]
             , y1=Bayes.policy.hi[Policy.Index$country==ps]
             , x0=yearLabels-1989
             , x1=yearLabels-1989
             , lwd=3)
   points (xy.coords (yearLabels-1989
           , (average.structural.index[Policy.Index$country==ps]-mean(average.structural.index, na.rm=T)))
           , type="l"
           , col="gray"
           , lwd=3)
   # Change scale of SPI, so that we can compare easily
   points (xy.coords (yearLabels-1989
                      , Policy.Index$spi.index[Policy.Index$country==ps]-2+Policy.Index$spi.index[Policy.Index$country==ps]*4)
           , type="l"
           , col="grey"
           , lwd=3
           , lty=2)
   legend ("topleft", legend=c("Policy orientation","Average over 16 policies", "Lora's SPI")
           , lty=c(1, 1, 2), lwd=c(3,3,2), col=c("black","grey","grey")
           , bty="n")
   dev.off()
}


#### What's happening with Uruguay ####
Uruguay <- data.frame  (OrderData[grep("Uruguay", OrderData$paisYear),],
policy.index = Policy.Index$policy.index[Policy.Index$country=="Uruguay"],
spi.index = Policy.Index$spi.index[Policy.Index$country=="Uruguay"])

Colombia <- data.frame  (OrderData[grep("Colombia", OrderData$paisYear),],
                        policy.index = Policy.Index$policy.index[Policy.Index$country=="Colombia"],
                        spi.index = Policy.Index$spi.index[Policy.Index$country=="Colombia"])

