#################################################
## sharedItemParameterModel
## This file simply puts data together to fit
## a combined McGann-GRM model where policy
## propensity and policy moods are bridged
## by common items
## GR, Nov 22, 2017
#################################################

# load jagsData, which contains data for McGann
load (file="Datasets/FinishedOutput/jagsDataInput.RData")

# Function to eliminate "corner" percentages (0s and 1s)
new.value <- function(variable){
   variable * .9998 + .0001  
}

jagsData$value <- new.value(jagsData$value)
# Eliminate some variables that we will need to rebuild
jagsData <- jagsData[,!is.element(colnames(jagsData), c("q","actor","actor.full","year.cons"))]


# Import SPI data
AllData <- read.csv ("Datasets/FinishedOutput/PublicPolicyIndicesUpdated.csv", header=TRUE)
AllData$country <- as.character (AllData$country)
AllData$country[AllData$country=="Brasil"] <- "Brazil"

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
colnames (AllData)[grep("FinalRequiredReserve", colnames(AllData))] <- "ReserveRequirement"

# Carry out optimal binning step
# Since we can only have ordered categorical variables with the same
# number of categories, simply choose the desired number as argument
# "cats" to function "optimal binning"
# Here we go with 4, because one of the ordered categorical variables
# from the SPI set (IntRate), already has four categories
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

OrderData <- data.frame (ResRequire, JobTermCost, WorkFlex, HireFlex                          
                         , CorporateTax, PersonalTax, FinTrans
                         , capitalopenness, avgTariff, VATrate
                         , tradeDisp, MinWage, IntRate
                         , Privatize, Privatize2, SocSecurity)

OrderData$paisYear <- paste(AllData$country, AllData$year, sep="-")

# Before melting, we change the "variable names" of two policies
# These names will be identical to question names from the surveys
# These will then be the "bridging variables"
colnames (OrderData)[grep("^Privatize$", colnames(OrderData))] <- "privatization.beneficial"
colnames (OrderData)[grep("^capitalopenness$", colnames(OrderData))] <- "capital.mobility.good"

# Now we need to stack the columns in "policies" into a single column vector
Q.data <- melt(OrderData)
names(Q.data)[1:2] <- c("cy","QuestionName")
Q.data$type <- rep ("Government", nrow(Q.data))
Q.data$country <- unlist (strsplit (as.character(Q.data$cy), split="-"))[odd(1:(ncol(Q.data)*2))]
Q.data$year <- as.numeric (unlist (strsplit (as.character(Q.data$cy), split="-"))[even(1:(ncol(Q.data)*2))])
Q.data <- Q.data[,c(2,3,1,4:6)]
Q.data$country.num <- as.factor (recode (Q.data$country, 
                                         "'Argentina'=1
                                         ;'Bolivia'=2
                                         ;'Brazil'=3
                                         ;'Chile'=4
                                         ;'Colombia'=5
                                         ;'CostaRica'=6
                                         ;'DominicanRep'=7
                                         ;'Ecuador'=8
                                         ;'ElSalvador'=9
                                         ;'Guatemala'=10
                                         ;'Honduras'=11
                                         ;'Mexico'=12
                                         ;'Nicaragua'=13
                                         ;'Panama'=14
                                         ;'Paraguay'=15
                                         ;'Peru'=16
                                         ;'Uruguay'=17
                                         ;'Venezuela'=18"))

# Row bind Q.data and jagsData into a single finalData object
finalData <- rbind (jagsData, Q.data)

# Omit missing entries
finalData  <- na.omit (finalData)

# Add a few additional variables
finalData$q  <- as.numeric (finalData$QuestionName)
finalData$actor.number <- recode (finalData$type, "'Citizen'=1; 'Legislator'=2
                                  ; 'Senator'=3; 'President'=4
                                  ; 'Government'=5")
finalData$year.cons <- finalData$year - min(finalData$year) + 1

save (finalData, file="Datasets/FinishedOutput/sharedItemParameterData.RData")

