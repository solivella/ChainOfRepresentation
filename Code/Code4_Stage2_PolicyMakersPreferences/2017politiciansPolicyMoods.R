#############################################
#### Plots of policy-makers policy moods ####
#############################################

library (tidyverse)
library (gtools)
library (reshape2)
library (plyr)
library (runjags)
library (MCMCpack)
library (mcmcplots)
library (car)
library (parallel)
library (data.table)
library (lme4)
library (zoo)
library (xtable)
library (rstan)
library (rminer)
library (ggrepel)
library (ggalt)


# Set graphs path
graphicsPath <- "Graphs/GraphsSept2019"
dataPath  <- "Datasets/OriginalDataFiles/"

# Correlation function to be used below
cor.test.plus <- function(x) {
   list(x, 
        Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

####################
# Load legislators #
####################
# These data include the individual level responses, not the percentages that we eventually use

load (file=paste0 (dataPath, "PELAIndividualResponsesMI.RData"))
colnames (LegMI)

LegMI$id <- paste (LegMI$pais, LegMI$year, LegMI$Term, sep="/")
aggdata <- as.character (aggregate(LegMI$id, by=list(LegMI$id), 
                                   FUN=unique, na.rm=FALSE)[,2])
countryCol <- unlist (strsplit (aggdata, split="/", fixed=TRUE ))[seq(1,length(aggdata)*3,by=3)]
yearCol <- unlist (strsplit (aggdata, split="/", fixed=TRUE ))[seq(2,length(aggdata)*3,by=3)]
termCol <- unlist (strsplit (aggdata, split="/", fixed=TRUE ))[seq(3,length(aggdata)*3,by=3)]

# Introduce corrections to termCol, so that all legislative terms
# coincide with what we see in NewPartyWeights.csv.
# Recall that the terms were not always well put together
yearCol[countryCol=="Argentina" & yearCol=="2005"] <- "2007"
termCol[countryCol=="Argentina" & termCol=="2007-2011"] <- "2007-2009"
termCol[countryCol=="Bolivia" & termCol=="2002-2007"] <- "2002-2005"
termCol[countryCol=="Mexico" & termCol=="2009-2011"] <- "2009-2012"
termCol[countryCol=="Uruguay" & termCol=="2009-2014"] <- "2010-2015"

# Introduce term columns for Senate and for Presidents
party.weights <- read.csv(paste0 (dataPath, "NewPartyWeightsOld.csv"), na.strings="NA")

termUpper <- termPres <- c()
for (i in 1:length (termCol)) {
   termUpper[i] <- as.character (unique (party.weights$TermUpper[party.weights$Country==countryCol[i] & party.weights$CorrectTerm==termCol[i]]))
   termPres[i]  <- as.character (unique (party.weights$TermPres[party.weights$Country==countryCol[i] & party.weights$CorrectTerm==termCol[i]]))
}


##################################################################################
##################################################################################
## These lines produce an object that we later use for plotting 
##################################################################################
##################################################################################
largestLHparty <- largestLHshare <- c()
largestUHparty <- largestUHshare <- c()
termLH <- termUH <- termPrez <- land <- match.year <- c()
for (i in 1:length (termCol)) {
   tmp <- party.weights[party.weights$Country==countryCol[i] & party.weights$CorrectTerm==termCol[i],]
   yr <- yearCol[i]
   ld <- as.character (unique (tmp$Country))
   LH <- termCol[i]
   UH <- termUpper[i]
   Prez <- termPres[i]
   largeLHp <- paste ( as.character (tmp$PartyAcronym[which (tmp$Seat.ShareLH==max(tmp$Seat.ShareLH))]), collapse="/")
   largeLHs <- unique (tmp$Seat.ShareLH[which (tmp$Seat.ShareLH==max(tmp$Seat.ShareLH))])
   # largeLHs <- tmp$Seat.ShareLH[which (tmp$Seat.ShareLH==max(tmp$Seat.ShareLH))]
   if ( invalid (tmp$Seat.ShareUH)) { 
      largeUHp <- NA
      largeUHs <- NA
   } else {
      largeUHp <- paste ( as.character (tmp$PartyAcronym[which (tmp$Seat.ShareUH==max(tmp$Seat.ShareUH))]), collapse="/")
      largeUHs <- unique (tmp$Seat.ShareUH[which (tmp$Seat.ShareUH==max(tmp$Seat.ShareUH))])
   }   
   land <- c(land, ld)
   termLH <- c(termLH, LH)
   termUH <- c(termUH, UH)
   termPrez <- c(termPrez, Prez)
   match.year <- c(match.year, yr)
   largestLHparty <- c(largestLHparty, largeLHp)
   largestLHshare <- c(largestLHshare, largeLHs)
   largestUHparty <- c(largestUHparty, largeUHp)
   largestUHshare <- c(largestUHshare, largeUHs)
}   

PartyLabelsData <- data.frame (pais=land, termLH=termLH, termUH=termUH
                               , termPrez=termPrez, matchYear=match.year
                               , largestLHparty=largestLHparty
                               , largestUHparty=largestUHparty
                               , largestLHshare=largestLHshare
                               , largestUHshare=largestUHshare)

write.csv (PartyLabelsData
           , file="Datasets/FinishedOutput/PartyLabelsData.csv"
           , row.names=FALSE)
##################################################################################
##################################################################################

# Execute, copy, and drop in AppendixWordingLegislators.tex
xtable (data.frame (Country=countryCol, Survey.Year=yearCol, Legislature=termCol, Senate=termUpper, Executive=termPrez)
        , caption="Legislatures in Latin America for which we have legislators' policy positions"
        , label="T:availLeg") 

# Show a high correlation between party weights of lower and upper houses
cor (cbind (party.weights$Seat.ShareLH, party.weights$Seat.ShareUH), use="p")
correctLabels4Book <- party.weights$Country
levels (correctLabels4Book) <- ifelse (levels(correctLabels4Book)=="Republica Dominicana"
                              , "Dominican Republic", levels(correctLabels4Book))


pdf (paste0(graphicsPath, "discrepantSeatShares.pdf"), h=5, w=7)
par (mar=c(4,4,1,1))
plot (party.weights$Seat.ShareLH, party.weights$Seat.ShareUH, pch=19, col="grey"
      , ylab="Party seat shares, upper houses", xlab="Party seat shares, lower houses")
discrepant.UH <- which (party.weights$Seat.ShareUH[party.weights$Seat.ShareLH<0.1]>0.5)
discrepant.LH <- which (party.weights$Seat.ShareLH[party.weights$Seat.ShareUH<0.2]>0.4)
discrepant.UH.hi <- which (party.weights$Seat.ShareLH[party.weights$Seat.ShareUH>0.76]>0.4)
text (xy.coords (party.weights$Seat.ShareLH[party.weights$Seat.ShareLH<0.1][discrepant.UH]
                 ,party.weights$Seat.ShareUH[party.weights$Seat.ShareLH<0.1][discrepant.UH])
      , labels=paste(correctLabels4Book[party.weights$Seat.ShareLH<0.1][discrepant.UH]
                     , party.weights$CorrectTerm[party.weights$Seat.ShareLH<0.1][discrepant.UH]
                     , party.weights$PartyAcronym[party.weights$Seat.ShareLH<0.1][discrepant.UH], sep=", ")
      , cex=0.8, pos=4)
text (xy.coords (party.weights$Seat.ShareLH[party.weights$Seat.ShareUH>0.76][discrepant.UH.hi]
                 ,party.weights$Seat.ShareUH[party.weights$Seat.ShareUH>0.76][discrepant.UH.hi])
      , labels=paste(correctLabels4Book[party.weights$Seat.ShareUH>0.76][discrepant.UH.hi]
                     , party.weights$CorrectTerm[party.weights$Seat.ShareUH>0.76][discrepant.UH.hi]
                     , party.weights$PartyAcronym[party.weights$Seat.ShareUH>0.76][discrepant.UH.hi], sep=", ")
      , cex=0.8, pos=2)
text (xy.coords (party.weights$Seat.ShareLH[party.weights$Seat.ShareUH<0.2][discrepant.LH]
                 ,party.weights$Seat.ShareUH[party.weights$Seat.ShareUH<0.2][discrepant.LH])
      , labels=paste(correctLabels4Book[party.weights$Seat.ShareUH<0.2][discrepant.LH]
                     , party.weights$CorrectTerm[party.weights$Seat.ShareUH<0.2][discrepant.LH]
                     , party.weights$PartyAcronym[party.weights$Seat.ShareUH<0.2][discrepant.LH], sep=", ")
      , cex=0.8, pos=3)
abline (a=0, b=1, lty=2)
dev.off()



availCountries <- read.table (file=paste0 (dataPath, "PELAavailableDataSurveyOnRows.txt")
                              , sep="\t", header=T)

# Function to change levels of a factor
changeLevels <- function (var) {
   if (is.factor(var)==FALSE) { 
      print ("Not a factor")
   } else {
      temp <- delevels (var, levels=levels(var), label=1)
      temp <- delevels (temp, levels="", label=NA)
      temp <- as.numeric (temp)
      return (temp)
   }
}

vars2recode <- colnames (availCountries)[!is.element(colnames(availCountries), c("id","Year","Country"))]

for (i in 1:ncol(availCountries)) {
   if (!is.element (colnames(availCountries)[i], vars2recode)) { next }
   else {
      availCountries[,i] <- changeLevels (availCountries[,i])
   }
}


################################################
# Plot country-years for which we have surveys #
################################################
countryLabels <- as.character (sort (unique (availCountries$Country)))
countryLabels[grep("Brasil", countryLabels)] <- "Brazil"
countryLabels[grep("Republica Dominicana", countryLabels)] <- "Dominican Rep."
yearLabels <- 1993:2013

pdf (paste (graphicsPath, "surveyAvailabilityLegislators.pdf", sep=""), h=7, w=10)
par (mar=c(4,8,1,1), las=2, mfrow=c(1,1))
plot (c(1, length(yearLabels)), c(1, length(countryLabels))
      , type="n", axes=F, ylab="", xlab="")
axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
axis (2, at=c(1:length(countryLabels)), labels=countryLabels)
abline (h=1:length(countryLabels), lty=3)
no.surveys <- c()
for (i in 1:length(countryLabels)) {
   cntr <- levels (availCountries$Country)[i]
   surv <- sort (unique (availCountries$Year[availCountries$Country==cntr]))
   no.surveys <- c(no.surveys, length (surv))
   points (xy.coords (c(1:length(yearLabels))[is.element (yearLabels, as.numeric(surv))], rep (i, length(surv)))
           , pch=19, cex=1, col="black")
}
dev.off() 

# Number of surveys per country contained in no.surveys
fivenum (no.surveys)


############################
#### Read in aux info   ####
############################
load (file="Datasets/FinishedOutput/JointIRT_Flexible_1990.RData")
load("Datasets/FinishedOutput/sharedItemParameterData.RData")  # Input for chains

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

actor.type <- actor[actor != "Government"]
country <- country[actor != "Government"]
year    <- year[actor != "Government"]


########################
# Graph available data #
########################
vars2recode <- vars2recode[!is.element(vars2recode, c("LR.ideology.prez1","LR.ideology.self","PartyCode"))]

jagsDataLeg <- finalData %>% filter(type=="Legislator")

# These graphs go into Chapter ChPoliticianPreferences.tex as "F:availableStimuli"
count.items.available <- items.available <- list()
whichCountry <- whichYear <- countItems <- c()
for (i in 1:length(unique(jagsDataLeg$country))) {
   pais <- unique (jagsDataLeg$country)[i]
   prettyPais <- car::recode (pais, "'ElSalvador'='El Salvador';
                         'DominicanRep'='Dominican Republic';
                         'CostaRica'='Costa Rica'")
   tempData <- jagsDataLeg[jagsDataLeg$country==pais,]
   # Which issues appear only once
   badIssues <- which (table(tempData$q)==1)
   tempData <- tempData[!is.element(tempData$q,as.numeric (names (badIssues))),]
   surv <- sort (unique (tempData$year))
   
   howManyItems <- yr <- ps <- c()
   for (j in 1:length(surv)){
      howManyItems <- c(howManyItems, length (tempData$QuestionName[tempData$year==surv[j]]))
      yr <- c(yr, surv[j])
      ps <- c(ps, pais)
   }
   whichCountry <- c(whichCountry, ps)
   whichYear <- c(whichYear, yr)
   countItems <- c(countItems, howManyItems)
   
   pdf (paste(graphicsPath, "availStimuliLegislators", pais, ".pdf", sep=""), h=9, w=5)
   par (mar=c(4,11,2,1), las=2, cex.axis=0.8)
   plot (c(1,23), c(1,length(vars2recode)), type="n", axes=F, ylab="", xlab="")
   axis (1, at=1:23, labels = 1992:2014)
   axis (2, at=1:length(vars2recode), labels = vars2recode, cex=0.9)
   par (las=0); mtext (side=3, line=0, at=9.5, prettyPais)
   abline (v=1:23, lty=3)
   abline (h=1:length(vars2recode), lty=3)
   for (j in 1:length(surv)){
      whichLabels <- tempData$QuestionName[tempData$year==surv[j]]
      whichX <- surv[j]-1991
      whichY <- c(1:length(vars2recode))[is.element (vars2recode, whichLabels)]
      points (xy.coords(rep(surv[j]-1991, length(whichY)), whichY), pch=19)
   }
   items.available[[i]] <- vars2recode[is.element (vars2recode, unique (tempData$QuestionName))]
   count.items.available[[i]] <- table (tempData$QuestionName)
   dev.off()
}	

##########################################################################################
# Some descriptive stats about the number of items per legislature
##########################################################################################
data.frame (whichCountry, whichYear, countItems) # print all information
min (countItems) # minimum number of items (6)
whichCountry[which (countItems<6)] # No legislature now with anything less than 6 items
whichCountry[which (countItems==max(countItems))] # Legislatures with 14 items
round (mean (countItems), 2) # average number of items per legislature
##########################################################################################


#######################################################################
#### Presidents: how many weighted percentages close to 100% or 0% ####
#######################################################################
jagsDataPres <- finalData %>% filter(type=="President")

# Read labels for party that holds largest plurality in the country
PartyLabelsData <- read.csv (file="Datasets/FinishedOutput/PartyLabelsData.csv"
                             , header=TRUE)
levels (PartyLabelsData$pais) <- car::recode (levels (PartyLabelsData$pais)
                                              , "'Brasil'='Brazil'
                                              ; 'Costa Rica'='CostaRica'
                                              ; 'Republica Dominicana'='DominicanRep'
                                              ; 'El Salvador'='ElSalvador'")

################################################
#### Get policy moods for all policy-makers ####
################################################
# Load MCMC object
chainsConv <- rstan::As.mcmc.list(common_model)


# Summaries of posterior distribution
Mu <- rstan::extract (common_model,"mu_a")[[1]]

Sigma <- rstan::extract (common_model, "sigma")[[1]]

colMeans (Sigma[,actor.type=="Legislator" & country=="Mexico"])


# Citizen moods
Mu.cit <- Mu[,actor.type=="Citizen"]
# Mu.sep <- Mu.sep[,actor.type=="Citizen"]
pais.cit <- country[actor.type=="Citizen"]
anyo.cit <- year[actor.type=="Citizen"]

CitizenMoods <- c()
for (i in 1:ncol(Mu.cit)){
   temp <- quantile (Mu.cit[,pais.cit==pais.cit[i] & anyo.cit==anyo.cit[i]]
                     , prob=c(0.1,0.25,0.5,0.75,0.9))
   CitizenMoods <- rbind (CitizenMoods, temp)
}
CitizenMoods <- as.data.frame (CitizenMoods)
colnames (CitizenMoods) <- c("q10","q25","q50","q75","q90")
CitizenMoods$real.year <- anyo.cit
CitizenMoods$pais <- pais.cit
rownames (CitizenMoods) <- 1:nrow(CitizenMoods)

# Legislator moods
Mu.leg <- Mu[,actor.type=="Legislator"]
pais.leg <- country[actor.type=="Legislator"]
anyo.leg <- year[actor.type=="Legislator"]

LegislatorMoods <- c()
for (i in 1:ncol(Mu.leg)){
   temp <- quantile (Mu.leg[,pais.leg==pais.leg[i] & anyo.leg==anyo.leg[i]]
                     , prob=c(0.1,0.25,0.5,0.75,0.9))
   LegislatorMoods <- rbind (LegislatorMoods, temp)
}
LegislatorMoods <- as.data.frame (LegislatorMoods)
colnames (LegislatorMoods) <- c("q10","q25","q50","q75","q90")
LegislatorMoods$real.year <- anyo.leg
LegislatorMoods$pais <- pais.leg
rownames (LegislatorMoods) <- 1:nrow(LegislatorMoods)
LegislatorMoods$name <- paste (LegislatorMoods$pais, LegislatorMoods$real.year, sep="-")

# Senator moods
Mu.sen <- Mu[,actor.type=="Senator"]
pais.sen <- country[actor.type=="Senator"]
anyo.sen <- year[actor.type=="Senator"]

SenatorMoods <- c()
for (i in 1:ncol(Mu.sen)){
   temp <- quantile (Mu.sen[,pais.sen==pais.sen[i] & anyo.sen==anyo.sen[i]]
                     , prob=c(0.1,0.25,0.5,0.75,0.9))
   SenatorMoods <- rbind (SenatorMoods, temp)
}
SenatorMoods <- as.data.frame (SenatorMoods)
colnames (SenatorMoods) <- c("q10","q25","q50","q75","q90")
SenatorMoods$real.year <- anyo.sen
SenatorMoods$pais <- pais.sen
rownames (SenatorMoods) <- 1:nrow(SenatorMoods)
SenatorMoods$name <- paste (SenatorMoods$pais, SenatorMoods$real.year, sep="-")

# President moods
Mu.pres <- Mu[,actor.type=="President"]
pais.pres <- country[actor.type=="President"]
anyo.pres <- year[actor.type=="President"]

PresidentMoods <- c()
for (i in 1:ncol(Mu.pres)){
   temp <- quantile (Mu.pres[,pais.pres==pais.pres[i] & anyo.pres==anyo.pres[i]]
                     , prob=c(0.1,0.25,0.5,0.75,0.9))
   PresidentMoods <- rbind (PresidentMoods, temp)
}
PresidentMoods <- as.data.frame (PresidentMoods)
colnames (PresidentMoods) <- c("q10","q25","q50","q75","q90")
PresidentMoods$real.year <- anyo.pres
PresidentMoods$pais <- pais.pres
rownames (PresidentMoods) <- 1:nrow(PresidentMoods)
PresidentMoods$name <- paste (PresidentMoods$pais, PresidentMoods$real.year, sep="-")





#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
# Polarization measures
# Citizen polarization
Sigma.cit <- Sigma[,actor.type=="Citizen"]

CitizenPolar <- c()
for (i in 1:ncol(Sigma.cit)){
  temp <- quantile (Sigma.cit[,pais.cit==pais.cit[i] & anyo.cit==anyo.cit[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  CitizenPolar <- rbind (CitizenPolar, temp)
}
CitizenPolar <- as.data.frame (CitizenPolar)
colnames (CitizenPolar) <- c("q10","q25","q50","q75","q90")
CitizenPolar$real.year <- anyo.cit
CitizenPolar$pais <- pais.cit
rownames (CitizenPolar) <- 1:nrow(CitizenPolar)

# Legislator moods
Sigma.leg <- Sigma[,actor.type=="Legislator"]

LegislatorPolar <- c()
for (i in 1:ncol(Sigma.leg)){
  temp <- quantile (Sigma.leg[,pais.leg==pais.leg[i] & anyo.leg==anyo.leg[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  LegislatorPolar <- rbind (LegislatorPolar, temp)
}
LegislatorPolar <- as.data.frame (LegislatorPolar)
colnames (LegislatorPolar) <- c("q10","q25","q50","q75","q90")
LegislatorPolar$real.year <- anyo.leg
LegislatorPolar$pais <- pais.leg
rownames (LegislatorPolar) <- 1:nrow(LegislatorPolar)
LegislatorPolar$name <- paste (LegislatorPolar$pais, LegislatorPolar$real.year, sep="-")

# Senator moods
Sigma.sen <- Sigma[,actor.type=="Senator"]

SenatorPolar <- c()
for (i in 1:ncol(Sigma.sen)){
  temp <- quantile (Sigma.sen[,pais.sen==pais.sen[i] & anyo.sen==anyo.sen[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  SenatorPolar <- rbind (SenatorPolar, temp)
}
SenatorPolar <- as.data.frame (SenatorPolar)
colnames (SenatorPolar) <- c("q10","q25","q50","q75","q90")
SenatorPolar$real.year <- anyo.sen
SenatorPolar$pais <- pais.sen
rownames (SenatorPolar) <- 1:nrow(SenatorPolar)
SenatorPolar$name <- paste (SenatorPolar$pais, SenatorPolar$real.year, sep="-")

# President moods
Sigma.pres <- Sigma[,actor.type=="President"]

PresidentPolar <- c()
for (i in 1:ncol(Sigma.pres)){
  temp <- quantile (Sigma.pres[,pais.pres==pais.pres[i] & anyo.pres==anyo.pres[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  PresidentPolar <- rbind (PresidentPolar, temp)
}
PresidentPolar <- as.data.frame (PresidentPolar)
colnames (PresidentPolar) <- c("q10","q25","q50","q75","q90")
PresidentPolar$real.year <- anyo.pres
PresidentPolar$pais <- pais.pres
rownames (PresidentPolar) <- 1:nrow(PresidentPolar)
PresidentPolar$name <- paste (PresidentPolar$pais, PresidentPolar$real.year, sep="-")







#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
countryLegs <- countryCol
countryLegs[grep("Brasil", countryLegs)] <- "Brazil"
countryLegs[grep("Republica Dominicana", countryLegs)] <- "DominicanRep"
countryLegs[grep("El Salvador", countryLegs)] <- "ElSalvador"
countryLegs[grep("Costa Rica", countryLegs)] <- "CostaRica"

whichLegs <- paste (countryLegs, yearCol, sep="-")
# Change names(Legislator.Moods) for Argentina-2005 to Argentina-2007
LegislatorMoods$name[grep ("Argentina-2005", LegislatorMoods$name)] <- "Argentina-2007" # this one may not coincide
LegislatorPolar$name[grep ("Argentina-2005", LegislatorPolar$name)] <- "Argentina-2007" # this one may not # The next two vectors should be empty, if the matching works
LegislatorMoods$name[!is.element (LegislatorMoods$name, whichLegs)]
whichLegs[!is.element (whichLegs, LegislatorMoods$name)]
# check that the order in which Legislator.Moods is stored coincides with the order of whichLegs
identical (whichLegs, LegislatorMoods$name)   # will be false
# Don't panic: DominicanRep is sorted wrongly!
identical (sort(whichLegs), sort (LegislatorMoods$name))  # should be true

LegislatorMoods <- LegislatorMoods[order(LegislatorMoods$name),]
SenatorMoods <- SenatorMoods[order(SenatorMoods$name),]
PresidentMoods <- PresidentMoods[order(PresidentMoods$name),]
LegislatorPolar <- LegislatorPolar[order(LegislatorMoods$name),]
SenatorPolar <- SenatorPolar[order(SenatorMoods$name),]
PresidentPolar <- PresidentPolar[order(PresidentMoods$name),]

plotLegData <- data.frame (country=countryLegs
                           , survey.year=as.numeric(yearCol)
                           , survey=as.character (whichLegs)
                           , start.leg=as.numeric (substr (termCol, 1, 4))
                           , end.leg=as.numeric (substr (termCol, 6, 9))
                           , start.Sen=as.numeric (substr (termUpper, 1, 4))
                           , end.Sen=as.numeric (substr (termUpper, 6, 9))
                           , start.Pres=as.numeric (substr (termPres, 1, 4))
                           , end.Pres=as.numeric (substr (termPres, 6, 9)))

plotLegData$ccode <- car::recode (plotLegData$country, "'Argentina'='ARG';
                             'Bolivia'='BOL';
                             'Brazil'='BRZ';
                             'Chile'='CHL';
                             'Colombia'='COL';
                             'CostaRica'='CRC';
                             'Ecuador'='ECU';
                             'ElSalvador'='SLV';
                             'Guatemala'='GUA';
                             'Honduras'='HON';
                             'Mexico'='MEX';
                             'Nicaragua'='NIC';
                             'Panama'='PAN';
                             'Paraguay'='PRY';
                             'Peru'='PER';
                             'DominicanRep'='DOM';
                             'Uruguay'='URY';
                             'Venezuela'='VEN'")

plotLegData <- plotLegData[order(plotLegData$survey),]

Legislator.Moods <- merge (LegislatorMoods, plotLegData, by.x="name", by.y=as.character("survey"))
President.Moods  <- merge (PresidentMoods, plotLegData, by.x="name", by.y=as.character("survey"))  # Here we lose Argentina-2005
Senator.Moods    <- merge (SenatorMoods, plotLegData, by.x="name", by.y=as.character("survey"))

Legislator.Polar <- merge (LegislatorPolar, plotLegData, by.x="name", by.y=as.character("survey"))
President.Polar  <- merge (PresidentPolar, plotLegData, by.x="name", by.y=as.character("survey"))
Senator.Polar    <- merge (SenatorPolar, plotLegData, by.x="name", by.y=as.character("survey"))

rm (LegislatorMoods, PresidentMoods, SenatorMoods, LegislatorPolar, PresidentPolar, SenatorPolar)

# We need to correct President.Moods and Senate.Moods, because some presidents and senates are scored twice
samePres <- with (President.Moods
                  , paste (ccode, start.Pres, end.Pres, sep="-"))

sameSen <- with (Senator.Moods
                 , paste (ccode, start.Sen, end.Sen, sep="-"))

for (i in 1:length(samePres)) {
   if (duplicated(samePres)[i]==TRUE) {
      President.Moods$q10[i-1] <- (President.Moods$q10[i-1]+President.Moods$q10[i])/2
      President.Moods$q25[i-1] <- (President.Moods$q25[i-1]+President.Moods$q25[i])/2
      President.Moods$q50[i-1] <- (President.Moods$q50[i-1]+President.Moods$q50[i])/2
      President.Moods$q75[i-1] <- (President.Moods$q75[i-1]+President.Moods$q75[i])/2
      President.Moods$q90[i-1] <- (President.Moods$q90[i-1]+President.Moods$q90[i])/2
   } else { next } 
}
President.Moods <- President.Moods[duplicated(samePres)==FALSE,]

for (i in 1:length(sameSen)) {
   if (duplicated(sameSen)[i]==TRUE) {
      Senator.Moods$q10[i-1] <- (Senator.Moods$q10[i-1]+Senator.Moods$q10[i])/2
      Senator.Moods$q25[i-1] <- (Senator.Moods$q25[i-1]+Senator.Moods$q25[i])/2
      Senator.Moods$q50[i-1] <- (Senator.Moods$q50[i-1]+Senator.Moods$q50[i])/2
      Senator.Moods$q75[i-1] <- (Senator.Moods$q75[i-1]+Senator.Moods$q75[i])/2
      Senator.Moods$q90[i-1] <- (Senator.Moods$q90[i-1]+Senator.Moods$q90[i])/2
   } else { next } 
}
Senator.Moods <- Senator.Moods[duplicated(sameSen)==FALSE,]


for (i in 1:length(samePres)) {
  if (duplicated(samePres)[i]==TRUE) {
    President.Polar$q10[i-1] <- (President.Polar$q10[i-1]+President.Polar$q10[i])/2
    President.Polar$q25[i-1] <- (President.Polar$q25[i-1]+President.Polar$q25[i])/2
    President.Polar$q50[i-1] <- (President.Polar$q50[i-1]+President.Polar$q50[i])/2
    President.Polar$q75[i-1] <- (President.Polar$q75[i-1]+President.Polar$q75[i])/2
    President.Polar$q90[i-1] <- (President.Polar$q90[i-1]+President.Polar$q90[i])/2
  } else { next } 
}
President.Polar <- President.Polar[duplicated(samePres)==FALSE,]

for (i in 1:length(sameSen)) {
  if (duplicated(sameSen)[i]==TRUE) {
    Senator.Polar$q10[i-1] <- (Senator.Polar$q10[i-1]+Senator.Polar$q10[i])/2
    Senator.Polar$q25[i-1] <- (Senator.Polar$q25[i-1]+Senator.Polar$q25[i])/2
    Senator.Polar$q50[i-1] <- (Senator.Polar$q50[i-1]+Senator.Polar$q50[i])/2
    Senator.Polar$q75[i-1] <- (Senator.Polar$q75[i-1]+Senator.Polar$q75[i])/2
    Senator.Polar$q90[i-1] <- (Senator.Polar$q90[i-1]+Senator.Polar$q90[i])/2
  } else { next } 
}
Senator.Polar <- Senator.Polar[duplicated(sameSen)==FALSE,]


########################
#### Average graphs ####
########################
# Polarization graphs
# Graph for legislators
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "polarMcGannAllLegs.pdf", sep=""), h=5, w=9)
ggplot(Legislator.Polar, aes(start.leg, q50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
  ) +
  geom_text_repel(aes(label=ccode)
                  , box.padding=unit(0.45, "lines")
                  , size=geom.text.size) +
  geom_point(size=3, colour="gray") +
  geom_smooth(colour="black") +
  scale_x_continuous(name="", breaks=1993:2013
                     , labels=as.character(yearLabels)) +
  scale_y_continuous(name="Lower <--- Heterogeneity ---> Higher"
                     , breaks=c(0,1,2,3,4,5)
                     , labels=c(0,1,2,3,4,5))
dev.off()


# Graph for presidents
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "polarMcGannAllPresidents.pdf", sep=""), h=5, w=9)
ggplot(President.Polar, aes(start.Pres, q50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
  ) +
  geom_text_repel(aes(label=ccode)
                  , box.padding=unit(0.45, "lines")
                  , size=geom.text.size) +
  geom_point(size=3, colour="gray") +
  geom_smooth(colour="black") +
  scale_x_continuous(name="", breaks=1993:2013
                     , labels=as.character(yearLabels)) +
  scale_y_continuous(name="Lower <--- Heterogeneity ---> Higher"
                     , breaks=c(0,1,2,3,4,5)
                     , labels=c(0,1,2,3,4,5))
dev.off()


# Graph for senators
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "polarMcGannAllSenators.pdf", sep=""), h=5, w=9)
ggplot(Senator.Polar, aes(start.Sen, q50)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
  ) +
  geom_text_repel(aes(label=ccode)
                  , box.padding=unit(0.45, "lines")
                  , size=geom.text.size) +
  geom_point(size=3, colour="gray") +
  geom_smooth(colour="black") +
  scale_x_continuous(name="", breaks=1993:2013
                     , labels=as.character(yearLabels)) +
  scale_y_continuous(name="Lower <--- Heterogeneity ---> Higher"
                     , breaks=c(0,1,2,3,4,5)
                     , labels=c(0,1,2,3,4,5))
dev.off()









# Graph for legislators
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "moodMcGannAllLegs.pdf", sep=""), h=5, w=9)
ggplot(Legislator.Moods, aes(start.leg, q50)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
   ) +
   geom_text_repel(aes(label=ccode)
                   , box.padding=unit(0.45, "lines")
                   , size=geom.text.size) +
   geom_point(size=3, colour="gray") +
   geom_smooth(colour="black") +
   scale_x_continuous(name="", breaks=1993:2013
                      , labels=as.character(yearLabels)) +
   scale_y_continuous(name="Pro State <--- Mood ---> Pro Market"
                      , breaks=c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1)
                      , labels=c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1))
dev.off()


# Graph for presidents
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "moodMcGannAllPresidents.pdf", sep=""), h=5, w=9)
ggplot(President.Moods, aes(start.Pres, q50)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
   ) +
   geom_text_repel(aes(label=ccode)
                   , box.padding=unit(0.45, "lines")
                   , size=geom.text.size) +
   geom_point(size=3, colour="gray") +
   geom_smooth(colour="black") +
   scale_x_continuous(name="", breaks=1993:2013
                      , labels=as.character(yearLabels)) +
   scale_y_continuous(name="Pro State <--- Mood ---> Pro Market"
                      , breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5)
                      , labels=c(-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5))
dev.off()


# Graph for senators
## Adding confidence interval
geom.text.size = 3
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "moodMcGannAllSenators.pdf", sep=""), h=5, w=9)
ggplot(Senator.Moods, aes(start.Sen, q50)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
   ) +
   geom_text_repel(aes(label=ccode)
                   , box.padding=unit(0.45, "lines")
                   , size=geom.text.size) +
   geom_point(size=3, colour="gray") +
   geom_smooth(colour="black") +
   scale_x_continuous(name="", breaks=1993:2013
                      , labels=as.character(yearLabels)) +
   scale_y_continuous(name="Pro State <--- Mood ---> Pro Market"
                      , breaks=c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1)
                      , labels=c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1))
dev.off()


fit.l <- lm (q50 ~ as.factor(survey.year), data=Legislator.Moods)
fit.s <- lm (q50 ~ as.factor(survey.year), data=Senator.Moods)
fit.p <- lm (q50 ~ as.factor(survey.year), data=President.Moods)

anova (fit.l)
round (anova(fit.l)["Residuals", "Mean Sq"], 2) # within-year variance
round (anova(fit.l)["as.factor(survey.year)", "Mean Sq"], 2) # between-year variance

anova (fit.s)
round (anova(fit.s)["Residuals", "Mean Sq"], 2) # within-year variance
round (anova(fit.s)["as.factor(survey.year)", "Mean Sq"], 2) # between-year variance

anova (fit.p)
round (anova(fit.p)["Residuals", "Mean Sq"], 2) # within-year variance
round (anova(fit.p)["as.factor(survey.year)", "Mean Sq"], 2) # between-year variance



# Plots
# The next lines read auxiliary information to plot presidential paths
Pela <- LegMI    
Pela$pais <- car::recode (Pela$pais, "'Costa Rica'='CostaRica'; 'Brasil'='Brazil';
                     'El Salvador'='ElSalvador'; 'Republica Dominicana'='DominicanRep'")
Pela$pais <- as.factor (Pela$pais)
Pela$paisYear <- as.factor (paste (Pela$pais, Pela$year, sep="-"))
colnames (Pela)[grep("LR.ideology.self", colnames(Pela))] <- "left.right"
id <- names (by (Pela$PartyAcronym, Pela$paisYear, unique))

presidentialParties <- c("PJ","PJ","PJ","PJ","PJ"  # Argentina
                         ,"MNR","ADN","MNR","MAS"  # Bolivia
                         ,"PT","PT","PT"           # Brazil
                         ,"DC","DC","PS","PS","RN" # Chile
                         ,"PC","PL-U","U","U"      # Colombia
                         ,"PLN","PUSC","PUSC","PLN","PLN" # Costa Rica
                         ,"PRD","PLD","PRD","PLD","PLD" # DomRep
                         ,"PRE","DP","PRIAN","PRIAN","MPAIS" # Ecuador
                         ,"ARENA","ARENA","ARENA","ARENA","FMLN" # El Salvador
                         ,"PAN","FRG","Patriota","UNE","Patriota" # Guatemala
                         ,"PL","PL","PN","PL","PN"  # Honduras
                         ,"PRI","PRI","PAN","PAN","PAN","PAN" # Mexico
                         ,"AL","PLC","FSLN"  # Nicaragua
                         ,"PA","PRD","CD"   # Panama 
                         ,"ANR","ANR","ANR","Other"  # Paraguay
                         ,"Cambio90","PP","PAP"  # Peru
                         ,"PC","PC","FA","FA" # Uruguay
                         ,"AD","MVR")   # Venezuela

prezParties <- data.frame (presidentialParties=presidentialParties
                           , paisYear=id)

samePres <- c("Argentina-1997","Argentina-2005","ElSalvador-1997","ElSalvador-2003","Mexico-1997","Mexico-2003","Mexico-2009")
prezPartiesBis <- prezParties[!is.element(prezParties$paisYear, samePres),]

# Read information about presidents
prezPartiesBis$pais <- unlist (strsplit (as.character(prezPartiesBis$paisYear), split="-"))[odd(1:(nrow(prezPartiesBis)*2))]
prezPartiesBis$year <- unlist (strsplit (as.character(prezPartiesBis$paisYear), split="-"))[even(1:(nrow(prezPartiesBis)*2))]


countryNames <- c("Argentina","Bolivia","Brazil","Chile"
                  ,"Colombia","CostaRica","Ecuador","ElSalvador"
                  ,"Guatemala","Honduras","Mexico","Nicaragua"
                  ,"Panama","Paraguay","Peru","DominicanRep"
                  ,"Uruguay","Venezuela")



for (i in 1:length(countryLabels)){
   
   which.country <- c(1:18)[i]
   which.country.name <- countryNames[i]
   which.country.label <- countryLabels[i]
   
   pdf (paste0("~/Dropbox/ChainBook/BookManuscript/GraphsSept2019/pooledExpandedPlusMoodsByCountry"
                , which.country.name, ".pdf"), h=7, w=14)
   dat  <- Legislator.Moods[Legislator.Moods$country==which.country.name,]
   dat.s  <- Senator.Moods[Senator.Moods$country==which.country.name,]
   dat.p  <- President.Moods[President.Moods$country==which.country.name,]
   
   tmpPrez <- prezPartiesBis[prezPartiesBis$pais==which.country.name,]
   tmp <- PartyLabelsData[PartyLabelsData$pais==which.country.name,]
   md.cit <- CitizenMoods[CitizenMoods$pais==which.country.name, "q50"]
   yr.cit <- CitizenMoods[CitizenMoods$pais==which.country.name, "real.year"]
   md.pol <- Legislator.Moods[Legislator.Moods$pais==which.country.name, "q50"]
   yr.pol <- Legislator.Moods[Legislator.Moods$pais==which.country.name,"real.year"]


   par (srt=0, las=2, mar=c(4,3,1,1), mfrow=c(1,1))
   plot (c(1993,2015),c(-3,4), type="n", main="", ylab="", xlab="", axes=F)
   axis (2)
   axis (1, at=c(1993:2015), labels=c(1993:2015))
   par (las=0)
   mtext (side=2, line=2, text="Pro-market policy mood")
   legend ("bottomleft", legend=which.country.label, bty="n", cex=1.2)	
   for (j in 1:nrow(dat)){
      polygon (x=c(rep(dat$start.leg[j], 2), rep(dat$end.leg[j], 2))
               , y=c(dat$q25[j], dat$q75[j], dat$q75[j], dat$q25[j])
               , border=NA, col=rgb(1,10/255,10/255,0.5))
      segments (y0=dat$q50[j]
                , y1=dat$q50[j]
                , x0=dat$start.leg[j]
                , x1=dat$end.leg[j]
                , lwd=1, col=rgb(1,10/255,10/255,0.9))
      text (x=dat$start.leg[j] + 0.5*(dat$end.leg[j]-dat$start.leg[j])
            , y=dat$q75[j], pos=3
            , labels=paste0 (tmp$largestLHparty[j], " (", round(tmp$largestLHshare[j]*100, 1), ")")
            , cex=0.7, col="red")
   }
   for (j in 1:nrow(dat.p)){
      polygon (x=c(rep(dat.p$start.Pres[j], 2), rep(dat.p$end.Pres[j], 2))
               , y=c(dat.p$q25[j], dat.p$q75[j], dat.p$q75[j], dat.p$q25[j])
               , border=NA, col=rgb(0,5/255,1,0.5))
      segments (y0=dat.p$q50[j]
                , y1=dat.p$q50[j]
                , x0=dat.p$start.Pres[j]
                , x1=dat.p$end.Pres[j]
                , lwd=1, col=rgb(0,5/255,1,0.9))
      text (x=dat.p$start.Pres[j] + 0.5*(dat.p$end.Pres[j]-dat.p$start.Pres[j])
            , y=dat.p$q50[j], pos=3
            , labels=tmpPrez$presidentialParties[j]
            , cex=0.7, col="blue")
   }
   if (which.country.name=="Mexico") {tmp <- tmp[c(1,2,3,5),]}  # fix for Mexico's Senate
   if (nrow(dat.s) >= 1) {
      for (j in 1:nrow(dat.s)){
         polygon (x=c(rep(dat.s$start.Sen[j], 2), rep(dat.s$end.Sen[j], 2))
                  , y=c(dat.s$q25[j], dat.s$q75[j], dat.s$q75[j], dat.s$q25[j])
                  , border=NA, col=rgb(1,135/255,0,0.5))
         segments (y0=dat.s$q50[j]
                   , y1=dat.s$q50[j]
                   , x0=dat.s$start.Sen[j]
                   , x1=dat.s$end.Sen[j]
                   , lwd=1, col=rgb(1,135/255,0,0.9))
         text (x=dat.s$start.Sen[j] + 0.5*(dat.s$end.Sen[j]-dat.s$start.Sen[j])
               , y=dat.s$q25[j], pos=1
               , labels=paste0 (tmp$largestUHparty[j], " (", round(tmp$largestUHshare[j]*100, 1), ")")
               , cex=0.7, col="orange")
      }
      legend ("bottomright", legend=c("President","Lower House","Upper House")
              , bty="n", col=c("blue","red","orange"), pch=19)
   } else {
      legend ("bottomright", legend=c("President","Lower House")
              , bty="n", col=c("blue","red"), pch=19)
   }
   dev.off()
}


# In glorious black and white
grisLeg  <- rgb(128/255,128/255,128/255, alpha=0.7)
grisSen  <- rgb(211/255,211/255,211/255, alpha=0.7)
grisPrez <- rgb(60/255,60/255,60/255, alpha=0.7)


Legislator.Moods <- left_join(Legislator.Moods, dplyr::select(PartyLabelsData,
                                        c("pais", real.year="matchYear",
                                          "largestLHparty",
                                             "largestLHshare")))
Senator.Moods <- left_join(Senator.Moods, dplyr::select(PartyLabelsData,
                                                 c("pais", real.year="matchYear",
                                                   "largestUHparty",
                                                   "largestUHshare")))
                       
President.Moods <- left_join(President.Moods,
                         dplyr::select(prezPartiesBis,c(name="paisYear",
                                        presidentialParties="presidentialParties")))
President.Moods$share=1



moods_for_plot <- Legislator.Moods %>%
  dplyr::rename(start_yr=start.leg, end_yr=end.leg) %>%
  mutate(Actor="Lower House") %>%
  dplyr::select(pais, start_yr, end_yr, q25, q50, q75, Actor, party=largestLHparty, share=largestLHshare) %>%
  bind_rows(dplyr::mutate(dplyr::select(dplyr::rename(Senator.Moods, start_yr=start.Sen, end_yr=end.Sen, party=largestUHparty, share=largestUHshare), pais, start_yr, end_yr, q25, q50, q75, party, share), Actor="Upper House")) %>%
  bind_rows(dplyr::mutate(dplyr::select(dplyr::rename(President.Moods, start_yr=start.Pres, end_yr=end.Pres), pais, start_yr, end_yr, q25, q50, q75, party=presidentialParties, share), Actor="President")) %>%
  mutate(pais = dplyr::recode(.$pais,
                              "DominicanRep" = "Dominican Republic",
                              "ElSalvador" = "El Salvador",
                              "CostaRica" = "Costa Rica"))%>%
  mutate(Actor = factor(Actor, levels=c("Lower House", "Upper House","President")))
poly_df <- do.call(rbind, lapply(1:nrow(moods_for_plot),
                  function(x){
                    data.frame(x_poly=c(rep(moods_for_plot[x,"start_yr"], 2), rep(moods_for_plot[x,"end_yr"], 2)),
                               y_poly=c(moods_for_plot[x,"q25"], moods_for_plot[x,"q75"], moods_for_plot[x,"q75"], moods_for_plot[x,"q25"]),
                               pais = moods_for_plot[x,"pais"],
                               Actor = moods_for_plot[x,"Actor"],
                               party = moods_for_plot[x,"party"],
                               share = moods_for_plot[x,"share"],
                               grp = x)
                  }))
moods_for_plot <- moods_for_plot %>%
  mutate(text_lab = ifelse(Actor == "President",party,paste0(party, " (",round(share*100, 2), ")")))
pdf (paste0(graphicsPath, "pooledExpandedPlusMoodsByCountryBW.pdf"), h=5, w=16.5)
ggplot(moods_for_plot, aes(x=start_yr, y = q50, fill=Actor,group=Actor)) +
  facet_wrap(~pais, nrow=3) + 
  scale_fill_manual(values=c(grisLeg, grisSen, grisPrez)) +
  scale_color_manual(values=c(grisLeg, grisSen, grisPrez)) +
  geom_segment(aes(xend=end_yr,yend=q50, col=Actor), position="dodge",alpha=1) +
  geom_label(aes(label=text_lab,
                 x = start_yr + 0.5*(end_yr - start_yr),
                 y = case_when(Actor == "Lower House" ~ q25-0.25,
                               Actor == "Upper House" ~ q75+0.5,
                               Actor == "President" ~ q50+0.55)),
             cex=2, #segment.size = 0.05,box.padding = 0.1, segment.size = 0.05,
                  fill="white", 
             label.padding = unit(0.10, "lines"))+
  geom_polygon(aes(x=x_poly, y=y_poly, group=grp), position="dodge",data=poly_df) + 
  scale_x_continuous(breaks = seq(1993, 2015, by=3))+
  theme_bw()+
  theme(legend.position="bottom")+
  xlab("Year")+
  ylab("Pro State <--- Mood ---> Pro Market")
dev.off()
  

for (i in 1:length(countryLabels)){
   
   which.country <- c(1:18)[i]
   which.country.name <- countryNames[i]
   which.country.label <- countryLabels[i]
   
   pdf (paste0(graphicsPath,"pooledExpandedPlusMoodsByCountryBW"
               , which.country.name, ".pdf"), h=7, w=14)
   dat  <- Legislator.Moods[Legislator.Moods$country==which.country.name,]
   dat.s  <- Senator.Moods[Senator.Moods$country==which.country.name,]
   dat.p  <- President.Moods[President.Moods$country==which.country.name,]
   
   tmpPrez <- prezPartiesBis[prezPartiesBis$pais==which.country.name,]
   tmp <- PartyLabelsData[PartyLabelsData$pais==which.country.name,]
   md.cit <- CitizenMoods[CitizenMoods$pais==which.country.name, "q50"]
   yr.cit <- CitizenMoods[CitizenMoods$pais==which.country.name, "real.year"]
   md.pol <- Legislator.Moods[Legislator.Moods$pais==which.country.name, "q50"]
   yr.pol <- Legislator.Moods[Legislator.Moods$pais==which.country.name,"real.year"]
   
   par (srt=0, las=2, mar=c(4,3,1,1), mfrow=c(1,1))
   plot (c(1993,2015),c(-3,4), type="n", main="", ylab="", xlab="", axes=F)
   axis (2)
   axis (1, at=c(1993:2015), labels=c(1993:2015))
   par (las=0)
   mtext (side=2, line=2, text="Pro-market policy mood")
   legend ("bottomleft", legend=which.country.label, bty="n", cex=1.2)	
   for (j in 1:nrow(dat)){
      polygon (x=c(rep(dat$start.leg[j], 2), rep(dat$end.leg[j], 2))
               , y=c(dat$q25[j], dat$q75[j], dat$q75[j], dat$q25[j])
               , border=NA, col=grisLeg)
      segments (y0=dat$q50[j]
                , y1=dat$q50[j]
                , x0=dat$start.leg[j]
                , x1=dat$end.leg[j]
                , lwd=1, col="black")

   }
   for (j in 1:nrow(dat.p)){
      polygon (x=c(rep(dat.p$start.Pres[j], 2), rep(dat.p$end.Pres[j], 2))
               , y=c(dat.p$q25[j], dat.p$q75[j], dat.p$q75[j], dat.p$q25[j])
               , border=NA, col=grisPrez)
      segments (y0=dat.p$q50[j]
                , y1=dat.p$q50[j]
                , x0=dat.p$start.Pres[j]
                , x1=dat.p$end.Pres[j]
                , lwd=1, col="black")
      
   }
   if (which.country.name=="Mexico") {tmp <- tmp[c(1,2,3,5),]}  # fix for Mexico's Senate
   if (nrow(dat.s) >= 1) {
      for (j in 1:nrow(dat.s)){
         polygon (x=c(rep(dat.s$start.Sen[j], 2), rep(dat.s$end.Sen[j], 2))
                  , y=c(dat.s$q25[j], dat.s$q75[j], dat.s$q75[j], dat.s$q25[j])
                  , border=NA, col=grisSen)
         segments (y0=dat.s$q50[j]
                   , y1=dat.s$q50[j]
                   , x0=dat.s$start.Sen[j]
                   , x1=dat.s$end.Sen[j]
                   , lwd=1, col="black")
         text (x=dat.s$start.Sen[j] + 0.5*(dat.s$end.Sen[j]-dat.s$start.Sen[j])
               , y=dat.s$q25[j], pos=1
               , labels=paste0 (tmp$largestUHparty[j], " (", round(tmp$largestUHshare[j]*100, 1), ")")
               , cex=0.7, col="black")
      }
      legend ("bottomright", legend=c("President","Lower House","Upper House")
              , bty="n", col=c(grisPrez,grisLeg,grisSen), pch=19)
   } else {
      legend ("bottomright", legend=c("President","Lower House")
              , bty="n", col=c(grisPrez,grisLeg), pch=19)
   }
   for (j in 1:nrow(dat)){
      text (x=dat$start.leg[j] + 0.5*(dat$end.leg[j]-dat$start.leg[j])
            , y=dat$q75[j], pos=3
            , labels=paste0 (tmp$largestLHparty[j], " (", round(tmp$largestLHshare[j]*100, 1), ")")
            , cex=0.7, col="black")
   }
   for (j in 1:nrow(dat.p)){
      text (x=dat.p$start.Pres[j] + 0.5*(dat.p$end.Pres[j]-dat.p$start.Pres[j])
            , y=dat.p$q50[j], pos=3
            , labels=tmpPrez$presidentialParties[j]
            , cex=0.7, col="black")
   }
   dev.off()
}


# Some specific values reported in the book
round (Legislator.Moods$q50[Legislator.Moods$country=="Argentina"], 2)
round (Legislator.Moods$q50[Legislator.Moods$country=="Mexico"], 2)


#######################################################
#### Comparing legislature and senate policy moods ####
#### against a bunch of other stuff                ####
#######################################################

##################################################################
# Build "legislature-revealed rightism, combining LH seat shares
# with expert party placement scores from Baker and Greene,
# then lay over our mood indicators
##################################################################
dataPath  <- "Datasets/OriginalDataFiles/"
partyWeights <- read.csv (paste0 (dataPath, "NewPartyWeightsOld.csv"), header=T)

levels(partyWeights$Country)[grep("Brasil", levels(partyWeights$Country))] <- "Brazil"
levels(partyWeights$Country)[grep("Costa Rica", levels(partyWeights$Country))] <- "CostaRica"
levels(partyWeights$Country)[grep("El Salvador", levels(partyWeights$Country))] <- "ElSalvador"
levels(partyWeights$Country)[grep("Republica Dominicana", levels(partyWeights$Country))] <- "DominicanRep"

partyWeights$id <- paste (partyWeights$Country, partyWeights$CorrectTerm, sep="/")

revLegRight <- c()
for (i in 1:length (unique(partyWeights$id))) {
   whichLeg <- unique(partyWeights$id)[i]
   seatShare <- partyWeights$Seat.ShareLH[partyWeights$id==whichLeg & !is.na(partyWeights$Ideology.Score)]
   ideology  <- partyWeights$Ideology.Score[partyWeights$id==whichLeg & !is.na(partyWeights$Ideology.Score)]
   propSeatShare <- seatShare/sum(seatShare)
   revLegRight <- c(revLegRight, as.numeric (propSeatShare %*% ideology))
}
names (revLegRight) <- unique (partyWeights$id)

Legislator.Moods$id <- paste (Legislator.Moods$pais, "/"
                              , Legislator.Moods$start.leg, "-"
                              , Legislator.Moods$end.leg, sep="")

# Legislatures for which we lack survey data
names (revLegRight)[!is.element (names (revLegRight), Legislator.Moods$id)]
# Surveys for which we lack legislative data (should be empty, return character(0))
Legislator.Moods$id[!is.element (Legislator.Moods$id, names (revLegRight))]

# Build measure of revealedRightism in the legislature
revealedRightism <- c()
for (i in 1:length (Legislator.Moods$id)) {
   revealedRightism[i] <- revLegRight[which (names(revLegRight)==Legislator.Moods$id[i])]	
}
Legislator.Moods$revealedRightism <- revealedRightism

# Plotting all measures for a single country is too confusing
# We are better off with scatterplots of policy-mood against each measure
# Overall correlation between revealedRightism and policy.moods
pdf (paste (graphicsPath, "LegMoodVSrevealedRight", ".pdf", sep=""), h=5, w=9)
par (mar=c(5,4,1,1), las=1)
plot (Legislator.Moods$q50, Legislator.Moods$revealedRightism
      , bty="n", pch=19, col="grey"
      , xlab=""
      , ylab="", xlim=c(-1,2.5))
mtext (text="Legislative policy moods", side=1, line=3)
mtext (side=1, line=2, text="Pro State <--- Mood ---> Pro Market", cex=0.9)
par (las=0)
mtext (text="Revealed legislative rightism", side=2, line=2.5)
abline (lm(Legislator.Moods$revealedRightism~Legislator.Moods$q50), lty=1)

text (xy.coords(Legislator.Moods$q50[76], Legislator.Moods$revealedRightism[76])
      , labels=Legislator.Moods$id[76], pos=4, cex=0.8)
rho <- cor.test.plus(cor.test (x=Legislator.Moods$q50, y=Legislator.Moods$revealedRightism))
r1 <- as.numeric (round (rho[[1]]$estimate, 2))
r2 <- round (rho$Standard.Error, 2)
legend ("bottomleft", bty="n", legend=expression ( hat(rho)==0.22))
dev.off ()



############################################################################
#### Now use left-right self-placement
############################################################################
LegMI$weighted.LRself <- LegMI$weight * LegMI$LR.ideology.self

average.LRparty <- c()
for (i in 1:length (unique (LegMI$id))) {
   idIN <- unique (LegMI$id)[i]
   temp <- LegMI[LegMI$id==idIN,]
   if (sum (!is.na(temp$LR.party.ideology)) <= 10) { rile <- NA }
   else if (invalid (temp$LR.party.ideology)) { rile <- NA } else {
      rile <- as.numeric (with (temp, by(LR.party.ideology, PartyCode, mean, na.rm=TRUE)))
      rile <- ifelse (is.na(rile), 0, rile)
   }
   wt <- as.numeric (with (temp, by(Seat.ShareLH, PartyCode, unique)))
   if (sum(wt) != 1) {
      wt <- wt / sum(wt)
   }
   wtRile <- rile %*% wt
   if (length(wtRile) > 1) { wtRile <- NA }
   average.LRparty <- c(average.LRparty, wtRile)
}

avgLeftRightSelf  <-  with (LegMI, by (weighted.LRself, id, mean, na.rm=T))
avgLeftRightParty <- average.LRparty


# We need names that will allow merging with plotLegData
newNames <- paste (unlist (strsplit (names (avgLeftRightSelf), split="/"))[seq(1,(length(avgLeftRightSelf)*3),by=3)]
                   , unlist (strsplit (names (avgLeftRightSelf), split="/"))[seq(3,(length(avgLeftRightSelf)*3),by=3)]
                   , sep="/")

# Check that names coincide, if so, simply add avgLeftRight* to plotLegData
plotLegData$pais <- car::recode (plotLegData$country, "'Brazil'='Brasil';
                                  'CostaRica'='Costa Rica';
                                  'DominicanRep'='Republica Dominicana';
                                  'ElSalvador'='El Salvador'")
plotLegData$id <- paste0 (plotLegData$pais, "/", plotLegData$start.leg, "-", plotLegData$end.leg)
plotLegData$id2 <- paste0 (plotLegData$country, "/", plotLegData$start.leg, "-", plotLegData$end.leg)
newNames[which(!is.element (plotLegData$id, newNames))]
newNames <- ifelse (newNames=="Argentina/2007-2011", "Argentina/2007-2009", newNames)

plotLegData <- plotLegData[order (plotLegData$id),]
cbind (plotLegData$id, newNames)   # Check that order is identical

plotLegData$avgLeftRightSelf  <- avgLeftRightSelf

# We need more names that will allow merging avgLeftRightParty with plotLegData
newNamesParty <- paste (unlist (strsplit (unique (LegMI$id), split="/"))[seq(1,(length(unique (LegMI$id))*3),by=3)]
                        , unlist (strsplit (unique (LegMI$id), split="/"))[seq(3,(length(unique (LegMI$id))*3),by=3)]
                        , sep="/")

# Check that names coincide, if so, simply add avgLeftRight* to plotLegData
cbind (plotLegData$id, sort(newNamesParty))
plotLegData$avgLeftRightParty <- avgLeftRightParty[order(newNamesParty)][order (plotLegData$id)]

# Mostly high correspondence, except for one single data point: Ecuador 2007
with (plotLegData, plot (avgLeftRightSelf, avgLeftRightParty, pch=19))
which (plotLegData$avgLeftRightSelf==min (plotLegData$avgLeftRightSelf, na.rm=T))

############################################################################
############################################################################
#### Aldrich-McKelvey measures for legislators?
############################################################################
############################################################################
# Import file produced by basicspacePELA.R
load (paste0 (dataPath, "bbLegMoods.RData"))
summary (result$individuals[[1]]$c1)
result$individuals[[1]]$weightedMood <- result$individuals[[1]]$weights * (result$individuals[[1]]$c1 * (-1))
result$individuals[[1]]$pais <- substr (result$individuals[[1]]$paisYear, start=1, stop=nchar(result$individuals[[1]]$paisYear)-4)
result$individuals[[1]]$pais <- car::recode (result$individuals[[1]]$pais
                                        , "'Brasil'='Brazil'
                                        ; 'RepublicaDominicana'='DominicanRep'")
result$individuals[[1]]$id <- paste (result$individuals[[1]]$pais
                                     , result$individuals[[1]]$Term
                                     , sep="/")
avgAMscore  <- with (result$individuals[[1]], by (weightedMood, id, mean, na.rm=T))

# Names do not coincide, because we only have 57 observations for A-M scores
names (avgAMscore)[!is.element(names(avgAMscore), plotLegData$id2)]
# These are the missing surveys
plotLegData$id2[!is.element(plotLegData$id2, names(avgAMscore))]

AMscore <- c()
for (i in 1:length (plotLegData$id2)) {
   dat <- plotLegData$id2[i]
   if (length(which (names(avgAMscore)==dat))==0) { 
      y <- NA 
   } else {
      y <- as.numeric (avgAMscore[which (names(avgAMscore)==dat)])
   }
   AMscore <- c (AMscore, y)
}

plotLegData$AMscore <- AMscore


#### Individual-level correlations between A-M and self LR placement ####
cor.test.plus(cor.test (x=result$individuals[[1]]$SelfIdeol, y=result$individuals[[1]]$c1*(-1))) 

for (i in 1:length(unique(plotLegData$id2))) {
   ps <- unique (plotLegData$id2)[i]
   dat <- cbind (result$individuals[[1]]$SelfIdeol[plotLegData$id2==ps], result$individuals[[1]]$c1[plotLegData$id2==ps]*(-1))
   print (paste (ps, round (cor (dat, use="p")[1,2], 2), sep="-"))
}

# Between LR self placement and own party LR placement
cor.test.plus (cor.test (x=LegMI$LR.party.ideology, y=LegMI$LR.ideology.self))

# Add McGann legislative scores
Legislator.Moods$id[!is.element (Legislator.Moods$id, plotLegData$id2)]
plotLegData$id2[!is.element (plotLegData$id2, Legislator.Moods$id)]

# reorder plotLegData
plotLegData <- plotLegData[order (plotLegData$id2),]
plotLegData$policy.moods <- Legislator.Moods$q50
plotLegData$revealedRightism <- Legislator.Moods$revealedRightism

# Find out about correlations
corTable <- cor (cbind (plotLegData$avgLeftRightSelf
                        , plotLegData$avgLeftRightParty
                        , plotLegData$policy.moods
                        , plotLegData$revealedRightism
                        # , plotLegData$avgAMscore.notWeighted
                        , plotLegData$AMscore), use="p")
rownames (corTable) <- colnames (corTable) <- c("LR.self","LR.party","Mood","Rightism","A-M")
print (round (corTable, 2))

cor.test.plus(cor.test (x=plotLegData$AMscore, y=plotLegData$policy.moods)) 
cor.test.plus(cor.test (x=plotLegData$policy.moods, y=plotLegData$revealedRightism)) 
cor.test.plus(cor.test (x=plotLegData$avgLeftRightSelf, y=plotLegData$policy.moods)) 
cor.test.plus(cor.test (x=plotLegData$avgLeftRightParty, y=plotLegData$policy.moods))

cor.test.plus(cor.test (x=plotLegData$avgLeftRightParty, y=plotLegData$avgLeftRightSelf))
cor.test.plus(cor.test (x=plotLegData$avgLeftRightSelf, y=plotLegData$revealedRightism)) 


#### Plot the alternative measures against our policy-mood measure.
## Mean policy.moods: -0.578
## SD policy.moods: 0.57
## Mean revealedRightism: 12.28
## SD revealedRightism: 1.98
plotLegData$rescaledRightism <- (plotLegData$revealedRightism-mean(plotLegData$revealedRightism)) / sd (plotLegData$revealedRightism)
plotLegData$rescaledPolicyMood <- (plotLegData$policy.moods-mean(plotLegData$policy.moods)) / sd (plotLegData$policy.moods)
plotLegData$rescaledLRself  <- (plotLegData$avgLeftRightSelf-mean(plotLegData$avgLeftRightSelf, na.rm=T)) / sd (plotLegData$avgLeftRightSelf, na.rm=T)
plotLegData$rescaledAM  <- (plotLegData$AMscore-mean(plotLegData$AMscore, na.rm=T)) / sd (plotLegData$AMscore, na.rm=T)

mnpm <- mean (plotLegData$policy.moods)
sdpm <- sd (plotLegData$policy.moods)
mnrr <- mean (plotLegData$rescaledRightism)
sdrr <- sd (plotLegData$rescaledRightism)

# Country-specific plots of available legislature policy moods
for (i in 1:length (unique (plotLegData$country))) {
   pais <- unique (plotLegData$country)[i]
   dat  <- plotLegData[plotLegData$country==pais,]
   pdf (paste (graphicsPath, "moodMcGannLegs", pais, ".pdf", sep=""), h=5, w=9)
   par (mar=c(4,3,1,1), las=3, mfrow=c(1,1))
   plot (c(1, length(yearLabels)), c(-3, 3)
         , type="n", axes=F, ylab="", xlab="")
   axis (1, at=c(1:length(yearLabels)), labels=yearLabels)
   axis (2, at=c(-3,-2,-1,0,1,2,3)
         , labels=round (c(mnpm-3*sdpm, mnpm-2*sdpm, mnpm-sdpm, mnpm, mnpm+sdpm, mnpm+2*sdpm, mnpm+3*sdpm), 1))
   mtext (side=2, line=2, text="Pro State <--- Mood ---> Pro Market")
   points (xy.coords (dat$start.leg-1992, dat$policy.moods), pch=19, col="black", cex=1.4)
   # Uncomment the following lines to plot every measure that we have
   points (xy.coords (dat$start.leg-1992, dat$rescaledRightism), pch=19, col="red")
   points (xy.coords (dat$start.leg-1992, dat$rescaledLRself), pch=19, col="blue", bg="white")
   points (xy.coords (dat$start.leg-1992, dat$rescaledAM), pch=19, col="green")
   points (xy.coords (dat$survey.year-1991, dat$policy.moods), pch=4)
   for (j in 1:nrow(dat)){
      segments (y0=dat$policy.moods[j]
                , y1=dat$policy.moods[j]
                , x0=dat$start.leg[j]-1992
                , x1=dat$end.leg[j]-1992
                , lwd=4)
      segments (y0=dat$rescaledLRself[j]
                , y1=dat$rescaledLRself[j]
                , x0=dat$start.leg[j]-1992
                , x1=dat$end.leg[j]-1992
                , lwd=3, col="blue")
      segments (y0=dat$rescaledAM[j]
                , y1=dat$rescaledAM[j]
                , x0=dat$start.leg[j]-1992
                , x1=dat$end.leg[j]-1992
                , lwd=3, col="green")
      segments (y0=dat$rescaledRightism[j]
                , y1=dat$rescaledRightism[j]
                , x0=dat$start.leg[j]-1992
                , x1=dat$end.leg[j]-1992
                , lwd=3, col="red")
   }
   labelPais <- c("Argentina","Bolivia","Brazil","Chile"
                  ,"Colombia","Costa Rica","Ecuador","El Salvador"
                  ,"Guatemala","Honduras","Mexico","Nicaragua"
                  ,"Panama","Paraguay","Peru","Dominican Republic"
                  ,"Uruguay","Venezuela")
   legend ("topleft", legend=labelPais[i], cex=1.2, bty="n")
   legend ("bottomleft"
           , ncol=3
           , border=c("black","green","blue","red")
           , fill=c("black","green","blue","red")
           , cex=0.8
           , legend=c("Policy mood"
                      ,"Blackbox score"
                      , "Left-right self-placement"
                      , "Legislative rightism"
           )
           , bty="n")

   dev.off()
}



pdf (paste (graphicsPath, "LegMoodVSaldrichMckelvey", ".pdf", sep=""), h=5, w=9)
par (mar=c(5,4,1,1), las=1)
plot (plotLegData$policy.moods, plotLegData$AMscore
      , bty="n", pch=19, col="grey"
      , xlab=""
      , ylab="")
mtext (text="Legislative policy moods", side=1, line=3)
mtext (side=1, line=2, text="Pro State <--- Mood ---> Pro Market", cex=0.9)
par (las=0)
mtext (text="Aldrich-McKelvey scores (Blackbox)", side=2, line=2.5)
abline (lm(plotLegData$AMscore~plotLegData$policy.moods), lty=1)
legend ("bottomright", bty="n", legend=expression ( hat(rho)==0.65))
dev.off ()

pdf (paste (graphicsPath, "LegMoodVSleftRightSelf", ".pdf", sep=""), h=5, w=9)
par (mar=c(5,4,1,1), las=1)
plot (plotLegData$policy.moods, plotLegData$avgLeftRightSelf
      , bty="n", pch=19, col="grey"
      , xlab=""
      , ylab="")
mtext (text="Legislative policy moods", side=1, line=3)
mtext (side=1, line=2, text="Pro State <--- Mood ---> Pro Market", cex=0.9)
par (las=0)
mtext (text="Average Left-Right self-placement", side=2, line=2.5)
abline (lm(plotLegData$avgLeftRightSelf~plotLegData$policy.moods), lty=1)
legend ("bottomright", bty="n", legend=expression ( hat(rho)==0.19))
dev.off ()

pdf (paste (graphicsPath, "LegMoodVSleftRightParty", ".pdf", sep=""), h=5, w=9)
par (mar=c(5,4,1,1), las=1)
plot (plotLegData$policy.moods, plotLegData$avgLeftRightParty
      , bty="n", pch=19, col="grey"
      , xlab=""
      , ylab="")
mtext (text="Legislative policy moods", side=1, line=3)
mtext (side=1, line=2, text="Pro State <--- Mood ---> Pro Market", cex=0.9)
par (las=0)
mtext (text="Average Left-Right party placement", side=2, line=2.5)
abline (lm(plotLegData$avgLeftRightParty~plotLegData$policy.moods), lty=1)
badLegs <- plotLegData$id[plotLegData$avgLeftRightParty < 2 & !is.na(plotLegData$avgLeftRightParty)]
text (xy.coords(plotLegData$policy.moods[plotLegData$avgLeftRightParty<3 & !is.na(plotLegData$avgLeftRightParty)]
                , plotLegData$avgLeftRightParty[plotLegData$avgLeftRightParty<3 & !is.na(plotLegData$avgLeftRightParty)])
      , labels=badLegs
      , pos=3, cex=0.8)
legend ("bottomright", bty="n", legend=expression ( hat(rho)==0.08))
dev.off ()

####################################
#### presidents vs legislatures ####
####################################
President.Moods$id <- paste0 (President.Moods$country, "/", President.Moods$start.Pres
                             , "-", President.Moods$end.Pres)
commonData <- intersect (Legislator.Moods$id, President.Moods$id)
leg.q50  <- Legislator.Moods$q50[is.element (Legislator.Moods$id, commonData)]
exec.q50 <- President.Moods$q50[is.element (President.Moods$id, commonData)]

# Expert scores for presidents' parties, from "presidentElections.txt"
add.PresParty.expert.score <- c(16.7273, 8.35, 8.35 #Arg
, 14.6875, 16.7, 14.6875, 3.875 #Bol
, 8.14815, 8.14815, 6.4 # Brz
, 10.1429, 10.1429, 7.904, 6.2381, 16.333 #Chi
, 17.133, 18.2, 18.2, 17.6666 #Col
, 12.45, 15, 15, 13.2727, 12.454545 #CR
, 9, 12.7143, 9, 12.7143, 12.7143 #DR
, 11.8, 12.2, 18, 18, 4.2 #Ecu
, 18.5, 18.5, 2 # Sal
, 15.125, 15.875, 16.125, 8.375, 16.857 #Guate
, 15, 15, 17.33, 15.667, 17.33 # Hon
, 11.55, 17.35, 16.45 # Mex
, 17.667, 18, 8.55 # Nic
, 16.889, 11.33, 17.875 # Pan
, 15.7, 15.7, 14.4, 5.4 #Par
, 15, 13.18, 12.4545 # Per
, 14.47, 14.47, 6.64, 5.235 # Uru
, 11.35, 2.65) #Ven



prez.VRR <- read.table ("Datasets/OriginalDataFiles/BakerGreene_WorldPolitics/presidentialVRLusingBakerGreeneDofile.txt"
                        , sep="\t", header=T)
prez.VRR$name <- paste (prez.VRR$country, prez.VRR$year, sep="-")
President.Moods$name[!is.element (President.Moods$name, prez.VRR$merge.with)]

Prez.KimFording <- merge (President.Moods, prez.VRR, by.x="name", by.y="merge.with", all.x=T)
Prez.KimFording$VRR[Prez.KimFording$name=="Chile-2002"] <- 12.55989
Prez.KimFording$text_label <- paste(Prez.KimFording$ccode, Prez.KimFording$start.Pres, sep="-")

pdf (paste0 (graphicsPath, "presVsKimFording.pdf"), h=5, w=7)
ggplot(Prez.KimFording, aes(x=VRR, y=q50)) +
  geom_smooth(method="lm", se=FALSE, col="black")+
  geom_text_repel(aes(label=text_label)) +
  xlab("Voter revealed rightism (presidential vote share)") +
  ylab("Executive policy mood")+
  theme_bw()
dev.off ()


par (mar=c(3,3,1,1))
plot (Prez.KimFording$q50~Prez.KimFording$VRR, type="n", axes=F
      , xlab=""
      , ylab=""
      , xlim=c(4,18)
      , ylim=c(-0.5,3.5))
axis (1)
axis (2)
mtext (side=1, line=2, text="Voter revealed rightism (presidential vote share)")
mtext (side=2, line=2, text="Executive policy mood")
text (xy.coords(Prez.KimFording$VRR,Prez.KimFording$q50)
      , labels=paste(Prez.KimFording$ccode, Prez.KimFording$start.Pres, sep="-")
      , cex=0.8)
abline (lm(Prez.KimFording$q50~Prez.KimFording$VRR), lwd=1)
cor (cbind (Prez.KimFording$q50, Prez.KimFording$VRR))
legend ("topleft", bty="n", legend=expression ( hat(rho)==0.2))

President.Moods$score <- add.PresParty.expert.score
President.Moods$text_label <- paste(President.Moods$ccode, President.Moods$start.Pres, sep="-")
pdf (paste0 (graphicsPath, "presVsExperts.pdf"), h=5, w=7)
setEPS()
postscript("EPS format/4.8.eps", height=5, width=7)
ggplot(President.Moods, aes(x=score,
                            y=q50, label=text_label))+
  geom_smooth(se=FALSE, method="lm", col="black")+
  geom_text_repel()+
  theme_bw()+
  xlab("President's party, expert score")+
  ylab("Executive policy mood")
dev.off ()

par (mar=c(3,3,1,1))
plot (President.Moods$q50~add.PresParty.expert.score, type="n", axes=F
      , xlab=""
      , ylab=""
      , xlim=c(0,20)
      , ylim=c(-0.5,3.5))
axis (1)
axis (2)
mtext (side=1, line=2, text="President's party expert score")
mtext (side=2, line=2, text="Executive policy mood")
text (xy.coords(add.PresParty.expert.score, President.Moods$q50)
      , labels=paste(President.Moods$ccode, President.Moods$start.Pres, sep="-")
      , cex=0.8)
abline (lm(President.Moods$q50~add.PresParty.expert.score), lwd=1)
cor (cbind (President.Moods$q50, add.PresParty.expert.score))
legend ("topleft", bty="n", legend=expression ( hat(rho)==0.43))

## Find cases to talk about in book
# Average rightist expert score of our top-3 rightists
mean (c(add.PresParty.expert.score[President.Moods$ccode=="CRC" & President.Moods$start.Pres==1998],
add.PresParty.expert.score[President.Moods$ccode=="COL" & President.Moods$start.Pres==1998],
add.PresParty.expert.score[President.Moods$ccode=="BOL" & President.Moods$start.Pres==1997]))

# Average leftist expert score of our top-3 leftists
add.PresParty.expert.score[President.Moods$q50 < 0]
President.Moods$ccode[President.Moods$q50 < 0]
add.PresParty.expert.score[President.Moods$ccode=="VEN" & President.Moods$start.Pres==2001]

cor.test.plus (cor.test (x=President.Moods$q50, y=add.PresParty.expert.score))
cor.test.plus (cor.test (x=Prez.KimFording$q50, y=Prez.KimFording$VRR))

###########################################################################
#### Comparing presidential positions with Arnold, Doyle, Wiesehomeier ####
###########################################################################

# Need to think this carefully: Which ADW score should we use? The first year of a new president seems most obvious

ADWprez <- read.csv (paste0 (dataPath, "PresPositions_2016_Arnold_Doyle_Wiesehomeier.csv"))
ADWprez$id <- paste (ADWprez$country, ADWprez$year, sep="-")
ADWshort <- ADWprez[,is.element(colnames(ADWprez), c("firstpositionbz","id"))]

# Add easy acronym/year
substr(President.Moods$start.Pres, nchar(President.Moods$start.Pres)-2+1, nchar(President.Moods$start.Pres))
plotSymbol <- paste0 (President.Moods$ccode, substr(President.Moods$start.Pres, nchar(President.Moods$start.Pres)-2+1, nchar(President.Moods$start.Pres)))

President.Moods$name[!is.element(President.Moods$name, ADWshort$id)]
plotPresData <- merge (President.Moods, ADWshort, by.x="name", by.y="id", all=TRUE)
plot (plotPresData$firstpositionbz~plotPresData$q50, type="n")
text (xy.coords (plotPresData$q50, plotPresData$firstpositionbz), labels=plotSymbol, cex=0.7)

cor.test.plus(cor.test ( x=plotPresData$firstpositionbz, y=plotPresData$q50))

