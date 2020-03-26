###############################################################################
# 2017dataPlotsCitizens
# GR: December 2017
# This file plots the first few graphs in Chapter 2 (Citizens)
# This file can be run independently of other files for this chapter
###############################################################################

library (car)
library (gtools)
library (foreach)
library (reshape2)
library (plyr)
library (xtable)


graphicsPath <- "Graphs/"  # Save copies here as well
###################
#### Load Data ####
###################

# Load LAPOP and LB data; we have performed multiple imputation routines for
# these data in files "readLBdataMI.R"
load ("Datasets/OriginalDataFiles/LAPOPindividualIssueScalesMI.RData")
load ("Datasets/OriginalDataFiles/LBindividualResponsesMI.RData")

Lapop  <- Datos; rm (Datos)
Latbar <- Aggr.Data.Full; rm (Aggr.Data.Full)

# Rename countries correctly
Latbar$pais <- car::recode (Latbar$pais, "'Costa Rica'='CostaRica';
                       'El Salvador'='ElSalvador'; 'Dominican Republic'='DominicanRep'")
Lapop$pais <- as.factor (Lapop$pais)
Lapop$paisYear <- as.factor (paste (Lapop$pais, Lapop$year, sep="-"))

Latbar$pais <- as.factor (Latbar$pais)
Latbar$paisYear <- as.factor (paste (Latbar$pais, Latbar$year, sep="-"))

# Recoding some questions into a 1-7 scale
questions = c("privatization.beneficial","state.not.reduce.inequality","state.not.in.pensions","state.not.in.sanitation")
for(i in questions){
  Latbar[,i]<-recode(Latbar[,i],"2=3;3=5;4=7")
}

# Merge datasets
Citizen <- smartbind (Lapop, Latbar, verbose=TRUE)
CitRoll <- Citizen[,!is.element(colnames(Citizen)
                                ,c("weight"
                                   ,"pais"
                                   ,"year"
                                   ,"paisYear"))]

# The following are auxiliary functions that we need in order to 
# aggregate individual-level data into the "pro-market percentages"
# required by McGann's dyad ratio algorithm

# Function to calculate percentage of pro-market responses
calculatePercent <- function (bloc=bloc, mdpt=3.5) {
  if (invalid (bloc)==TRUE){ 
    val <- NA 
  } else {
    bloc <- as.vector (recode (bloc, "-2:0=NA"))
    tmp <- summary (as.factor(bloc))
    tmp <- tmp[names(tmp) != "NA's"]
    aye <- tmp[as.numeric(names(tmp))>mdpt]
    nay <- tmp[as.numeric(names(tmp))<mdpt]
    split <- tmp[as.numeric(names(tmp))==mdpt]
    if (length (split)==1) {
      aye <- sum (aye) + split/2
      nay <- sum (nay) + split/2	
    } else {
      aye <- sum (aye)
      nay <- sum (nay)
    }
    val <- aye/(aye+nay)
  }
  return (val)
}

# We actually need to weight responses
calculateWtPercent <- function (bloc, w, mdpt=3.5) {
  if (invalid (bloc)==TRUE){ 
    val <- NA 
  } else {
    bloc <- as.vector (car::recode (bloc, "-2:0=NA"))
    adVal <- sort (unique (bloc))
    tmp <- c()
    for (k in 1:length(adVal)) {
      tmp[k] <- sum (w[bloc==adVal[k] & !is.na(bloc)])
    }
    names (tmp) <- adVal
    aye <- tmp[as.numeric(names(tmp))>mdpt]
    nay <- tmp[as.numeric(names(tmp))<mdpt]
    split <- tmp[as.numeric(names(tmp))==mdpt]
    if (length (split)==1) {
      aye <- sum (aye) + split/2
      nay <- sum (nay) + split/2	
    } else {
      aye <- sum (aye)
      nay <- sum (nay)
    }
    val <- aye/(aye+nay)
  }
  return (val)
}


dataEntries <- sort (as.character (unique (Citizen$paisYear)))
colEntries  <- colnames (CitRoll)

# Auxiliary quantities to use in calculatePercent
maxima <- apply (CitRoll, 2, max, na.rm=T) # largest value of ordered categories
midpnt <- function (x) { tmp <- quantile (1:x, prob=0.5); return (tmp) }
mids <- sapply (maxima, midpnt)  # Find midpoint category

#############################################################################################
# The following lines are commented out because they take a while to run.
# We have collected two objects produced by these lines (PercentData and WtPercentData)
# below, and saved them as percentData.RData). Instead of re-running these lines,
# simply import this object executing line 167.

## Run calculatePercent (don't need to run again, object saved)
# PercentData <- matrix (NA, ncol=length (colEntries), nrow=length(dataEntries))
# system.time (
# for (i in 1:length(dataEntries)){
# 	print (i)
# 	for (j in 1:length(colEntries)){
# 		print (j)
# 		bloc <- CitRoll[Citizen$paisYear==dataEntries[i], which (colnames (CitRoll)==colEntries[j])]
# 		mdpt <- mids[j] 
# 		PercentData[i,j] <- calculatePercent (bloc=bloc, mdpt=mdpt)
# 	}
# }
# )
# colnames (PercentData) <- colnames (CitRoll)


## Run calculateWtPercent (don't need to run again, object saved)
# WtPercentData <- matrix (NA, ncol=length (colEntries), nrow=length(dataEntries))
# system.time (
# for (i in 1:length(dataEntries)){
# 	print (i)
# 	for (j in 1:length(colEntries)){
# 		print (j)
# 		bloc <- CitRoll[Citizen$paisYear==dataEntries[i], which (colnames (CitRoll)==colEntries[j])]
# 		mdpt <- mids[j] 
# 		PercentData[i,j] <- calculateWtPercent (bloc=bloc, mdpt=mdpt)
# 	}
# }
# )
# colnames (PercentData) <- colnames (CitRoll)

#############################################################################################
# Save objects in a dedicated .RData
# save (WtPercentData, PercentData, dataEntries, list=c("WtPercentData","PercentData","dataEntries")
# 	  , file="Datasets/OriginalDataFiles/percentData.RData")

load (file="Datasets/OriginalDataFiles/percentData.RData")
#############################################################################################

## Checks to see that functions returned accurate percentages (these are in unit-interval)
## Check: pick a few values of i,j and corroborate that calculatePercent() is accurate
i=5; j=27
summary (as.factor (CitRoll[Citizen$paisYear==dataEntries[i], which (colnames (CitRoll)==colEntries[j])]))
PercentData[i,j]

## Check: pick a few values of i,j and corroborate that calculatePercent() is accurate
i=5; j=1; mids[j]
tmp <- CitRoll[Citizen$paisYear==dataEntries[i], which (colnames (CitRoll)==colEntries[j])]
w <- Citizen$weight[Citizen$paisYear==dataEntries[i]]
for (k in 1:length (sort (unique(tmp)))){
  print (sum (w[tmp==sort(unique(tmp))[k] & !is.na(tmp)]))
}
PercentData[i,j]
WtPercentData[i,j]

#########################################################
#### Present percent data for one country: Argentina ####
#########################################################
Argentina <- WtPercentData[1:18,]
which.columns <- c("state.not.in.high.edu","state.not.in.telecom","state.not.in.petrol"
                   ,"capital.mobility.good","labor.migration.good","privatization.improved.quality"
                   ,"market.works.well","state.not.solves.problems.2","state.not.in.health"
                   ,"state.not.in.primary.edu","state.not.solves.problems","state.limited.scope.b"
                   ,"encourage.fdi","market.best","state.not.in.price.control","state.limited.scope"
                   ,"state.not.in.water","state.not.in.electricity","state.not.in.sanitation"
                   ,"state.not.in.pensions","state.not.reduce.inequality","state.not.job.creation"
                   ,"economy.best.in.private.sector","privatization.beneficial")
xtable (t(round (Argentina[1:10,which.columns]*100, 2)))
xtable (t(round (Argentina[11:18,which.columns]*100, 2)))

######################################################################
#### Present an overview of data availability across LAPOP and LB ####
######################################################################

years <- 1996:2014
countries <- unique (unlist (strsplit (dataEntries, split="-"))[odd(1:(length(dataEntries)*2))])
countriesNice <- car::recode (countries, "'DominicanRep'='Dominican Republic'; 'CostaRica'='Costa Rica';
                         'ElSalvador'='El Salvador'")

extYear <- function (x) { substr (x, start=nchar(x)-3, stop=nchar(x)) }

# Produce surveyAvailabilityMcGann.pdf
setEPS()
postscript (paste0(graphicsPath,"RevisedGraphs/EPS format/3.1.eps"), height=7, width=10)
par (mar=c(4,9,1,1), las=2, mfrow=c(1,1))
plot (c(1, length(years)), c(1, length(countries)+2)
      , type="n", axes=F, ylab="", xlab="")
axis (1, at=c(1:length(years)), labels=years)
axis (2, at=c(1:length(countries)), labels=countriesNice)
abline (h=1:length(countries), lty=3)
legend(x = "top", bty="n",
       legend = c("LatinBarometer", "AmericasBarometer"), 
       col=c("black","gray"), pch=c(21,21), pt.bg = c("black","gray"), cex=.8, horiz = TRUE)
for (i in 1:length(countries)) {
  cty <- countries[i]
  lb  <- extYear (as.character (unique (Latbar$paisYear)[grep (cty, unique (Latbar$paisYear))]))
  lp  <- extYear (as.character (unique (Lapop$paisYear)[grep (cty, unique (Lapop$paisYear))]))
  points (xy.coords (c(1:length(years))[is.element (years, as.numeric(lb))]-0.15, rep (i, length(lb)))
          , pch=19, cex=1, col="black")
  points (xy.coords (c(1:length(years))[is.element (years, as.numeric(lp))]+0.15, rep (i, length(lp)))
          , pch=21, cex=1, col="black", bg="gray")
}
dev.off()




###############################################################
# We will need to eliminate items we are not interested in, 
# especially questions on regionalism
###############################################################
droppedItems <- c("state.not.solves.problems.3","state.not.in.basic.needs"
                  ,"regional.trade.benefits","latin.american.integration"
                  ,"government.collaborate.ifo","in.favor.ftaa"
                  ,"spanish.capital.good","trade.treaties.good.for.jobs"
                  ,"market.integration.good","regional.integration.concessions"
                  ,"regional.integration.labor.mobility","regional.integration.no.intl.taxes"
                  ,"regional.integration.single.currency","integration.not.in.crisis"
                  ,"regional.integration.possible","regional.integration.central.bank"
                  ,"state.not.in.finance.parties","imports.help"
                  ,"l1","left.right") # Doesn't make sense to include LR here.
colnames (WtPercentData)[!is.element(colnames(WtPercentData), droppedItems)]

vars2get <- c("privatization.beneficial","state.not.reduce.inequality"
              ,"state.not.in.pensions","state.not.in.sanitation"
              ,"state.not.in.unemployment","market.best"
              ,"state.not.in.primary.edu"
              ,"economy.best.in.private.sector","state.not.job.creation"
              ,"state.not.in.health","state.not.in.price.control")
is.element (vars2get, colnames(WtPercentData)) # These are the variables employed in the individual-level basicspace analysis

# "questions" contains the items that we will actually use
questions.Few <- WtPercentData[,is.element(colnames(WtPercentData), vars2get)]
questions.All <- WtPercentData[,!is.element(colnames(WtPercentData), droppedItems)]
# nquest <- ncol (questions.Few) # Toggle here to perform analysis with Few or All stimuli
nquest <- ncol (questions.All) # Toggle here to perform analysis with Few or All stimuli

validDataEntries <- dataEntries[apply (questions.All, 1, invalid)==FALSE]

################################################################
# Present an overview of data availability across LAPOP and LB #
# after dropping data entries that are not valid               #
################################################################

years <- 1996:2014
countries <- unique (unlist (strsplit (validDataEntries, split="-"))[odd(1:(length(validDataEntries)*2))])
# countriesNice <- recode (countries, "'DominicanRep'='Dominican Rep.';
#                          'ElSalvador'='El Salvador'; 'Costa Rica'='CostaRica'")

extYear <- function (x) { substr (x, start=nchar(x)-3, stop=nchar(x)) }

# Produce surveyRealAvailabilityMcGann.pdf
# pdf (paste0(graphicsPath,"surveyRealAvailabilityMcGann.pdf"), h=7, w=10)
par (mar=c(4,9,1,1), las=2, mfrow=c(1,1))
plot (c(1, length(years)), c(1, length(countries)+2)
      , type="n", axes=F, ylab="", xlab="")
axis (1, at=c(1:length(years)), labels=years)
axis (2, at=c(1:length(countries)), labels=countriesNice)
abline (h=1:length(countries), lty=3)
legend(x = "top", bty="n",
       legend = c("LatinBarometer", "AmericasBarometer"), 
       col=c("black","gray"), lwd=5, cex=.8, horiz = TRUE)
for (i in 1:length(countries)) {
  cty <- countries[i]
  valid <- extYear (as.character (unique (validDataEntries)[grep (cty, unique (validDataEntries))]))
  lb  <- extYear (as.character (unique (Latbar$paisYear)[grep (cty, unique (Latbar$paisYear))]))
  lp  <- extYear (as.character (unique (Lapop$paisYear)[grep (cty, unique (Lapop$paisYear))]))
  points (xy.coords (c(1:length(years))[is.element (years, as.numeric(lb))]-0.15, rep (i, length(lb)))
          , pch=19, cex=1, col="black")
  points (xy.coords (c(1:length(years))[is.element (years, as.numeric(lp))]+0.15, rep (i, length(lp)))
          , pch=21, cex=1, col="black", bg="gray")
  points (xy.coords (c(1:length(years))[is.element (years, valid)], rep (i, length(valid)))
          , cex=1, pch=19, col="black")
}
# dev.off()


###################################################################
#### Prepare data for input into McGann's dyad-ratio algorithm ####
###################################################################

#Stack the columns in "questions" into a single column vector
#  Q.data <- melt(questions.Few)  # Toggle here
Q.data <- melt(questions.All)  # Toggle here

# Add countryyear and question.number identifiers, then omit rows with missing data
Q.data$cy <- rep (dataEntries, nquest)
Q.data$q  <- as.numeric (Q.data$Var2)
jagsData  <- na.omit (Q.data)
jagsData  <- jagsData[,-grep("Var1",colnames(jagsData))]
jagsData  <- jagsData[order(jagsData$cy,jagsData$value),]
jagsData$row.index <- 1:nrow(jagsData)
jagsData$country <- unlist (strsplit (jagsData$cy, split="-"))[odd(1:(nrow(jagsData)*2))]
jagsData$year <- as.numeric (unlist (strsplit (jagsData$cy, split="-"))[even(1:(nrow(jagsData)*2))])

########################
# Graph available data #
########################
# Produce availDataMcGann[country].pdf graphs
avItemXyear <- list()
for (i in 1:length (unique (jagsData$country))) {
  ps <- unique (jagsData$country)[i]
  tempData <- jagsData[jagsData$country==ps,]
  badIssues <- which (table(tempData$q)==1)
  tempData <- tempData[!is.element(tempData$q,as.numeric (names (badIssues))),]
  tempData$Var2 <- factor (tempData$Var2)
  tmpLabel <- levels (tempData$Var2)
  tmp <- tempData$q[tempData$country==ps]
  tempData$q.alternative <- mapvalues(tempData$q, from = sort(unique(tempData$q)), to = order (sort(unique(tempData$q)))) # items sorted in order already, but numbers are not consecutive; this function makes them consecutive to work with jags
  
  avItemXyear[[i]] <- table (tempData$year)
  # pdf (paste(graphicsPath,"availDataMcGann", ps, ".pdf", sep=""), h=9, w=5)	# Uncomment to produce graphs again
  par (mar=c(4,12,2,1), las=2, cex.axis=0.8)
  plot (c(1,19), c(1,length(tmpLabel)), type="n", axes=F, ylab="", xlab="")
  axis (1, at=1:19, labels = 1996:2014)
  axis (2, at=1:length(tmpLabel), labels = tmpLabel, cex=0.9)
  par (las=0); mtext (side=3, line=1, at=9.5, countriesNice[i])
  abline (v=1:19, lty=3)
  abline (h=1:length(tmpLabel), lty=3)
  points (xy.coords(tempData$year-1995, tempData$q.alternative), pch=19)
  # dev.off()	# Uncomment to produce graphs again
}

DR <- avItemXyear[[7]]; DF <- c(rep (0,6), DR)
avItemXyear[[7]] <- NULL
df <- data.frame(matrix(unlist(avItemXyear), nrow=17, byrow=T))
colnames (df) <- names (avItemXyear[[1]])
df <- rbind (df, DF)
itemsPerYear <- colMeans (df)

# save (itemsPerYear, file="Datasets/OriginalDataFiles/itemsPerYear.RData")

avItemXcountry <- list()
for (i in 1:length (unique (jagsData$country))) {
  ps <- unique (jagsData$country)[i]
  tempData <- jagsData[jagsData$country==ps,]
  badIssues <- which (table(tempData$q)==1)
  tempData <- tempData[!is.element(tempData$q,as.numeric (names (badIssues))),]
  tempData$Var2 <- factor (tempData$Var2)
  tmpLabel <- levels (tempData$Var2)
  tmp <- tempData$q[tempData$country==ps]
  tempData$q.alternative <- mapvalues(tempData$q, from = sort(unique(tempData$q)), to = order (sort(unique(tempData$q)))) # items sorted in order already, but numbers are not consecutive; this function makes them consecutive to work with jags
  
  avItemXcountry[[i]] <- table (tempData$Var2)
}

DR <- avItemXcountry[[7]]
DR <- c(rep (0,4), DR)
names (DR)[1:4] <- names(avItemXcountry[[1]])[!is.element (names (avItemXcountry[[1]]), names (avItemXcountry[[7]]))]
avItemXcountry[[7]] <- NULL
df <- data.frame(matrix(unlist(avItemXcountry), nrow=17, byrow=T))
colnames (df) <- names (avItemXcountry[[1]])
DF <- rbind (DR[colnames(df)], df)
itemsPerCountry <- colMeans (df)

# save (itemsPerCountry, file="Datasets/OriginalDataFiles/itemsPerCountry.RData")

############################################################################
############################################################################


cy.index.wnames  <- ddply(jagsData, .(cy),.fun=function(x)c(x$row.index[1],x$row.index[nrow(x)])) 
cy <- unlist (strsplit (cy.index.wnames$cy, split="-"))[odd(1:(nrow(cy.index.wnames)*2))]
N  <- nrow(cy.index.wnames)
yr <- as.numeric (unlist (strsplit (cy.index.wnames$cy, split="-"))[even(1:(nrow(cy.index.wnames)*2))])

# save (jagsData, file="Datasets/OriginalDataFiles/itemInfo.RData")
