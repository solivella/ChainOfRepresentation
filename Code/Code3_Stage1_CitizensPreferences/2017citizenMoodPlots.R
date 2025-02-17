##############################################
## Pooled McGann
## GR, December 2017
## Plots a few more graphs based on moods
## from STAN blender run
## Should be run before 2017moodPredictors.R
##############################################

library (gtools)
library (ggrepel)
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
library (tidyverse)

graphicsPath <- "Graphs/"  # Save copies here as well


load("Code/Code5_Blender/sharedItemParameterData.RData")
load (file="Code/Code5_Blender/JointIRT_Flexible.RData")
load (file="Datasets/OriginalDataFiles/itemsPerYear.RData")

## Prepare data
finalData$ActorCtyYr <- as.integer(as.factor(with(finalData,
                                                  pæaste(type, country, year))))
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

# Correlation function to be used below
cor.test.plus <- function(x) {
   list(x,
        Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

# Get type, country, year
actor <- with (finalData, tapply (type, ActorCtyYr, unique))
country <- with (finalData, tapply (country, ActorCtyYr, unique))
year <- with (finalData, tapply (year, ActorCtyYr, unique))

actor.type <- actor[actor != "Government"]
country <- country[actor != "Government"]
year    <- year[actor != "Government"]

# Load MCMC object
chainsConv <- rstan::As.mcmc.list(common_model)

# Run Gelman-Rubin R-hat without multivariate option or wrapper will choke
# gelman.diag  (chainsConv, multivariate=FALSE) # looks good

# Summaries of posterior distribution
Mu <- rstan::extract(common_model, "mu_a")[[1]]
#
Sigma <-  rstan::extract(common_model, "sigma")[[1]]

#########################################################
#### Bluntly display all McGann citizen policy moods ####
#########################################################

Mu <- Mu[,actor.type=="Citizen"]
pais <- country[actor.type=="Citizen"]
anyo <- year[actor.type=="Citizen"]

CitizenMoods <- c()
for (i in 1:ncol(Mu)){
  temp <- quantile (Mu[,pais==pais[i] & anyo==anyo[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  CitizenMoods <- rbind (CitizenMoods, temp)
}
CitizenMoods <- as.data.frame (CitizenMoods)
colnames (CitizenMoods) <- c("q10","q25","q50","q75","q90")
CitizenMoods$real.year <- anyo
CitizenMoods$pais <- pais
rownames (CitizenMoods) <- 1:nrow(CitizenMoods)


Sigma <- Sigma[,actor.type=="Citizen"]
CitizenPolar <- c()
for (i in 1:ncol(Sigma)){
  temp <- quantile (Sigma[,pais==pais[i] & anyo==anyo[i]]
                    , prob=c(0.1,0.25,0.5,0.75,0.9))
  CitizenPolar <- rbind (CitizenPolar, temp)
}
CitizenPolar <- as.data.frame (CitizenPolar)
colnames (CitizenPolar) <- c("q10","q25","q50","q75","q90")
CitizenPolar$real.year <- anyo
CitizenPolar$pais <- pais
rownames (CitizenPolar) <- 1:nrow(CitizenPolar)

# ccode for labeling
CitizenPolar$ccode <- car::recode (CitizenPolar$pais, "'Argentina'='ARG';
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

CitizenMoods$ccode <- car::recode (CitizenMoods$pais, "'Argentina'='ARG';
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



## Adding LOESS smoothed regional trend and confidence interval for Polarization
geom.text.size = 2
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "polarMcGannAllCountriesNEW.pdf", sep=""), h=5, w=9)
ggplot(CitizenPolar, aes(real.year, q50)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
   ) +
   geom_text_repel(aes(label=ccode)
                   # , box.padding=unit(0.45, "lines")
                   , size=geom.text.size) +
   geom_point(size=3, colour="gray") +
   geom_smooth(colour="black") +
   scale_x_continuous(name="", breaks=1996:2014
                      , labels=as.character(1996:2014)) +
   scale_y_continuous(name="Lower <--- Heterogeneity ---> Higher"
                      , breaks=c(0,1,2,3,4,5)
                      , labels=c(0,1,2,3,4,5))
dev.off()
########################################################





## Adding LOESS smoothed regional trend confidence interval
geom.text.size = 2
theme.size = (14/5) * geom.text.size
pdf (paste (graphicsPath, "moodMcGannAllCountriesNEW.pdf", sep=""), h=5, w=9)
ggplot(CitizenMoods, aes(real.year, q50)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 0)
   ) +
   geom_text_repel(aes(label=ccode)
                   # , box.padding=unit(0.45, "lines")
                   , size=geom.text.size) +
   annotate(geom="text", x=c(1996,1998,2000:2014),
            y=rep(-1,17), label=round (itemsPerYear,1)) +
   geom_point(size=3, colour="gray") +
   geom_smooth(colour="black") +
   scale_x_continuous(name="", breaks=1996:2014
                      , labels=as.character(1996:2014)) +
   scale_y_continuous(name="Pro State <--- Mood ---> Pro Market"
                      , breaks=c(0,1,2,3,4,5)
                      , labels=c(0,1,2,3,4,5))
dev.off()
########################################################



# Correlation between estimate of Latin American mood and average available items
av.Lat.Mood <- by (CitizenMoods$q50, CitizenMoods$real.year, mean)
round (cor (cbind (av.Lat.Mood, itemsPerYear)), 2) # Report this correlation
cor.test.plus(cor.test (x=av.Lat.Mood, y=itemsPerYear)) # Here's the std error for the correlation coefficient
cor.test.plus(cor.test (x=abs(av.Lat.Mood), y=itemsPerYear)) # Here's the std error for the correlation coefficient

# Calculate within-year variance and between-year variance
fit <- lm (q50 ~ as.factor(real.year), data=CitizenMoods)
anova (fit)
anova(fit)["Residuals", "Mean Sq"] # within-year variance
anova(fit)["as.factor(real.year)", "Mean Sq"] # between-year variance
anova(fit)["as.factor(real.year)", "Mean Sq"]/anova(fit)["Residuals", "Mean Sq"] # between-to-within variance

# Very strongly patterned time series
# Across years, the series veers more drastically than within years
# Within-year variance is 5% of cross-year variance
w.year.var <- as.vector (unlist (with (CitizenMoods, by (q50, real.year, var, na.rm=T))))
cor.test.plus(cor.test (x=w.year.var, y=itemsPerYear)) # Here's the std error for the correlation coefficient


###############################################
#### Country-specific citizen policy moods ####
###############################################
pais.nice <- unique (pais)
pais.nice <- car::recode (pais.nice, "'CostaRica'='Costa Rica';
                     'DominicanRep'='Dominican Republic';
                     'ElSalvador'='El Salvador'")

for (i in 1:18) {
   pdf (paste0(graphicsPath,"CitizenMoods", unique(pais)[i], ".pdf"), h=5, w=9)
   par (mar=c(4,4,1,1), las=3)
   plot (c(1996, 2014), c(-3, 3)
      , type="n", ylab="", xlab=""
      , axes=F, ylim=c(-3,3), xlim=c(1996,2014))
   axis (2)
   axis (1, at=c(1996:2014), labels=1996:2014)
   mtext (side=2, line=2, text="Pro State <--- Mood ---> Pro Market")
   dat <- CitizenMoods[CitizenMoods$pais==unique(pais)[i],]
   points (xy.coords (dat$real.year, dat$q50), col="black", pch=19)
   segments (y0=dat$q25
             , y1=dat$q75
             , x0=dat$real.year, x1=dat$real.year, lwd=3)
   segments (y0=dat$q10
             , y1=dat$q90
             , x0=dat$real.year, x1=dat$real.year, lwd=1)
   legend ("bottomright", legend=pais.nice[i], bty="n", cex=1.2)
   dev.off()
}


####################################################
#### Add Left-Right position (GR: Feb 24, 2016) ####
####################################################
load (file="Datasets/OriginalDataFiles/percentData.RData")

LR <- data.frame (id=dataEntries
                  , leftRight=WtPercentData[,is.element(colnames(WtPercentData), "left.right")])

###################################################################################
#### Import data from Baker-Greene, to compare with McGann policy mood measure ####
###################################################################################

# "presidentialVRLusingBakerGreeneDofile.txt"
# and with "legislativeVRLusingBakerGreeneDofile.txt"
# include VRL and VRR scores built using Baker and Greene's stata code,
# which is in bakerGreeneCode.do

BG.Leg.Elec <- read.table ("DataSets/OriginalDataFiles/BakerGreene_WorldPolitics/legislativeVRLusingBakerGreeneDofile.txt", header=T, sep="\t", quote = "", fileEncoding = "UTF-16")
BG.Pres.Elec <- read.table ("DataSets/OriginalDataFiles/BakerGreene_WorldPolitics/presidentialVRLusingBakerGreeneDofile.txt", header=T, sep="\t", quote = "", fileEncoding = "UTF-8")

BG.Leg.Elec$id <- as.factor (paste (BG.Leg.Elec$country, BG.Leg.Elec$year, sep="-"))
BG.Pres.Elec$id <- as.factor (paste (BG.Pres.Elec$country, BG.Pres.Elec$year, sep="-"))

BG.L <- data.frame (id=BG.Leg.Elec$id, legVRR=as.numeric (BG.Leg.Elec$VRR))
BG.E <- data.frame (id=BG.Pres.Elec$id, presVRR=as.numeric (BG.Pres.Elec$VRR))

BG.All <- merge (BG.L, BG.E, by="id", all=TRUE)
cor (BG.All[,2:3], use="p")
plot (BG.All[,2:3], pch=19, xlab="Baker-Greene Leg Score", ylab="Baker-Greene Pres Score")

# The scales are identical: VRR based on B-G do files correspond exactly with my reconstruction
# We use theirs

# What follows is my own reconstruction of their scores
Leg.Elec  <- read.table ("DataSets/OriginalDataFiles/BakerGreene_WorldPolitics/legislativeElections.txt", header=T, sep="\t", quote = "", fileEncoding = "UTF-16")
Pres.Elec <- read.table ("DataSets/OriginalDataFiles/BakerGreene_WorldPolitics/presidentElections.txt", header=T, sep="\t", quote = "", fileEncoding = "UTF-16")

Leg.Elec$count.vote <- ifelse ( is.na(Leg.Elec$Ideology), NA, Leg.Elec$Votes.Pct)
Pres.Elec$count.vote <- ifelse ( is.na(Pres.Elec$Ideology), NA, Pres.Elec$Votes.Pct)
Leg.Elec$id <- as.factor (paste (Leg.Elec$Country, Leg.Elec$Year, sep="-"))
Pres.Elec$id <- as.factor (paste (Pres.Elec$Country, Pres.Elec$Year, sep="-"))
Leg.Elec$uncorrect.w.score <- Leg.Elec$Votes.Pct * Leg.Elec$Ideology
Pres.Elec$uncorrect.w.score <- Pres.Elec$Votes.Pct * Pres.Elec$Ideology

Leg.Elec <- with(Leg.Elec, Leg.Elec[order(id),])
Pres.Elec <- with(Pres.Elec, Pres.Elec[order(id),])

correctSum <- function (x) {
   tmp <- x / sum(x, na.rm=T)
   return (tmp)
}

Leg.Elec$correct.vote <- unlist (tapply (Leg.Elec$count.vote, Leg.Elec$id, correctSum))
Pres.Elec$correct.vote <- unlist (tapply (Pres.Elec$count.vote, Pres.Elec$id, correctSum))

Leg.Elec$correct.w.score <- Leg.Elec$correct.vote * Leg.Elec$Ideology
Pres.Elec$correct.w.score <- Pres.Elec$correct.vote * Pres.Elec$Ideology

BakerGreeneLegScore <- tapply (Leg.Elec$correct.w.score, Leg.Elec$id, sum, na.rm=T)
BakerGreenePresScore <- tapply (Pres.Elec$correct.w.score, Pres.Elec$id, sum, na.rm=T)

BGLeg <- data.frame (id=names (BakerGreeneLegScore), legScore=as.numeric (BakerGreeneLegScore))
BGPres <- data.frame (id=names (BakerGreenePresScore), presScore=as.numeric (BakerGreenePresScore))
rm (BakerGreeneLegScore, BakerGreenePresScore)

BG <- merge (BGLeg, BGPres, by="id", all=TRUE)
cor (BG[,2:3], use="p")
plot (BG[,2:3], pch=19, xlab="Baker-Greene Leg Score", ylab="Baker-Greene Pres Score")

BG.All$final.score <- rowMeans (BG.All[,2:3], na.rm=TRUE)
# BG$final.score contains our reconstruction of Baker-Greene's
# "vote-revealed leftism" (based on Kim and Fording):
# 1. I dropped  parties entered as "other", as their "ideology" is not known
# 2. I reweighted vote shares for identified parties, so that the sum adds up to 1
# 3. For all identified parties, I multiplied vote share times ideology score
# 4. I added all vote-weighted ideology score for the same election
# 5. I did all of this for legislative and presidential elections
# 6. If there are simultaneous presidential/legislative elections the same year,
#    final.score is an average of the two (vote-revealed leftisms from legislative
#    and presidential elections are very highly correlated.)
# 7. Voter-revealed leftism does not correspond to our State-Market mood; later
#    on, we need to flip this measure into a rescaled.BG.score
tmp <- unlist (strsplit (as.character (BG.All$id), split="-"))
BG.All$country <- tmp[odd(1:length(tmp))]
BG.All$year    <- tmp[even(1:length(tmp))]

simpleCap <- function(x) {
   s <- strsplit(x, " ")[[1]]
   paste(toupper(substring(s, 1,1)), substring(s, 2),
         sep="", collapse=" ")
}

BG.All$country <- sapply(tolower(BG.All$country), simpleCap)
BG.All$country <- car::recode (BG.All$country, "'Costarica'='CostaRica'
                          ; 'Elsalvador'='ElSalvador'
                          ; 'Dominicanrep'='DominicanRep'")
BG.All$uniqueID <- paste (BG.All$country, BG.All$year, sep="-")
BG.All <- BG.All[,!is.element(colnames(BG.All),c("id","country","year"))]
rm (Leg.Elec, Pres.Elec, BG)


####################################################
#### Add economic information (GR May 29, 2015) ####
####################################################
gdp <- read.table ("DataSets/OriginalDataFiles/WorldDevelopmentIndicators/gdpGrowth.txt", header=T)
unemp <- read.table ("DataSets/OriginalDataFiles/WorldDevelopmentIndicators/unemployment.txt", header=T)
credit <- read.table ("DataSets/OriginalDataFiles/WorldDevelopmentIndicators/domesticCredit.txt", header=T)
# For inflation, we use data from the IADB, except for ElSalvador, which still uses data from WDI (data are almost exactly the same for all common countries, but WDI lacks Argentina)
infl <- read.table ("DataSets/OriginalDataFiles/IADBdata/inflationIADB.csv", sep=",", header=T)

###################################################################
# Find within- and between-year covariances for economic outcomes #
###################################################################
GDP <- melt (gdp)
GDP$year <- rep (1995:2014, length (unique (GDP$variable)))
GDP <- GDP[GDP$variable!="Average",]
fit <- lm (value ~ as.factor(year), data=GDP)
anova(fit)["Residuals", "Mean Sq"] # within-year variance
anova(fit)["as.factor(year)", "Mean Sq"] # between-year variance
anova(fit)["as.factor(year)", "Mean Sq"]/anova(fit)["Residuals", "Mean Sq"] # between-to-within ratio

UNEMP <- melt (unemp)
UNEMP$year <- rep (1995:2014, length (unique (UNEMP$variable)))
UNEMP <- UNEMP[UNEMP$variable!="Average",]
fit <- lm (value ~ as.factor(year), data=UNEMP)
anova(fit)["Residuals", "Mean Sq"] # within-year variance
anova(fit)["as.factor(year)", "Mean Sq"] # between-year variance
anova(fit)["as.factor(year)", "Mean Sq"]/anova(fit)["Residuals", "Mean Sq"] # between-to-within ratio

INFL <- melt (infl)
INFL$year <- rep (1995:2014, length (unique (INFL$variable)))
INFL <- INFL[INFL$variable!="Average" & INFL$variable!="X",]
fit <- lm (value ~ as.factor(year), data=INFL)
anova(fit)["Residuals", "Mean Sq"] # within-year variance
anova(fit)["as.factor(year)", "Mean Sq"] # between-year variance
anova(fit)["as.factor(year)", "Mean Sq"]/anova(fit)["Residuals", "Mean Sq"] # between-to-within ratio


stimson <- read.table ("DataSets/OriginalDataFiles/StimsonMoods/StimsonMoodsReduced.csv", sep=",", header=TRUE)
stimson$flip.stimson <- stimson$Mood*(-1) + 2*min(stimson$Mood)
stimson$mcgannMood.med <- c(NA,NA,av.Lat.Mood[1],NA,av.Lat.Mood[2],NA,av.Lat.Mood[-c(1:2)])
stimson$mcgannMood.lo <- c(NA,NA,av.Lat.Mood[1]-1.96*sqrt(w.year.var[1])
                            ,NA,av.Lat.Mood[2]-1.96*sqrt(w.year.var[2])
                            ,NA,av.Lat.Mood[-c(1:2)]-1.96*sqrt(w.year.var[-c(1:2)]))
stimson$mcgannMood.hi <- c(NA,NA,av.Lat.Mood[1]+1.96*sqrt(w.year.var[1])
                            ,NA,av.Lat.Mood[2]+1.96*sqrt(w.year.var[2])
                            ,NA,av.Lat.Mood[-c(1:2)]+1.96*sqrt(w.year.var[-c(1:2)]))

# Correlate stimson and median McGann
cor.test.plus (cor.test (stimson$flip.stimson, stimson$mcgannMood.med))

par (mfrow=c(1,1))
pdf (paste0(graphicsPath,"StimsonVmcGannNEW.pdf"), h=5, w=7)
par (mar=c(4,3,1,3), las=3)
plot (stimson$mcgannMood.med~stimson$year, type="b", lwd=3, pch=19
      , ylab=""
      , xlab=""
      , axes=F, ylim=c(-1.5,2), xlim=c(1995,2014))
axis (1, at=1995:2014, labels=NA)
axis (2, labels=NA, at=c(-1.5,2))
mtext (side=1, line=1.5, at=1995:2014, text=1995:2014)
mtext (side=2, line=1, text="Pro State <--- Mood ---> Pro Market")

par(new=TRUE)
plot (stimson$flip.stimson, type="b", lwd=1, pch=19
      , col="grey"
      , xaxt="n",yaxt="n",xlab="",ylab="")
mtext("Conservative (flipped) policy mood in the US", side=4,line=1)
legend("topright",col=c("black","grey"), lty=1, lwd=3, bty="n"
       ,legend=c("Average Latin American policy mood","US policy mood (Stimson)"))
dev.off ()


pdf (paste0(graphicsPath,"avgMcgannMoodGrowthNEW.pdf"), h=5, w=7)
par (mar=c(4,3,1,4), las=3)
plot (stimson$mcgannMood.med~stimson$year, type="b", lwd=3, pch=19
      , ylab=""
      , xlab=""
      , axes=F, ylim=c(-1.5,1.5), xlim=c(1995,2014))
axis (1, at=1995:2014, labels=NA)
axis (2, labels=NA, at=c(-1.5,1.5))
mtext (side=1, line=1.5, at=1995:2014, text=1995:2014)
mtext (side=2, line=1, text="Pro State <--- Mood ---> Pro Market")

par(new=TRUE)
with (gdp, plot (Average, type="b", lwd=1, pch=19, cex=0.8
                 , col="grey"
                 , xaxt="n", yaxt="n", xlab="", ylab=""))
axis(4)
mtext("Average Latin American GDP growth", side=4,line=2)
legend("bottomleft",col=c("black","grey"), lty=1, lwd=3, bty="n"
       ,legend=c("Policy mood","GDP growth"))
dev.off ()


pdf (paste0(graphicsPath,"avgMcgannMoodInflationNEW.pdf"), h=5, w=7)
par (mar=c(4,3,1,4), las=3)
plot (stimson$mcgannMood.med~stimson$year, type="b", lwd=3, pch=19
      , ylab=""
      , xlab=""
      , axes=F, ylim=c(-1.5,1.5), xlim=c(1995,2014))
axis (1, at=1995:2014, labels=NA)
axis (2, labels=NA, at=c(-1.5,1.5))
mtext (side=1, line=1.5, at=1995:2014, text=1995:2014)
mtext (side=2, line=1, text="Pro State <--- Mood ---> Pro Market")

par(new=TRUE)
with (infl, plot (Average, type="b", lwd=1, pch=19, cex=0.8
                  , col="grey"
                  ,xaxt="n",yaxt="n",xlab="",ylab=""))
axis(side=4, at = pretty(range(infl$Average)))
mtext("Average Latin American inflation", side=4,line=2)
legend("bottomleft",col=c("black","grey"), lty=1, lwd=3, bty="n"
       ,legend=c("Policy mood","Inflation"))
dev.off ()


pdf (paste0(graphicsPath,"avgMcgannMoodUnemploymentNEW.pdf"), h=5, w=7)
par (mar=c(4,3,1,4), las=3)
plot (stimson$mcgannMood.med~stimson$year, type="b", lwd=3, pch=19
      , ylab=""
      , xlab=""
      , axes=F, ylim=c(-1.5,1.5), xlim=c(1995,2014))
axis (1, at=1995:2014, labels=NA)
axis (2, labels=NA, at=c(-1.5,1.5))
mtext (side=1, line=1.5, at=1995:2014, text=1995:2014)
mtext (side=2, line=1, text="Pro State <--- Policy Mood ---> Pro Market")

par(new=TRUE)
with (unemp, plot (Average, type="b", lwd=1, pch=19, cex=0.8
                   , col="grey"
                   ,xaxt="n",yaxt="n",xlab="",ylab=""))
axis(side=4, at = pretty(range(unemp$Average, na.rm=T)))
mtext("Average Latin American unemployment", side=4,line=2)
legend("bottomleft",col=c("black","grey"), lty=1, lwd=3, bty="n"
       ,legend=c("Policy mood","Unemployment"))
dev.off ()


pdf (paste0(graphicsPath, "avgMcgannMoodCreditNEW.pdf"), h=5, w=7)
par (mar=c(4,3,1,4), las=3)
plot (stimson$mcgannMood.med~stimson$year, type="b", lwd=3, pch=19
      , ylab=""
      , xlab=""
      , axes=F, ylim=c(-1.5,1.5), xlim=c(1995,2014))
axis (1, at=1995:2014, labels=NA)
axis (2, labels=NA, at=c(-1.5,1.5))
mtext (side=1, line=1.5, at=1995:2014, text=1995:2014)
mtext (side=2, line=1, text="Pro State <--- Mood ---> Pro Market")

par(new=TRUE)
with (credit, plot (Average, type="b", lwd=1, pch=19, cex=0.8
                    , col="grey"
                    ,xaxt="n",yaxt="n",xlab="",ylab=""))
axis(side=4, at = pretty(range(credit$Average, na.rm=T)))
mtext("Average Latin American bank credit to private sector", side=4,line=2)
legend("topleft",col=c("black","grey"), lty=1, lwd=3, bty="n"
       ,legend=c("Policy mood","Credit to private sector"))
dev.off ()





#############################################
#### Economic correlates of policy moods ####
#############################################
#####################################################################
# We build a dataset where we will have policy mood as an outcome,
# then three predictors: GDP growth, misery index, bank credit,
# then McGann mood estimates,
# all of them observed at the country-year level
#####################################################################

policyMoods <- CitizenMoods
policyMoods$id <- paste (policyMoods$pais, policyMoods$real.year, sep="-")



# Build a frame where we will dump country-year policy moods,
# aggregate.LR, inflation, unemployment, growth, and credit
countries <- unique(pais)
years <- 1995:2014
Dataset <- expand.grid (x=countries, y=years)
colnames (Dataset) <- c("pais","year")
Dataset <- Dataset[with(Dataset, order(pais)), ]
Dataset$uniqueID <- paste(Dataset$pais, Dataset$year, sep="-")


Dataset <- merge (Dataset, policyMoods, by.x="uniqueID", by.y="id", all.x=TRUE)
Dataset <- merge (Dataset, LR, by.x="uniqueID", by.y="id", all.x=TRUE)


# Interpolate missing moods for 1995, 1997, 1999
# and leftRight for 1995, 1999, 2012, and 2014
# using cubic spline interpolation function in "zoo" library
# These are still McGann moods
colnames (Dataset)[grep("q50", colnames(Dataset))] <- "Mood.md"
colnames (Dataset)[grep("q25", colnames(Dataset))] <- "Mood.lo"
colnames (Dataset)[grep("q75", colnames(Dataset))] <- "Mood.hi"

Dataset <- Dataset[,-grep("pais.y", colnames(Dataset))]
colnames(Dataset)[grep("pais.x", colnames(Dataset))] <- "pais"

Dataset$interpol.policy.mood <- unlist (tapply (Dataset$Mood.md, Dataset$pais, na.spline, maxgap=2, na.rm=FALSE))
Dataset$interpol.lo.policy.mood <- unlist (tapply (Dataset$Mood.lo, Dataset$pais, na.spline, maxgap=2, na.rm=FALSE))
Dataset$interpol.hi.policy.mood <- unlist (tapply (Dataset$Mood.hi, Dataset$pais, na.spline, maxgap=2, na.rm=FALSE))
Dataset$interpol.left.right <- unlist (tapply (Dataset$leftRight, Dataset$pais, na.spline, maxgap=2, na.rm=FALSE))



###############################################################################
# Build relevant datasets, inefficiently
###############################################################################
Credit <- melt (credit[,-grep("Average", colnames(credit))], value.name="credit")
Growth <- melt (gdp[,-grep("Average", colnames(gdp))], value.name="growth")
Inflation <- melt (infl[,-c(1,grep("Average", colnames(infl)))], value.name="inflation")
Unemploy <- melt (unemp[,-grep("Average", colnames(unemp))], value.name="unemploy")

colnames(Credit)[1] <- "country"
colnames(Growth)[1] <- "country"
colnames(Inflation)[1] <- "country"
colnames(Unemploy)[1] <- "country"

Credit$year <- rep(1995:2014, length (unique(Credit$country)))
Growth$year <- rep(1995:2014, length (unique(Growth$country)))
Inflation$year <- rep(1995:2014, length (unique(Inflation$country)))
Unemploy$year <- rep(1995:2014, length (unique(Unemploy$country)))

Credit$uniqueID <- paste (Credit$country, Credit$year, sep="-")
Growth$uniqueID <- paste (Growth$country, Growth$year, sep="-")
Inflation$uniqueID <- paste (Inflation$country, Inflation$year, sep="-")
Unemploy$uniqueID <- paste (Unemploy$country, Unemploy$year, sep="-")

Credit <- Credit[,!is.element(colnames(Credit), c("country","year"))]
Growth <- Growth[,!is.element(colnames(Growth), c("country","year"))]
Inflation <- Inflation[,!is.element(colnames(Inflation), c("country","year"))]
Unemploy <- Unemploy[,!is.element(colnames(Unemploy), c("country","year"))]
###############################################################################

BG.small <- BG.All[, is.element(colnames(BG.All), c("final.score","uniqueID","legVRR","presVRR"))]
MacroData <- Reduce(function(...) merge(..., by="uniqueID", all=TRUE), list(Dataset, Credit, Growth, Inflation, Unemploy))
MacroData$misery <- MacroData$inflation + MacroData$unemploy
MacroData <- merge (MacroData, BG.small, by="uniqueID", all.x=TRUE)



################################################
#### Plot Baker-Greene against our measure. ####
################################################
# Rescale: Baker-Greene goes, theoretically, from 0 to 20 (most leftist)
# In practice, the lowest score is 4.5, the highest score is 15.7
# We need it from -2 to 2 (most rightist), so 4 --> 2 and 16 --> -2
MacroData$rescaled.BG.score <- ((2-16/3) + MacroData$final.score/3) * (-1)
MacroData$rescaled.legScore <- ((2-16/3) + MacroData$legVRR/3) * (-1)
MacroData$rescaled.presScore <- ((2-16/3) + MacroData$presVRR/3) * (-1)


#
#### Load Blackbox (Stimson) policy moods
load ("Datasets/OriginalDataFiles/bbCitMoods.RData")
colnames (cit.Moods)[grep("year", colnames(cit.Moods))] <- "anyo"
cit.Moods$uniqueID <- paste (cit.Moods$.id, cit.Moods$anyo, sep="-")

MacroData <- merge (MacroData, cit.Moods, by="uniqueID", all.x=TRUE)


# Country-specific plots (point estimates only)
for (i in 1:length(unique(MacroData$pais))) {
   pais <- unique(MacroData$pais)[i]
   tmp <- MacroData[MacroData$pais==pais,]
   lr  <- (tmp$leftRight - mean (tmp$leftRight, na.rm=T)) / sd (tmp$leftRight, na.rm=T)
   mgm <- tmp$Mood.md 
   imm <- tmp$interpol.policy.mood 
   imm[1] <- NA
   stimson <- tmp$median
   st.md <- (stimson - mean(stimson, na.rm=T)) / sd(stimson, na.rm=T)
   pdf ( paste(graphicsPath,"MgGannMood3MethodsPointEstimatesNEW", pais,".pdf", sep=""), h=7, w=14)
   par (srt=0, las=2, mar=c(4,3,1,1), las=2)
   plot (c(1,length(mgm)),c(-3,3), type="n", main="", ylab="", xlab="", axes=F )
   axis (2)
   axis (1, at=c(1:length(tmp$year)), labels=tmp$year)
   par (las=0)
   mtext (side=2, line=2, text="Pro State <--- Mood ---> Pro Market")
   legend ("bottomright", legend=pais.nice[i], bty="n", cex=1.4)
   points ( xy.coords ( c(1:length(tmp$year)), imm ), type="b", lwd=3, pch=19, col="grey", bg="grey", cex=1.6)
   points ( xy.coords ( c(1:length(tmp$year)), mgm ), type="b", pch=19, col="black", cex=1.6)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.legScore), type="p", pch=19, col="red", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.presScore), type="p", pch=21, bg="pink", col="red", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), lr), type="b", pch=21, col="blue", bg="blue", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)+0.1), st.md), type="b", pch=19, col="green", cex=1)
   legend ("topleft"
           , ncol=2
           , border=c("black","grey","green","blue","red","red")
           , fill=c("black","grey","green","blue","red","pink")
           , cex=0.8,
           , legend=c("Policy mood"
                      , "Policy mood (interpolated)"
                      ,"Blackbox score"
                      , "Left-right self-placement"
                      , "Voter-revealed rightism (Leg)"
                      , "Voter-revealed rightism (Pres)")
           , bty="n")
   dev.off ()
}




# Country-specific plots (point estimates only, black and white)
grisPrez <- rgb(80/255,80/255,80/255, alpha=0.8)
grisLeg  <- rgb(128/255,128/255,128/255, alpha=0.8)
grisSen  <- rgb(211/255,211/255,211/255, alpha=0.8)
for (i in 1:length(unique(MacroData$pais))) {
   pais <- unique(MacroData$pais)[i]
   tmp <- MacroData[MacroData$pais==pais,]
   lr  <- (tmp$leftRight - mean (tmp$leftRight, na.rm=T)) / sd (tmp$leftRight, na.rm=T)
   mgm <- tmp$Mood.md 
   imm <- tmp$interpol.policy.mood 
   imm[1] <- NA
   stimson <- tmp$median
   st.md <- (stimson - mean(stimson, na.rm=T)) / sd(stimson, na.rm=T)
   pdf ( paste (graphicsPath, "MgGannMood3MethodsPointEstimatesBW", pais,".pdf", sep=""), h=7, w=14)
   par (srt=0, las=2, mar=c(4,3,1,1), las=2)
   plot (c(1,length(mgm)),c(-3,3), type="n", main="", ylab="", xlab="", axes=F )
   axis (2)
   axis (1, at=c(1:length(tmp$year)), labels=tmp$year)
   par (las=0)
   mtext (side=2, line=2, text="Pro State <--- Mood ---> Pro Market")
   legend ("bottomright", legend=pais.nice[i], bty="n", cex=1.4)
   points ( xy.coords ( c(1:length(tmp$year)), imm ), type="b", lwd=3, pch=19, col="grey", bg="grey", cex=1.6)
   points ( xy.coords ( c(1:length(tmp$year)), mgm ), type="b", pch=19, col="black", cex=1.6)
   points ( xy.coords ( c(1:length(tmp$year)+0.1), st.md), type="b", pch=22, col=grisPrez, cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.legScore), type="p", pch=4, col="black", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.presScore), type="p", pch=2, col="black", cex=1)
   legend ("topleft"
           , ncol=2
           , col=c("black","grey",grisPrez,"black","black")
           , pch=c(19,19,22,4,2)
           , cex=0.8,
           , legend=c("Policy mood"
                      , "Policy mood (interpolated)"
                      ,"Blackbox score"
                      , "Voter-revealed rightism (Leg)"
                      , "Voter-revealed rightism (Pres)")
           , bty="n")
   dev.off ()
}


# Country-specific plots (including intervals)
for (i in 1:length(unique(MacroData$pais))) {
   pais <- unique(MacroData$pais)[i]
   tmp <- MacroData[MacroData$pais==pais,]
   lr  <- (tmp$leftRight - mean (tmp$leftRight, na.rm=T)) / sd (tmp$leftRight, na.rm=T)
   mgm <- tmp$Mood.md 
   mgl <- tmp$Mood.lo
   mgh <- tmp$Mood.hi
   imm <- tmp$interpol.policy.mood 
   iml <- tmp$interpol.lo.policy.mood 
   imh <- tmp$interpol.hi.policy.mood 
   imm[1] <- iml[1] <- imh[1] <- NA
   stimson <- tmp$median
   hi.stimson <- tmp$minimum
   lo.stimson <- tmp$maximum
   st.lo <- (lo.stimson - mean(stimson, na.rm=T)) / sd(stimson, na.rm=T)
   st.hi <- (hi.stimson - mean(stimson, na.rm=T)) / sd(stimson, na.rm=T)
   st.md <- (stimson - mean(stimson, na.rm=T)) / sd(stimson, na.rm=T)
   pdf ( paste (graphicsPath,"MgGannMood3MethodsNEW", pais,".pdf", sep=""), h=7, w=14)
   par (srt=0, las=2, mar=c(4,3,1,1), las=2)
   plot (c(1,length(mgm)),c(-3,3), type="n", main="", ylab="", xlab="", axes=F )
   axis (2)
   axis (1, at=c(1:length(tmp$year)), labels=tmp$year)
   par (las=0)
   mtext (side=2, line=2, text="Pro State <--- Mood ---> Pro Market")
   legend ("bottomright", legend=pais.nice[i], bty="n", cex=1.2)
   segments (y0=iml, y1=imh, x0=c(1:length(tmp$year)), x1=c(1:length(tmp$year)), col="grey", lwd=2)
   points ( xy.coords ( c(1:length(tmp$year)), imm ), lwd=3, pch=19, col="grey", bg="grey", cex=1.6)
   points ( xy.coords ( c(1:length(tmp$year)), mgm ), pch=19, col="black", cex=1.6)
   segments (y0=mgl, y1=mgh, x0=c(1:length(tmp$year)), x1=c(1:length(tmp$year)), col="black", lwd=2)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.legScore), type="p", pch=19, col="red", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), tmp$rescaled.presScore), type="p", pch=21, bg="pink", col="red", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)), lr), type="p", pch=21, col="blue", bg="blue", cex=1)
   points ( xy.coords ( c(1:length(tmp$year)+0.1), st.md), type="p", pch=19, col="green", cex=1)
   segments (y0=st.lo, y1=st.hi, x0=c(1:length(tmp$year)+0.1), x1=c(1:length(tmp$year)+0.1), col="green", lwd=1)
   legend ("topleft"
           , ncol=2
           , border=c("black","grey","green","blue","red","red")
           , fill=c("black","grey","green","blue","red","pink")
           , cex=0.8,
           , legend=c("Policy mood"
                      , "(interpolated policy mood)"
                      ,"Blackbox score"
                      , "Left-right self-placement"
                      , "Voter-revealed rightism (L)"
                      , "Voter-revealed rightism (P)")
           , bty="n")
   dev.off ()
}


# Does our measure of "mood" predict "voter-revealed rightism" well?
# There should be a small correspondence, after all.
xtable (cor (MacroData[,is.element (colnames(MacroData), c("interpol.policy.mood","median","rescaled.BG.score","leftRight"))], use="p"))
# Correlations are not particularly high (except of course between A-M and McGann mood measures)
cor.test.plus (cor.test (MacroData$median, MacroData$interpol.policy.mood))
cor.test.plus (cor.test (MacroData$rescaled.BG.score, MacroData$leftRight))
cor.test.plus (cor.test (MacroData$median, MacroData$rescaled.BG.score))
#


MacroData$paisNum <- as.numeric (MacroData$pais)
MacroData$time    <- MacroData$year - 1994


source ("Code/AuxFunctions/AuxFunctions.R")
Lags <- paneldata.lags(MacroData, "pais", "time"
                       , c("interpol.policy.mood","interpol.left.right","median"
                           ,"credit","growth","inflation","unemploy","misery")
                       , lags=1)
MacroData <- data.frame (cbind (MacroData, Lags))


write.table (MacroData, file="Datasets/OriginalDataFiles/macroMoodData.txt", sep="\t", row.names=FALSE)






