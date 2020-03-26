#####################################################################
# basicspacePELA.R
# SO: December 29, 2014
# INPUT: PELAIndividualResponses.RData
# OUTPUT: basicspacePELA.RData
# This file reads individual-level issue scales from PELA 
# then estimates a common basic space for Latin American legislators
#####################################################################

library(basicspace)
library(plyr)
library(stringr)
library(gtools)
library(fields)
library(tidyverse)
library(ggrepel)

#################################################################
#### Load dataset with multiply-imputed legislator responses ####
#################################################################
load("Datasets/OriginalDataFiles/PELAIndividualResponsesMI.RData")

#############################################
#### Questions used for Blackbox scaling ####
#############################################
vars2get <- c("state.not.solves.problems"         
              ,"state.not.reduce.inequality"
              ,"state.limited.scope"           
              ,"economy.best.in.private.sector"
              ,"state.not.in.price.control"        
              ,"state.not.in.primsec.edu" 
              ,"state.not.in.housing"              
              ,"state.not.in.job.creation" 
              ,"state.not.in.pensions"               
              ,"state.not.in.high.edu"
              ,"state.not.in.unemployment"         
              ,"state.not.in.basic.needs"          
              ,"privatization.beneficial"
			  ,"state.not.in.sanitation"
			  ,"market.best")

Aggr.Data.Sal <- LegMI
####################################
#### Correct Peru party weights ####
####################################

Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="UPP"] <- 0.375/0.3523
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="PAP"] <- 0.3/0.2727
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="AF"] <- 0.108333333/0.1477 
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
              & Aggr.Data.Sal$year==2006
              & Aggr.Data.Sal$PartyAcronym=="PPC"] <- 0.016666667/0.09659 
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="UN"] <- 0.141666667/0.0625
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="FC"] <- 0.041666667/0.02273 
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="PNP"] <- 0.008333333/0.04545
Aggr.Data.Sal$weight[Aggr.Data.Sal$pais=="Peru" 
                     & Aggr.Data.Sal$year==2006
                     & Aggr.Data.Sal$PartyAcronym=="Other"] <- 0.008333333/0.04545


################################
#### Blackbox decomposition ####
################################
LegRoll <- Aggr.Data.Sal[,vars2get]


result <- blackbox (as.matrix(LegRoll)
                    , dims=1
                    , minscale=1
                    , verbose=TRUE) #Extremely fast

summary(result) 

Aggr.Data.Sal$pais <- gsub("\\s","",Aggr.Data.Sal$pais)
result$individuals[[1]]$paisYear <- with(Aggr.Data.Sal,paste(pais,year,sep=""))
result$individuals[[1]]$PartyCode <- Aggr.Data.Sal$PartyCode
result$individuals[[1]]$PartyAcronym <- Aggr.Data.Sal$PartyAcronym
result$individuals[[1]]$SelfIdeol <- Aggr.Data.Sal$LR.ideology.self
result$individuals[[1]]$PresIdeol <- Aggr.Data.Sal$LR.ideology.prez1
result$individuals[[1]]$Seat.ShareLH <- Aggr.Data.Sal$Seat.ShareLH
result$individuals[[1]]$weights <- Aggr.Data.Sal$weight
result$individuals[[1]]$Term <- Aggr.Data.Sal$Term
#result$individuals[[1]]$c1 <- result$individuals[[1]]$c1*-1
result$individuals[[1]]$weightedMood <- result$individuals[[1]]$weights * (result$individuals[[1]]$c1 * (-1))

# result$individuals[[1]]  <-  result$individuals[[1]][complete.cases(result$individuals[[1]]),]
# In carrying out this step, we go from 76 to 57 paisYear combinations. Why?

save (result, file="Datasets/OriginalDataFiles/bbLegMoods2.RData")

################################################################################
#### Aggregating responses (moods) at the party level to find party medians ####
################################################################################

res.by.party  <- plyr::ddply(result$individuals[[1]]
                       ,c("paisYear","PartyCode","Seat.ShareLH","PartyAcronym","weights")
                       ,.fun=function(x){
                         data.frame(mood=median(x[,"weightedMood"], na.rm=T)
                            ,n.resp=length(x[,"weightedMood"]))
                            # ,sd.mood=sd(x[,"weightedMood"])
                            # ,ideol=median(x[,"SelfIdeol"])
                            # ,pres.ideol=median(x[,"PresIdeol"]))
                       })


######################################################################################
#### Aggregating responses (moods) at the assembly level to find assembly medians ####
######################################################################################
res.by.party$pais <- str_sub(res.by.party$paisYear, start=1, end=-5)
res.by.party$year <- as.numeric(str_sub(res.by.party$paisYear, start= -4))

# In running this step we move from 18 to 17
res.by.country.year <- ddply(result$individuals[[1]]
                             ,c("paisYear")
                             ,.fun=function(x){
                               data.frame(mood=median(x[,"weightedMood"], na.rm=TRUE)
                                          ,n.resp=length(x[,"weightedMood"]))
                             })
res.by.country.year$pais <- str_sub(res.by.country.year$paisYear, start=1, end=-5)
res.by.country.year$year <- as.numeric(str_sub(res.by.country.year$paisYear, start= -4))

####################################################################
#### Reading auxiliary information to find presidential parties ####
####################################################################
Pela <- LegMI    #; rm (LegMI)
Pela$pais <- car::recode (Pela$pais, "'Costa Rica'='CostaRica';
                          'El Salvador'='ElSalvador';
                          'Republica Dominicana'='RepublicaDominicana'")
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
                         ,"PRE","DP","PRIAN","PRIAN","MPAIS" # Ecuador
                         ,"ARENA","ARENA","ARENA","ARENA","FMLN" # El Salvador
                         ,"PAN","FRG","Patriota","UNE","Patriota" # Guatemala
                         ,"PL","PL","PN","PL","PN"  # Honduras
                         ,"PRI","PRI","PAN","PAN","PAN","PAN" # Mexico
                         ,"AL","PLC","FSLN"  # Nicaragua
                         ,"PA","PRD","CD"   # Panama 
                         ,"ANR","ANR","ANR","PLRA"  # Paraguay
                         # Paraguay (Changed Lugo's partisanship from Other to PLRA. Lugo was not affiliated to this party, but PLRA was his most important backer)
                         ,"Cambio90","PP","PAP"  # Peru
                         ,"PRD","PLD","PRD","PLD","PLD" # DomRep
                         ,"PC","PC","FA","FA" # Uruguay
                         ,"AD","MVR")   # Venezuela

prezParties <- data.frame (presidentialParties=presidentialParties
                           , paisYear=id)
prezParties$pais <- unlist (strsplit (as.character(prezParties$paisYear), split="-"))[odd(1:(nrow(prezParties)*2))]
prezParties$year <- unlist (strsplit (as.character(prezParties$paisYear), split="-"))[even(1:(nrow(prezParties)*2))]

##############################################################
#### Function to find party medians and president's party ####
##############################################################
# temp <- tempData[tempData$year==unique(tempData$year)[j],]  # Test (keep other party in)
# Function to find all relevant medians
which.party.median <- function (temp, temp2) {
  sort.temp <- temp[order(temp$mood),]
  # sort.temp$sample.shares <- 100*(sort.temp$n.resp / sum (sort.temp$n.resp)) 
  # sort.temp$recovered.seat.shares <- sort.temp$sample.shares * sort.temp$weights
  sort.temp$recovered.seat.shares <- sort.temp$Seat.ShareLH
  # formula to approximate original party seat shares: seat.share=sample.share*weights
  # where "weights" are the over/undersample weights we had constructed
  short.sort.temp <- sort.temp[sort.temp$PartyAcronym != "Other",]
  # we simply ignore "other" parties
  cumsum.series <- cumsum (short.sort.temp$recovered.seat.shares)
  median.party <- short.sort.temp$PartyAcronym[which (cumsum.series > sum(short.sort.temp$recovered.seat.shares)/2)[1]]
  median.party.mood <- short.sort.temp$mood[which (cumsum.series > sum(short.sort.temp$recovered.seat.shares)/2)[1]]
  prez.party <- short.sort.temp$PartyAcronym[which(as.character(short.sort.temp$PartyAcronym)==as.character(temp2$presidentialParties))]
  prez.party.mood <- short.sort.temp$mood[which(as.character(short.sort.temp$PartyAcronym)==as.character(temp2$presidentialParties))]
  return (data.frame(median.party, median.party.mood, prez.party, prez.party.mood))
}

####################################################################
#### Use which.party.median to collect all relevant information ####
####################################################################
AllMedians <- c()
for (i in 1:length(unique(res.by.party$pais))) {
   which.country <- unique(res.by.party$pais)[i]
   tempData <- res.by.party[res.by.party$pais==which.country,]
   countryData <- res.by.country.year[res.by.country.year$pais==which.country,]
   for (j in 1:length(unique(tempData$year))) {
      which.year <- unique(tempData$year)[j]
      miniTempData <- tempData[tempData$year==which.year,]
      miniCountryData <- countryData[countryData$year==which.year,]
      miniPrezData <- prezParties[prezParties$pais==which.country & prezParties$year==which.year,] 
      dat <- which.party.median (temp=miniTempData, temp2=miniPrezData)
      dat$legislative.median <- countryData$mood[countryData$pais==which.country & countryData$year==which.year]
      dat$country <- which.country
      dat$year <- which.year
      AllMedians <- rbind (AllMedians, dat)
   }      
}

write.table (AllMedians
             , file="Datasets/FinishedOutput/allMedianMoods.txt"
             , sep="\t"
             , row.names=FALSE)

graphicsPath <- "Graphs/"

prettyCountryNames <- c("Argentina","Mexico","Chile","El Salvador"
                        ,"Bolivia","Honduras","Colombia","Costa Rica"
                        ,"Nicaragua","Guatemala","Paraguay","Ecuador"
                        ,"Dominican Republic","Peru","Uruguay"
                        ,"Venezuela","Panama","Brazil")
res.by.country.year$pais <- dplyr::recode(res.by.country.year$pais,
                                                   "RepublicaDominicana" = "Dominican Republic",
                                                   "ElSalvador" = "El Salvador",
                                                   "CostaRica" = "Costa Rica")


pdf (paste0 (graphicsPath, "PartyPositionsBlackboxI.pdf"), h=6, w=15)
res.by.party.I <- res.by.party %>%
  dplyr::select(PartyAcronym, mood, pais, year) %>%
  dplyr::mutate(Actor="Within-party median") %>%
  dplyr::left_join(dplyr::mutate(dplyr::select(prezParties, presidentialParties, pais, year), year=as.numeric(year))) %>%
  dplyr::mutate(presidentialParties = as.character(PartyAcronym)==as.character(presidentialParties)) %>%
  dplyr::mutate(Actor = ifelse(presidentialParties, "President", Actor)) %>%
  mutate(pais = dplyr::recode(.$pais,
                              "DominicanRep" = "Dominican Republic",
                              "RepublicaDominicana" = "Dominican Republic",
                              "ElSalvador" = "El Salvador",
                              "CostaRica" = "Costa Rica")) %>%
  dplyr::filter(pais %in% unique(pais)[1:9]) 

res.by.country.year.filt.I <- res.by.country.year %>%
  dplyr::mutate(Actor="Overall median") %>%
  dplyr::select(pais, year, mood, Actor) %>%
  dplyr::filter(pais %in% unique(res.by.party.I$pais)) 


  ggplot(res.by.party.I, aes(x=mood, y=as.factor(year), color=Actor, shape=Actor)) +
  facet_wrap(~pais, nrow=3, scales="free_y") +
  scale_color_manual(values=c("black", "black", "gray")) +
  scale_shape_manual(values=c(0, 19, 19)) +
  geom_text_repel(aes(label=PartyAcronym), direction="x", angle=45, force=0.1,show.legend = FALSE, size=3)+
  geom_point(data=res.by.country.year.filt.I)+
  theme_bw() +
  theme(panel.grid.major.x = element_blank() 
        ,panel.grid.minor.x = element_blank())+
  xlab("Pro State <--- Mood ---> Pro Market") +
  ylab("Year")
dev.off()  

pdf (paste0 (graphicsPath, "PartyPositionsBlackboxII.pdf"), h=6, w=15)
res.by.party.II <- res.by.party %>%
  dplyr::select(PartyAcronym, mood, pais, year) %>%
  dplyr::mutate(Actor="Within-party median") %>%
  dplyr::left_join(dplyr::mutate(dplyr::select(prezParties, presidentialParties, pais, year), year=as.numeric(year))) %>%
  dplyr::mutate(presidentialParties = as.character(PartyAcronym)==as.character(presidentialParties)) %>%
  dplyr::mutate(Actor = ifelse(presidentialParties, "President", Actor)) %>%
  mutate(pais = dplyr::recode(.$pais,
                              "DominicanRep" = "Dominican Republic",
                              "RepublicaDominicana" = "Dominican Republic",
                              "ElSalvador" = "El Salvador",
                              "CostaRica" = "Costa Rica")) %>%
  dplyr::filter(pais %in% unique(pais)[10:18]) 

res.by.country.year.filt.II <- res.by.country.year %>%
  dplyr::mutate(Actor="Overall median") %>%
  dplyr::select(pais, year, mood, Actor) %>%
  dplyr::filter(pais %in% unique(res.by.party.II$pais)) 


ggplot(res.by.party.II, aes(x=mood, y=as.factor(year), color=Actor, shape=Actor)) +
  facet_wrap(~pais, nrow=3, scales="free_y") +
  scale_color_manual(values=c("black", "black", "gray")) +
  scale_shape_manual(values=c(0, 19, 19)) +
  geom_text_repel(aes(label=PartyAcronym), direction="x", angle=45, force=0.1,show.legend = FALSE, size=3)+
  geom_point(data=res.by.country.year.filt.II)+
  theme_bw() +
  theme(panel.grid.major.x = element_blank() 
        ,panel.grid.minor.x = element_blank())+
  xlab("Pro State <--- Mood ---> Pro Market") +
  ylab("Year")
dev.off()  




for (i in 1:length(unique(res.by.party$pais))) {
   which.country <- unique(res.by.party$pais)[i]
   tempData <- res.by.party[res.by.party$pais==which.country,]
   countryData <- res.by.country.year[res.by.country.year$pais==which.country,]
   range <- (max (tempData$mood[tempData$PartyAcronym!="Other"], na.rm=T)-min (tempData$mood[tempData$PartyAcronym!="Other"], na.rm=T))
   min.range <- min (tempData$mood[tempData$PartyAcronym!="Other"], na.rm=T) 
   max.range <- max (tempData$mood[tempData$PartyAcronym!="Other"], na.rm=T) 
   minimum <- min.range - 0.05*range
   maximum <- max.range + 0.05*range
   no.years <- length(unique(tempData$year))
   pdf (paste0 (graphicsPath, "PartyPositionsBlackbox", which.country,".pdf"), h=7, w=9)
   plot (c(minimum,maximum), c(1,no.years+2), type="n"
         , xlab="Policy mood"
         , ylab=""
         , main=prettyCountryNames[i]
         , axes=F, cex=1.2)
   axis (1)
   axis (2, at=c(2:(length(unique(tempData$year))+1)), labels=unique(tempData$year), las=1)
   # legend ("topright"
   #         , legend=c("Median legislator","President's party","Within-party median")
   #         , col=c("black","black","gray")
   #         , pch=c(1,19,19), bty="n")
   for (j in 1:length(unique(tempData$year))) {
      which.year <- unique(tempData$year)[j]
      miniTempData <- tempData[tempData$year==which.year & tempData$PartyAcronym!="Other",]
      miniCountryData <- countryData[countryData$year==which.year,]
      miniPrezData <- prezParties[prezParties$pais==which.country & prezParties$year==which.year,] 
      prezParty <- which (as.character (miniTempData$PartyAcronym)==as.character (miniPrezData$presidentialParties))
      if (even(length(miniTempData$mood))) {
         min.mood <- min (miniTempData$mood, na.rm=T)
         max.mood <- max (miniTempData$mood, na.rm=T)
         y.coord <- jitter(rep(c(j+1.25, j+0.75), nrow(miniTempData)/2), amount=0.15)
         if (length(miniTempData$mood) > 3) {
            x.coord <- min.range + ((miniTempData$mood-min.mood)/(max.mood-min.mood))*range
         } else {
            x.coord <- miniTempData$mood
         }
         segments (x0=x.coord, y0=y.coord
                   , x1=miniTempData$mood, y1=rep((j+1), nrow(miniTempData))
                   , lwd=1, col="grey")
         text (xy.coords(x.coord, y.coord) 
               , labels=miniTempData$PartyAcronym, cex=0.9)
      } else {
         min.mood <- min (miniTempData$mood, na.rm=T)
         max.mood <- max (miniTempData$mood, na.rm=T)
         y.coord <- jitter(c(rep(c(j+1.25, j+0.75), floor(nrow(miniTempData)/2)), j+1.2), amount=0.15)
         if (length(miniTempData$mood) > 3) {
            x.coord <- min.range + ((miniTempData$mood-min.mood)/(max.mood-min.mood))*range
         } else {
            x.coord <- miniTempData$mood
         }
         segments (x0=x.coord, y0=y.coord
                   , x1=miniTempData$mood, y1=rep((j+1), nrow(miniTempData))
                   , lwd=1, col="grey")
         text (xy.coords(x.coord, y.coord)
               , labels=miniTempData$PartyAcronym, cex=0.7)
      }
      points (xy.coords(miniTempData$mood, rep((j+1), nrow(miniTempData))), pch=19, col="gray")
      points (xy.coords(miniCountryData$mood, (j+1)), pch=0, col="black", cex=1.3)
      points (xy.coords(miniTempData$mood[prezParty], (j+1)), pch=19, col="black")
   }
   dev.off()
}


# Additional legend plot
pdf (paste0 (graphicsPath, "PartyPositionsLegend", ".pdf"), h=7, w=9)
par (mar=c(0,0,0,0))
plot (c(0,1), c(0,1), type="n"
      , xlab=""
      , ylab=""
      , main=""
      , axes=F)
legend(x=0, y=0.9, bty="n"
       , pch=c(0,19,19,NA), col=c("black","gray","black",NA)
       , legend=c("Median legislator"
                  , "Median within-party legislator"
                  , "Median within-party legislator in", "the President's party")
       , cex=2)
dev.off()




