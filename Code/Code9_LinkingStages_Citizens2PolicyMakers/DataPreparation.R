#######################
## Data preparation
## script for analysis
## of cit->PM
#######################

library(car)
library (reshape)
library(plyr)
library (dplyr)

## Aux functions

ks_cong <- function(mean,sd){
  if(any(is.na(c(mean))))
    return(NA)
  samp1 <- rnorm(1000, mean[1], sd[1])
  samp2 <- rnorm(1000, mean[2], sd[2])
  1-ks.test(samp1,samp2)$statistic
}

mo_cong <- function(mean,sd){
  if(any(is.na(c(mean))))
    return(NA)
  samps <- rnorm(1500,mean[2],sd)
  sum((samps-mean[2])^2)/sum((samps-mean[1])^2)
}


## Import new moods
AllMoods1990 <- read.csv("Datasets/FinishedOutput/AllMoodsFinal1990.csv")
AllMoods <- AllMoods1990 %>%
  dplyr::select(type, country, year, mood1990=mood, Heterogeneity1990=Heterogeneity)
  

##Import party data
PartyData <- read.table("Datasets/FinishedOutput/allMedianMoods.txt",
                        header = TRUE,
                        sep="\t")
AllMoods <- PartyData %>%
  dplyr::select(country, year, legislative.median, prez.party.mood, median.party.mood) %>%
  dplyr::mutate(country=dplyr::recode(country,"Brasil"="Brazil",
         "RepublicaDominicana"="DominicanRep")) %>%
  dplyr::right_join(AllMoods)


AllMoods <- subset(AllMoods, type != "Policy")
Moods.stack <- subset(AllMoods, type != "Citizen")
##Interpolation
Moods.stack <- ddply(Moods.stack, c("country","type"),
      function(x){
        num <- diff(range(x$year))
        moods_s <- spline(x$year,x$mood1990, num+1)
        if(all(is.na(x$prez.party.mood))){
          e_mood_s <- e_mood_c <- list()
          e_mood_s$y <- e_mood_c$y <- rep(NA, num+1)
        } else {
          e_mood_s <- spline(x$year, x$prez.party.mood, num+1)
          e_mood_c <- approx(x$year, x$prez.party.mood, method="constant", n=num+1)
        }
        if(all(is.na(x$median.party.mood))){
          party_mood_s <- party_mood_c <- list()
          party_mood_s$y <- party_mood_c$y <- rep(NA, num + 1)
        } else {
          party_mood_s <- spline(x$year, x$median.party.mood, num+1)
          party_mood_c <- approx(x$year, x$median.party.mood, method = "constant", n=num+1)
        }
        hets_s <- spline(x$year,log(x$Heterogeneity1990), num+1)
        moods_c <- approx(x$year,x$mood1990,method="constant",n = num+1)
        hets_c <- approx(x$year,log(x$Heterogeneity1990), method="constant", n=num+1)
        data.frame(year=moods_s$x,type=x$type[1],country=x$country[1],
                   mood_spline=moods_s$y,Heterogeneity_spline=exp(hets_s$y),
                   mood_const=moods_c$y,Heterogeneity_const=exp(hets_c$y),
                   exec_party_spline=e_mood_s$y, median_party_spline = party_mood_s$y,
                   exec_party_const=e_mood_c$y, median_party_const = party_mood_c$y
                   )
        })
Citizen.Moods <- subset(AllMoods, type == "Citizen")
##Spline interpolation
Citizen.Moods <- ddply(Citizen.Moods, c("country","type"),
                       function(x){
                         n <- diff(range(x$year))
                         moods <- spline(x$year,x$mood1990, n+1)
                         hets <- spline(x$year,log(x$Heterogeneity1990), n+1)
                         data.frame(year=moods$x,type=x$type[1],country=x$country[1],mood=moods$y,Heterogeneity=exp(hets$y))
                       })


Moods.stack <- ddply(Moods.stack,c("country","year")
                     , function(x){
                       cty <- unique(x$country)[1]
                       yr <- unique(x$year)[1]
                       cit_mood <- Citizen.Moods[Citizen.Moods$country==cty,]
                       dists <- cit_mood[,"year"]-yr
                       cit <- NULL
                       if(any(dists==-1)){
                           cit <- cit_mood[which(dists==-1),]
                       }
                       else if(any(dists==0)&any(dists==-2))
                           cit <- cit_mood[which(dists==0|dists==-2),]
                       else if(any(dists==0))
                           cit <- cit_mood[which(dists==0),]
                       if(is.null(cit)){
                         x$CitizenMood <- NA
                         x$CitizenHet <- NA
                         return(x)
                       }else{
                         x$CitizenMood <- mean(cit[,"mood"])
                         x$CitizenHet <- mean(cit[,"Heterogeneity"])
                       }
                       return(x)
                     })




#################################################################
#### Import data on institutional rules for political actors ####
#################################################################
elec_rules <- read.csv("Datasets/FinishedOutput/Electoral_SystemsFinal.csv")
Moods.stack$sa.formula <- Moods.stack$dm <- Moods.stack$thresh <- Moods.stack$elec_family <- Moods.stack$group <-  NA

moods_rules <- adply(Moods.stack,1
                     ,function(x){
                       x$sa.formula <- NA
                       x$dm <- NA
                       x$thresh <- NA
                       x$elec_family <- NA
                       x$group <- NA
                       if(grepl("Citizen",x$type)){
                         return(x)
                       }
                       else{
                         right_row <- subset(elec_rules
                                             ,country==x$country &
                                               (x$year >= year_begin & x$year < year_end) &
                                               as.character(type)==as.character(x$type)
                         )
                         if(nrow(right_row)>1)
                           right_row <- right_row[1,]
                         else if(nrow(right_row)==0)
                           return(x);
                         x$dm <- right_row$Mean_Magnitude
                         x$thresh <- right_row$Threshold
                         x$elec_family <- right_row$ES_family
                         x$sa.formula <- right_row$Seat_Formula
                         x$group <- right_row$Group
                         return(x)
                       }
                     })



#################################################################
#### Import controls ####
#################################################################
controls <- read.csv("Datasets/OriginalDataFiles/Control_variables.csv")
controls$unemploy <- ifelse(is.na(controls$unemployment_wb)
                            ,controls$unemployment_iadb
                            ,controls$unemployment_wb)
controls <- controls[order(controls$country,controls$year),c("country"
                                                             ,"year"
                                                             ,"unemploy"
                                                             ,"inflation_cpi"
                                                             ,"gdpgrowth.y")]

controls$inflation_cpi <- ave(controls$inflation_cpi, controls$country, FUN=scale)

# 2yr moving averages
controls$gdp2y <- ave(controls$gdpgrowth.y,controls$country,FUN=function(x)stats::filter(x,filter=rep(1/2,2),sides=1))
controls$infl2y <- ave(controls$inflation_cpi,controls$country,FUN=function(x)stats::filter(x,filter=rep(1/2,2),sides=1))
controls$unemp2y <- ave(controls$unemploy,controls$country,FUN=function(x)stats::filter(x,filter=rep(1/2,2),sides=1))
controls <- controls[,c("country","year","gdp2y","infl2y","unemp2y")]

full_data_long <- merge(moods_rules,controls,all.x=TRUE)


## Preprocess data

cit_pm_long <- subset(full_data_long,!is.na(CitizenMood)&!is.na(group))

cit_pm_long$decade <- as.factor(as.numeric(cit_pm_long$year) - (as.numeric(cit_pm_long$year)%%10))

cit_pm_long <- cit_pm_long[with(cit_pm_long,order(country, type, year)),]
cit_pm_long$lag_mood_const <- ave(cit_pm_long$mood_const,cit_pm_long$country,cit_pm_long$type
                                  ,FUN=function(x)c(NA,x[-length(x)]))
cit_pm_long$ChangeMoodConst <- with(cit_pm_long, mood_const-lag_mood_const)
cit_pm_long$lag_mood_spline <- ave(cit_pm_long$mood_spline,cit_pm_long$country,cit_pm_long$type
                                   ,FUN=function(x)c(NA,x[-length(x)]))
cit_pm_long$ChangeMoodSpline <- with(cit_pm_long, mood_spline-lag_mood_spline)
cit_pm_long$lag_CitMood <- ave(cit_pm_long$CitizenMood,cit_pm_long$country,cit_pm_long$type
                               ,FUN=function(x)c(NA,x[-length(x)]))
cit_pm_long$lag_CitHet <- ave(cit_pm_long$CitizenHet,cit_pm_long$country,cit_pm_long$type
                               ,FUN=function(x)c(NA,x[-length(x)]))
cit_pm_long$ChangeCitMood <- with(cit_pm_long, CitizenMood - lag_CitMood)
cit_pm_long$lag_ChangeCitMood <- ave(cit_pm_long$ChangeCitMood,cit_pm_long$country,cit_pm_long$type
                                     ,FUN=function(x)c(NA,x[-length(x)]))

cit_pm_long <- adply(cit_pm_long,1
                     ,function(x){
                       x$mm_cong_spline <- ks_cong(c(x$mood_spline,x$lag_CitMood)
                                                ,c(x$Heterogeneity_spline,x$lag_CitHet))
                       x$mm_cong_const <- ks_cong(c(x$mood_const,x$lag_CitMood)
                                                       ,c(x$Heterogeneity_const,x$lag_CitHet))

                       x$oo_cong_spline <- 1/(1+abs(x$mood_spline-x$lag_CitMood))
                       x$oo_cong_const <- 1/(1+abs(x$mood_const-x$lag_CitMood))

                       x$mo_cong_spline <- mo_cong(c(x$mood_spline,x$lag_CitMood),x$lag_CitHet)
                       x$mo_cong_const <- mo_cong(c(x$mood_const,x$lag_CitMood),x$lag_CitHet)

                       return(x)
                     })



#Write data
write.csv(cit_pm_long,"Datasets/FinishedOutput/FullDataLong.csv"
          ,row.names=FALSE)

