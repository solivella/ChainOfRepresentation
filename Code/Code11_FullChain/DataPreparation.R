#######################
## Data preparation
## script for analysis
## of cit->policies
#######################

library(dplyr)

setwd("~/Dropbox/ChainBook/Code/Code10_Conclusion/")
AllMoods1990 <- read.csv("../../Datasets/FinishedOutput/AllMoodsFinal1990.csv")
AllMoods <- AllMoods1990 %>%
  select(type, country, year, mood1990=mood, Heterogeneity1990=Heterogeneity)

lh_powers <- read.table("../../Datasets/OriginalDataFiles/PolicyMakingPowers/LowerHouseDoubleClusters.txt",sep="\t",header=TRUE)
uh_powers <- read.table("../../Datasets/OriginalDataFiles/PolicyMakingPowers/UpperHouseDoubleClusters.txt",sep="\t",header=TRUE)
ex_powers <-  read.table("../../Datasets/OriginalDataFiles/PolicyMakingPowers/ExecutiveClusters.txt",sep="\t",header=TRUE)
elec_rules <- read.csv("../../Datasets/FinishedOutput/Electoral_SystemsFinal.csv")
elec_rules <- elec_rules[with(elec_rules,order(country, type, year_begin)),]

##Helper functions

mo_cong <- function(mean,sd){
  if(any(is.na(c(mean))))
    return(NA)
  samps <- rnorm(1500,mean[2],sd)
  sum((samps-mean[2])^2)/sum((samps-mean[1])^2)
}
select.year <- function(years,y){
  dist.yr <- abs(years-y)[years<=y]
  if(length(dist.yr)==0)dist.yr <- Inf
  ifelse(min(dist.yr)>5,1800,max(years[years<=y]))
}
obtain.pref <- function(actor,data, ctry, yr){
  data <- subset(data,country==ctry&type==actor)
  best.yr <- select.year(data$year, yr)
  if(actor=="Citizen"){
    print(head(data))
    pref <- data$mood1990[data$year==best.yr]
    f_pref <- ifelse(length(pref)==0, NA, pref)
    het <- data$Heterogeneity1990[data$year==best.yr]
    f_pref_het <- ifelse(length(het)==0,NA,het)
    return(c(f_pref, f_pref_het))
  }
  pref_c <- data$mood_const[data$year==best.yr]
  f_pref_c <- ifelse(length(pref_c)==0, NA, pref_c)
  pref_s <- data$mood_spline[data$year==best.yr]
  f_pref_s <- ifelse(length(pref_s)==0, NA, pref_s)
  het_c <- data$Heterogeneity_const[data$year==best.yr]
  f_pref_het_c <- ifelse(length(het_c)==0,NA,het_c)
  het_s <- data$Heterogeneity_spline[data$year==best.yr]
  f_pref_het_s <- ifelse(length(het_s)==0,NA,het_s)
  e_pref_c <- data$exec_party_const[data$year==best.yr]
  f_e_pref_c <- ifelse(length(e_pref_c)==0, NA, e_pref_c)
  e_pref_s <- data$exec_party_spline[data$year==best.yr]
  f_e_pref_s <- ifelse(length(e_pref_s)==0, NA, e_pref_s)
  m_pref_c <- data$median_party_const[data$year==best.yr]
  f_m_pref_c <- ifelse(length(m_pref_c)==0, NA, m_pref_c)
  m_pref_s <- data$median_party_spline[data$year==best.yr]
  f_m_pref_s <- ifelse(length(m_pref_s)==0, NA, m_pref_s)
  return(c(f_pref_c,f_pref_het_c,
           f_pref_s,f_pref_het_s, f_e_pref_c, f_m_pref_c, f_e_pref_s, f_m_pref_s))
}
obtain.elec <- function(actor, data, ctry, yr){

  right_set <- subset(elec_rules
                      ,country==ctry)
  if(any(as.character(right_set$type)==as.character(actor))){
    right_set <- subset(right_set, as.character(type)==actor)
    if(yr >= max(right_set$year_end)){
      right_row <- right_set[which.max(right_set$year_end),]
    } else{
      right_row <-  right_set[(yr >= right_set$year_begin & yr < right_set$year_end),]
    }
    if(nrow(right_row)>1)
      right_row <- right_row[1,]
    else if(nrow(right_row)==0)
      return(NA);
    return(right_row$Group)
  }else{
    return(NA)
  }
}


pm_pref <- subset(AllMoods,!type%in%c("Policy", "Citizen"))
pm_pref <- pm_pref[with(pm_pref,order(country, type, year)),]

##Import party data
PartyData <- read.table("../../Datasets/FinishedOutput/allMedianMoods.txt",
                        header = TRUE,
                        sep="\t")
pm_pref <- PartyData %>%
  select(country, year, legislative.median, prez.party.mood, median.party.mood) %>%
  mutate(country=recode(country,"Brasil"="Brazil",
                        "RepublicaDominicana"="DominicanRep")) %>%
  right_join(pm_pref)
##Interpolation
pm_pref <- plyr::ddply(pm_pref, c("country","type"),
                 function(x){
                   pp <- diff(range(x$year))
                   moods1 <- spline(x$year,x$mood1990, pp+1)
                   hets1 <- spline(x$year,log(x$Heterogeneity1990), pp+1)
                   moods2 <- approx(x$year, x$mood1990, method="constant", n = pp+1)
                   hets2 <- approx(x$year, log(x$Heterogeneity1990), method="constant", n = pp+1)
                   if(all(is.na(x$prez.party.mood))){
                     e_mood_s <- e_mood_c <- list()
                     e_mood_s$y <- e_mood_c$y <- rep(NA, pp+1)
                   } else {
                     e_mood_s <- spline(x$year, x$prez.party.mood, pp+1)
                     e_mood_c <- approx(x$year, x$prez.party.mood, method="constant", n=pp+1)
                   }
                   if(all(is.na(x$median.party.mood))){
                     party_mood_s <- party_mood_c <- list()
                     party_mood_s$y <- party_mood_c$y <- rep(NA, pp + 1)
                   } else {
                     party_mood_s <- spline(x$year, x$median.party.mood, pp+1)
                     party_mood_c <- approx(x$year, x$median.party.mood, method = "constant", n=pp+1)
                   }
                   data.frame(year=moods1$x,type=x$type[1],country=x$country[1]
                              ,mood_const = moods2$y,Heterogeneity_const = exp(hets2$y)
                              ,mood_spline=moods1$y,Heterogeneity_spline=exp(hets1$y),
                              exec_party_spline=e_mood_s$y, median_party_spline = party_mood_s$y,
                              exec_party_const=e_mood_c$y, median_party_const = party_mood_c$y
                   )

                 })
Citizen.Moods <- subset(AllMoods, type == "Citizen")
Citizen.Moods <- Citizen.Moods[with(Citizen.Moods,order(country, year)),]

##Spline interpolation
Citizen.Moods <- plyr::ddply(Citizen.Moods, c("country","type"),
                       function(x){
                         n <- diff(range(x$year))
                         moods <- spline(x$year,x$mood1990, n+1)
                         hets <- spline(x$year,log(x$Heterogeneity1990), n+1)
                         data.frame(year=moods$x,type=x$type[1],country=x$country[1],mood1990=moods$y,Heterogeneity1990=exp(hets$y))
                       })

policies <- subset(AllMoods,type=="Policy")
## Change country names to match other files
lh_powers$country <- gsub("\\s","",lh_powers$country)
lh_powers$country <- gsub("DominicanRepublic","DominicanRep",lh_powers$country)
lh_powers$country <- gsub("Salvador","ElSalvador",lh_powers$country)

uh_powers$country <- gsub("\\s","",uh_powers$country)
uh_powers$country <- gsub("DominicanRepublic","DominicanRep",uh_powers$country)
ex_powers$country <- gsub("\\s","",ex_powers$country)
ex_powers$country <- gsub("DominicanRepublic","DominicanRep",ex_powers$country)
ex_powers$country <- gsub("Salvador","ElSalvador",ex_powers$country)



full_data <- adply(policies,1, function(r,lh,uh,ex,pref,pm_pref, elec){
  ctry <- r$country
  yr <- r$year
  lh_clust <- lh$lower.house.type[lh$country==ctry&yr>=lh$startyear&yr<=lh$endyear]
  r$LH_clust <- ifelse(length(lh_clust)==0,NA,paste("LH-",lh_clust,sep=""))
  uh_clust <- uh$upper.house.type[uh$country==ctry&yr>=uh$startyear&yr<=uh$endyear]
  r$UH_clust <- ifelse(length(uh_clust)==0,NA,paste("UH-",uh_clust,sep=""))
  e_clust <- ex$president.type[ex$country==ctry&yr>=ex$startyear&yr<=ex$endyear]
  r$E_clust <- ifelse(length(e_clust)==0,NA,as.character(e_clust))

  prefs_c <- obtain.pref("Citizen",pref,ctry,yr)
  r$Cit_pref <- prefs_c[1]
  r$Cit_pref_het <- prefs_c[2]
  r$ExecGroup <- obtain.elec("President", elec, ctry, yr)
  r$LegGroup <- obtain.elec("Legislator", elec, ctry, yr)
  r$SenGroup <- obtain.elec("Senator", elec, ctry, yr)
  r$Lpref <- obtain.pref("Legislator",pm_pref,ctry,yr)[3]
  r$Epref <- obtain.pref("President",pm_pref,ctry,yr)[3]
  r$ExecParty_pref <- obtain.pref("Legislator",pm_pref,ctry,yr)[7]
  r$MedianParty_pref <- obtain.pref("Legislator",pm_pref,ctry,yr)[8]
  r$LEDist <- abs(r$Lpref - r$Epref)
  return(r)
},
lh=lh_powers,uh=uh_powers,ex=ex_powers,pref=Citizen.Moods,elec=elec_rules, pm_pref=pm_pref)

full_data <- subset(full_data, !(is.na(ExecGroup))&!(is.na(LegGroup)))

## Import controls
controls <- read.csv("../../Datasets/OriginalDataFiles/Control_variables.csv")
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
full_data <- merge(full_data,controls,all.x=TRUE)
full_data$decade <- as.factor(as.numeric(full_data$year) - (as.numeric(full_data$year)%%10))

##Get lagged values
full_data <- full_data[with(full_data,order(country, year)),]
full_data$lag_policy <- ave(full_data$mood1990, full_data$country,
                            FUN=function(x){
                              c(NA,x[-length(x)])
                            })
full_data$ChangePolicy <- with(full_data, mood1990 - lag_policy)
full_data <- full_data[with(full_data, order(country, year)),]
full_data$lag_Cit_pref <- ave(full_data$Cit_pref, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeCitPref <- with(full_data, Cit_pref - lag_Cit_pref)
full_data$lag_ChangeCitPref <- ave(full_data$ChangeCitPref, full_data$country,
                                      FUN=function(x)c(NA, x[-length(x)]))
full_data$lag_Cit_pref_het <- ave(full_data$Cit_pref_het, full_data$country,
                              FUN=function(x)c(NA, x[-length(x)]))

full_data <- subset(full_data, !is.na(Cit_pref))

## Compute congruence measures
full_data <- adply(full_data,1
                   ,function(x){
                     x$oo_cong <- 1/(1+abs(x$lag_Cit_pref - x$mood1990))
                     x$mo_cong <- mo_cong(c(x$mood1990,x$lag_Cit_pref),x$lag_Cit_pref_het)
                     return(x)
                   })
full_data <- subset(full_data, LH_clust!="<NA>")
full_data$ProcessName <- with(full_data, paste(LH_clust,UH_clust,E_clust,sep=":"))
all_nums <- c(12, 1, 3, 6, 10, 9, 11, 2, 4, 5, 7)
all_nums_collapsed <- c(7, 1, 2, 4, 6, 5, 6, 1, 2, 3, 4)
names(all_nums) <- unique(full_data$ProcessName)
names(all_nums_collapsed) <- unique(full_data$ProcessName)
full_data$ProcessNumberAll <- all_nums[(full_data$ProcessName)]
full_data$ProcessCollapsed <- all_nums_collapsed[full_data$ProcessName]

## Write data
write.csv(full_data,"../../Datasets/FinishedOutput/CitPrefESPow.csv"
          ,row.names=FALSE)


