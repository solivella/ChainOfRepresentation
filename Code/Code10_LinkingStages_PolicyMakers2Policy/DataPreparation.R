#######################
## Data preparation
## script for analysis
## of PM->policies
#######################


library(plyr)
library(dplyr)
## Import data
lh_powers <- read.table("Datasets/OriginalDataFiles/PolicyMakingPowers/LowerHouseDoubleClusters.txt",sep="\t",header=TRUE)
uh_powers <- read.table("Datasets/OriginalDataFiles/PolicyMakingPowers/UpperHouseDoubleClusters.txt",sep="\t",header=TRUE)
ex_powers <-  read.table("Datasets/OriginalDataFiles/PolicyMakingPowers/ExecutiveClusters.txt",sep="\t",header=TRUE)

##Helper functions

mo_cong <- function(mean,sd){
  if(any(is.na(c(mean))))
    return(NA)
  samps <- rnorm(1500,mean[2],sd)
  sum((samps-mean[2])^2)/sum((samps-mean[1])^2)
}

winset <- function(sq, conv_hull){
  switch(findInterval(sq, sort(conv_hull)) + 1,
         c(sq, min(conv_hull)+abs(sq-min(conv_hull))),
         c(sq,sq),
         c(max(conv_hull)-abs(sq-max(conv_hull)), sq)
  )
}


## Preprocess data

AllMoods1990 <- read.csv("Datasets/FinishedOutput/AllMoodsFinal1990.csv")
AllMoods <- AllMoods1990 %>%
  select(type, country, year, mood1990=mood, Heterogeneity1990=Heterogeneity)
pm_pref <- subset(AllMoods,!type%in%c("Policy", "Citizen"))
pm_pref <- pm_pref[with(pm_pref,order(country, type, year)),]

##Import party data
PartyData <- read.table("Datasets/FinishedOutput/allMedianMoods.txt",
                        header = TRUE,
                        sep="\t")
pm_pref <- PartyData %>%
  select(country, year, legislative.median, prez.party.mood, median.party.mood) %>%
  mutate(country=dplyr::recode(country,"Brasil"="Brazil",
                        "RepublicaDominicana"="DominicanRep")) %>%
  right_join(pm_pref)

##Interpolation
pm_pref <- ddply(pm_pref, c("country","type"),
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


##Merge preferences institutions and policies, at the country-year level.
select.year <- function(years,y){
  dist.yr <- abs(years-y)[years<=y]
  if(length(dist.yr)==0)dist.yr <- Inf
  ifelse(min(dist.yr)>5,1800,max(years[years<=y]))
}
obtain.pref <- function(actor,data, ctry, yr){
  data <- subset(data,country==ctry&type==actor)
  best.yr <- select.year(data$year, yr)
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

full_data <- adply(policies,1, function(r,lh,uh,ex,pref){
  ctry <- r$country
  yr <- r$year
  lh_clust <- lh$lower.house.type[lh$country==ctry&yr>=lh$startyear&yr<=lh$endyear]
  r$LH_clust <- ifelse(length(lh_clust)==0,NA,paste("LH-",lh_clust,sep=""))
  uh_clust <- uh$upper.house.type[uh$country==ctry&yr>=uh$startyear&yr<=uh$endyear]
  r$UH_clust <- ifelse(length(uh_clust)==0,NA,paste("UH-",uh_clust,sep=""))
  e_clust <- ex$president.type[ex$country==ctry&yr>=ex$startyear&yr<=ex$endyear]
  r$E_clust <- ifelse(length(e_clust)==0,NA,as.character(e_clust))
  prefs_l <- obtain.pref("Legislator",pref,ctry,yr)
  r$LH_pref_const <- prefs_l[1]
  r$LH_pref_het_const <- prefs_l[2]
  r$LH_pref_spline <- prefs_l[3]
  r$LH_pref_het_spline <- prefs_l[4]
  r$ExecParty_pref_const <- prefs_l[5]
  r$MedianParty_pref_const <- prefs_l[6]
  r$ExecParty_pref_spline <- prefs_l[7]
  r$MedianParty_pref_spline <- prefs_l[8]
  prefs_u <- obtain.pref("Senator",pref,ctry,yr)
  r$UH_pref_const <- prefs_u[1]
  r$UH_pref_het_const <- prefs_u[2]
  r$UH_pref_spline <- prefs_u[3]
  r$UH_pref_het_spline <- prefs_u[4]
  r$L_pref_const <- mean(c(prefs_l[1], prefs_u[1]), na.rm=TRUE)
  r$L_pref_spline <- mean(c(prefs_l[3], prefs_u[3]),na.rm=TRUE)
  r$L_het_const <- sqrt(mean(c(prefs_l[2]^2,prefs_u[2]^2),na.rm=TRUE))
  r$L_het_spline <- sqrt(mean(c(prefs_l[4]^2,prefs_u[4]^2),na.rm=TRUE))
  prefs_e <- obtain.pref("President",pref,ctry,yr)
  r$E_pref_const <- prefs_e[1]
  r$E_pref_het_const <- prefs_e[2]
  r$E_pref_spline <- prefs_e[3]
  r$E_pref_het_spline <- prefs_e[4]
  return(r)
},
lh=lh_powers,uh=uh_powers,ex=ex_powers,pref=pm_pref)

## Assign process numbers
full_data <- subset(full_data, LH_clust!="<NA>")
full_data$ProcessName <- with(full_data, paste(LH_clust,UH_clust,E_clust,sep=":"))

all_nums <- c(8, 12, 1, 3, 6, 10, 9, 11, 2, 13, 7, 4, 5)
all_nums_collapsed <- c(5, 7, 1, 2, 4, 6, 5, 6, 1, 7, 4, 2, 3)
names(all_nums) <- unique(full_data$ProcessName)
names(all_nums_collapsed) <- unique(full_data$ProcessName)
full_data$ProcessNumberAll <- all_nums[(full_data$ProcessName)]
#num_key <- c("1" = 1 ,"2" = 1,"3" = 2,"4" = 2,"5" = 3,"6" = 4,
#             "7" = 4,"8" = 5,"9" = 5,"10" = 6,"11" = 6,"12" = 7,"13" = 7)
full_data$ProcessCollapsed <- all_nums_collapsed[full_data$ProcessName]

## Import controls
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

full_data <- merge(full_data,controls,all.x=TRUE)
full_data$decade <- as.factor(as.numeric(full_data$year) - (as.numeric(full_data$year)%%10))

## get lagged policy values
full_data <- full_data[with(full_data,order(country, year)),]
full_data$lag_policy <- ave(full_data$mood1990, full_data$country,
                            FUN=function(x){
                              c(NA,x[-length(x)])
                            })
full_data$ChangePolicy <- with(full_data, mood1990 - lag_policy)
full_data <- subset(full_data, !is.na(L_pref_const))

## get lagged mood values and compute change variables
full_data <- full_data[with(full_data, order(country, year)),]
full_data$lag_L_pref_const <- ave(full_data$L_pref_const, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeLPrefConst <- with(full_data, L_pref_const - lag_L_pref_const)
full_data$lag_ChangeLPrefConst <- ave(full_data$ChangeLPrefConst, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))

full_data$lag_L_pref_spline <- ave(full_data$L_pref_spline, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeLPrefSpline <- with(full_data, L_pref_spline - lag_L_pref_spline)
full_data$lag_ChangeLPrefSpline <- ave(full_data$ChangeLPrefSpline, full_data$country,
                                   FUN=function(x)c(NA, x[-length(x)]))

full_data$lag_E_pref_const <- ave(full_data$E_pref_const, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeEPrefConst <- with(full_data, E_pref_const - lag_E_pref_const)
full_data$lag_ChangeEPrefConst <- ave(full_data$ChangeEPrefConst, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))

full_data$lag_E_pref_spline <- ave(full_data$E_pref_spline, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeEPrefSpline <- with(full_data, E_pref_spline - lag_E_pref_spline)
full_data$lag_ChangeEPrefSpline <- ave(full_data$ChangeEPrefSpline, full_data$country,
                                   FUN=function(x)c(NA, x[-length(x)]))

full_data$lag_L_het_const <- ave(full_data$L_het_const, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeLHetConst <- with(full_data, L_het_const - lag_L_het_const)
full_data$lag_L_het_spline <- ave(full_data$L_het_spline, full_data$country,
                                  FUN=function(x)c(NA, x[-length(x)]))
full_data$ChangeLHetSpline <- with(full_data, L_het_spline - lag_L_het_spline)


## Compute congruence measures
full_data <- adply(full_data,1
                     ,function(x){
                       x$mo_cong_s_L <- mo_cong(c(x$mood1990,x$lag_L_pref_spline),x$lag_L_het_spline)
                       x$mo_cong_c_L <- mo_cong(c(x$mood1990,x$lag_L_pref_const),x$lag_L_het_const)
                       if(any(is.na(c(x$lag_E_pref_const, x$lag_L_pref_const)))){
                           x$winset_cong_c_B <- x$winset_incong_s_B <- NA
                       } else {
                           x$winset_cong_c_B <- findInterval(x$mood1990,
                                                               winset(x$lag_policy,
                                                                      c(x$lag_E_pref_const, x$lag_L_pref_const)),
                                                             rightmost.closed = TRUE
                                                               ) == 1
                           x$winset_cong_s_B <- findInterval(x$mood1990,
                                                               winset(x$lag_policy,
                                                                      c(x$lag_E_pref_spline, x$lag_L_pref_spline)),
                                                             rightmost.closed = TRUE
                                                               ) == 1
                       }
                       return(x)
                     })
full_data$dist_const <- with(full_data,abs(lag_E_pref_const-lag_L_pref_const))
full_data$dist_spline <- with(full_data,abs(lag_E_pref_spline-lag_L_pref_spline))
## Write data
write.csv(full_data,"Datasets/FinishedOutput/PowersPrefPolicies.csv"
          ,row.names=FALSE)
