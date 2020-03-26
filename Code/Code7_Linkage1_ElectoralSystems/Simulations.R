########################
## Code to conduct
## simulations of electoral
## system strength and 
## classify using regression
## tree.
#########################


library(plyr)
library(Rcpp)
library(doMC)
library(rpart)
library(caret)
library(tidyverse)


weighted.quantile <- function(x, w, probs=seq(0,1,0.25), na.rm=TRUE) {
  x <- as.numeric(as.vector(x))
  w <- as.numeric(as.vector(w))
  if(anyNA(x) || anyNA(w)) {
    ok <- !(is.na(x) | is.na(w))
    x <- x[ok]
    w <- w[ok]
  }
  stopifnot(all(w >= 0))
  if(all(w == 0)) stop("All weights are zero", call.=FALSE)
  #'
  oo <- order(x)
  x <- x[oo]
  w <- w[oo]
  Fx <- cumsum(w)/sum(w)
  #'
  result <- numeric(length(probs))
  for(i in seq_along(result)) {
    p <- probs[i]
    lefties <- which(Fx <= p)
    if(length(lefties) == 0) {
      result[i] <- x[1]
    } else {
      left <- max(lefties)
      result[i] <- x[left]
      if(Fx[left] < p && left < length(x)) {
        right <- left+1
        y <- x[left] + (x[right]-x[left]) * (p-Fx[left])/(Fx[right]-Fx[left])
        if(is.finite(y)) result[i] <- y
      }
    }
  }
  names(result) <- paste0(format(100 * probs, trim = TRUE), "%")
  return(result)
}


weighted.median <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.5, w=w, na.rm=na.rm))
}

source("Code/AuxFunctions/CVFunction.R")


cppFunction("
Rcpp::NumericMatrix voting(Rcpp::NumericMatrix parties, Rcpp::NumericMatrix voters, int nvotes, double strat){
  int n_parties = parties.nrow();
  int n_sims = parties.ncol();
  int n_voters = voters.nrow();
  Rcpp::NumericMatrix votes(n_parties,n_sims);
  Rcpp::NumericVector dists(n_parties);
  int vote_party;
  for(int sim = 0; sim < n_sims; ++sim){
    for(int voter = 0; voter < n_voters; ++voter){
      double ru = R::runif(0,1);
      dists = abs(parties(_,sim)-voters(voter,sim));
      if(strat < ru)
        dists(which_min(dists)) = 1e6;
      for(int vote = 0; vote < nvotes; ++vote){
        vote_party = which_min(dists);
         ++votes(vote_party,sim);
        //dists(vote_party) = 1e6;
      }
    }
  }
  return(votes);
}
")

cppFunction("
Rcpp::NumericVector voting_one(Rcpp::NumericVector parties, Rcpp::NumericVector voters){
  int n_parties = parties.size();
  int n_voters = voters.size();
  Rcpp::NumericVector votes(n_parties);
  Rcpp::NumericVector dists(n_parties);
  int vote_party;
    for(int voter = 0; voter < n_voters; ++voter){
      dists = abs(parties-voters(voter));
      vote_party = which_min(dists);
      ++votes(vote_party);
    }
  return(votes);
  }
")

cppFunction("
Rcpp::NumericVector disp(Rcpp::NumericMatrix votes, Rcpp::NumericMatrix seats){
  int n_sims = votes.ncol();
  Rcpp::NumericVector disp(n_sims);
  for(int sim = 0; sim < n_sims; ++sim){
    disp(sim) = sqrt(0.5*sum(pow(votes(_,sim)-seats(_,sim),2)));
  }
  return(disp);
}
")

sim.fun <- function(dm, thresh, strat, sa.formula, n.parties,n.voters, n.votes, pres){
  set.seed(831213)
  parties <- replicate(1000,runif(n.parties))
  voters <- replicate(1000,runif(n.voters))
  party.votes <- prop.table(voting(parties,voters,n.votes, strat),2)
  if(pres){
    party.votes[party.votes>=thresh] <- 1
  }
  else {
    party.votes[party.votes<thresh] <- 0
  }
  if(any(c("majority","super-plurality")==sa.formula)){
    party.seats <- prop.table(get(sa.formula)(party.votes,dm,thresh,parties,voters),2)
  }else{
    party.seats <- prop.table(get(sa.formula)(party.votes,dm),2)
  }
  gall.disp <- disp(party.votes,party.seats)
  #congruence <- cong()
  return(mean(gall.disp))
}

sim.fun.all <- function(dm, thresh.l, thresh.p, strat, sa.formula.l, sa.formula.p, n.parties, n.voters, pres.pow){
  ks_cong <- function(mean,sd){
    if(any(is.na(c(mean))))
      return(NA)
    if(any(is.na(sd)))
      sd[is.na(sd)] <- 0.5
    samp1 <- rnorm(1000, mean[1], sd[1])
    samp2 <- rnorm(1000, mean[2], sd[2])
    1/(1+ks.test(samp1,samp2)$statistic)
    #1/sum(abs(pnorm(ind,mean[1],sd[1])-pnorm(ind,mean[2],sd[2])))
  }

  mo_cong <- function(mean,sd){
    if(any(is.na(c(mean))))
      return(NA)
    if(is.na(sd)){
      sd  <- 0.5
    }
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
  winset_cong <- function(current, sq, mood_e, mood_l){
    findInterval(current,
                 winset(sq,
                        c(mood_e, mood_l)),
                 rightmost.closed = TRUE) == 1
  }
  set.seed(831213)
  parties.pres <- replicate(1000, rnorm(n.parties, 1.51, 5.48))
  parties.leg <- replicate(1000, rnorm(n.parties, 0.70, 1.53))
  voters <- replicate(1000, rnorm(n.voters, 0.55, 2.04))
  party.votes.l <- prop.table(voting(parties.leg,voters,1, strat),2)
  party.votes.p <- prop.table(voting(parties.pres,voters,1, strat),2)
  party.votes.l[party.votes.l<thresh.l] <- 0
  party.votes.p[party.votes.p>=thresh.p] <- 1
  if(any(c("majority","super-plurality")==sa.formula.l)){
    party.seats.l <- prop.table(get(sa.formula.l)(party.votes.l,dm,thresh.l,parties.l,voters),2)
  }else{
    party.seats.l <- prop.table(get(sa.formula.l)(party.votes.l,dm),2)
  }
  if(any(c("majority","super-plurality")==sa.formula.p)){
    party.seats.p <- prop.table(get(sa.formula.p)(party.votes.p,1,thresh.p,parties.pres,voters),2)
  }else{
    party.seats.p <- prop.table(get(sa.formula.p)(party.votes.p,1),2)
  }
  policy <- array(NA, 1000)
  mm_cong_cpl <- array(NA, 1000)
  oo_cong_cpl <- array(NA, 1000)
  mo_cong_cpl <- array(NA, 1000)
  oo_cong_cpp <- array(NA, 1000)
  mo_cong_cpp <- array(NA, 1000)
  mo_cong_lp <- array(NA, 1000)
  winset_cong_vec <- array(NA, 1000)
  oo_cong_cp <- array(NA, 1000)
  mo_cong_cp <- array(NA, 1000)

  delta_l <- 0
  delta_e <- 0
  delta_c <- 0
  delta_p <- 0
  past_l <- 0
  past_c <- 0
  past_e <- 0
  past_p <- 0
  resp_lc <- array(NA, 1000)
  resp_ec<- array(NA, 1000)
  resp_pc<- array(NA, 1000)
  resp_pl <- array(NA, 1000)
  resp_pe<- array(NA, 1000)
  for(i in 1:1000){
    mean_p <- mean(parties.pres[party.seats.p[,i]>0, i], na.rm=TRUE)
    mean_l <- mean(parties.leg[party.seats.l[,i]>0, i],na.rm=TRUE)
    sd_l <- sd(parties.leg[party.seats.l[,i]>0, i], na.rm=TRUE)
    mean_c <- mean(voters[,i],na.rm=TRUE)
    sd_c <- sd(voters[,i],na.rm=TRUE)
    policy[i] <- weighted.median(parties.leg[,i], party.seats.l[,i])*(1-pres.pow) + sum(parties.pres[,i] * party.seats.p[,i])*pres.pow
    if(i>1){
      delta_l <- mean_l - past_l
      past_l <- mean_l
      delta_c <- mean_c - past_c
      past_c <- mean_c
      delta_e <- mean_p - past_e
      past_e <- mean_p
      delta_p <- policy[i] - past_p
      winset_cong_vec[i] <- winset_cong(policy[i], past_p, mean_p, mean_l)
      past_p <- policy[i]
      resp_lc[i] <- sign(delta_l*delta_c)>0
      resp_ec[i] <- sign(delta_e*delta_c)>0
      resp_pc[i] <- sign(delta_p*delta_c)>0
      resp_pl[i] <- sign(delta_p*delta_l)>0
      resp_pe[i] <- sign(delta_p*delta_e)>0
    }


    mm_cong_cpl[i] <- ks_cong(c(mean_l, mean_c), c(sd_l, sd_c))
    mo_cong_cpl[i] <- mo_cong(c(mean_l, mean_c),sd_c)
    oo_cong_cpl[i] <- 1/(1+abs(mean_l-mean_c))
    mo_cong_cpp[i] <- mo_cong(c(mean_p, mean_c),sd_c)
    oo_cong_cpp[i] <- 1/(1+abs(mean_p-mean_c))
    mo_cong_lp[i] <- mo_cong(c(policy[i], mean_l),sd_l)
    oo_cong_cp[i] <- 1/(1+abs(policy[i]-mean_c))
    mo_cong_cp[i]  <- mo_cong(c(policy[i], mean_c),sd_c)


  }
  return(c(mo_cong_cpl=mean(mo_cong_cpl),
           mm_cong_cpl=mean(mm_cong_cpl),
           oo_cong_cpl=mean(oo_cong_cpl),
           mo_cong_cpp=mean(mo_cong_cpp),
           oo_cong_cpp=mean(oo_cong_cpp),
           mo_cong_lp=mean(mo_cong_lp),
           mo_cong_cp=mean(mo_cong_cp),
           oo_cong_cp=mean(oo_cong_cp),
           winset_cong = mean(winset_cong_vec, na.rm=TRUE),
           resp_lc=mean(resp_lc, na.rm=TRUE),
           resp_ec=mean(resp_ec, na.rm=TRUE),
           resp_pc=mean(resp_pc, na.rm=TRUE),
           resp_pl=mean(resp_pl, na.rm=TRUE),
           resp_pe=mean(resp_pe, na.rm=TRUE)
           ))
}

####### Formulas #######
majority <- function(v,m,thresh,parties,voters){
  n.sim <- ncol(v)
  n.parties <- nrow(v)
  sapply(1:n.sim,function(x){
    seats <- rep(0,n.parties)
    if(any(v[,x]>thresh)){
      seats[which(v[,x]>thresh)] <- m
    }else{
      top2 <- order(-v[,x])[1:2]
      top_parties <- parties[top2,x]
      new_v <- voting_one(top_parties,voters[,x])
      seats[top2[which.max(new_v)]] <- m
    }
    return(seats)
  })
}

limited_nom <- function(v,m){
  # In this system, the top vote-getter gets all seats minus 1
  # The first loser gets 1 seat
  findSeats <- function (x) {
    seats <- rep (0, length(x))
    seats[order (-x)[1]] <- m-1
    seats[order (-x)[2]] <- 1
    return (seats)
  }
  dist.seats <- apply (v, 2, findSeats)
  return (dist.seats)
}

dhondt <- function(v,m){
  # m is district magnitude
  # which.party is a vector of length m*nrow(v), a placeholder for
  # information about which party will win a seat
  which.party <- rep (1:nrow (v), each=m)
  findSeats <- function (x) {
    # winners counts the number of seats that each winning party obtains
    # in a district of magnitude m. The rule is d'hondt, which awards
    # seats based on the size of quotients calculated as:
    # V / m
    # where V is total number of votes and m is district magnitude
    winners <- table (which.party[order (-sapply (x, function (x) x/1:m))[1:m]])
    seats <- rep (0, length (x))
    winner.pos <- as.numeric (names (winners))
    # seats is a vector that contains the number of seats won by each party
    seats[winner.pos] <- winners
    return (seats)
  }
  dist.seats <- apply (v, 2, findSeats)
  return (dist.seats)
}

hare <- function(v,m){
  # this snippet uses hare with largest remainders (as opposed to average remainders)
  # m is district magnitude
  findSeats <- function (x) {
    quota <- x/(1/m) # Hare quota is 1/m, where 1 is total votes
    quota.integer <- floor (quota)  # Seats immediately awarded
    quota.remainder <- quota - quota.integer # largest remainders
    remaining.seats <- m - sum(quota.integer) # total seats awarded to remainders
    remaining.seats.winners <- order (-quota.remainder)[1:remaining.seats] # winners of remainders
    extra.seats <- rep (0, length(x))
    extra.seats[remaining.seats.winners] <- 1
    seats <- quota.integer + extra.seats  # total seats
    return (seats)
  }
  dist.seats <- apply (v, 2, findSeats)
  return (dist.seats)
}

sainte_lague <- function(v,m){
  # m is district magnitude
  # which.party is a vector of length m*nrow(v), a placeholder for
  # information about which party will win a seat
  which.party <- rep (1:nrow (v), each=m+1)
  findSeats <- function (x) {
    # winners counts the number of seats that each winning party obtains
    # in a district of magnitude m. The rule is saint_lauge, which awards
    # seats based on the size of quotients calculated as:
    # V / 2m+1
    # where V is total number of votes and m is district magnitude
    winners <- table (which.party[order (-sapply (x, function (x) {x/((0:m)*2 + 1)}))[1:m]])
    seats <- rep (0, length (x))
    winner.pos <- as.numeric (names (winners))
    # seats is a vector that contains the number of seats won by each party
    seats[winner.pos] <- winners
    return (seats)
  }
  dist.seats <- apply (v, 2, findSeats)
  return (dist.seats)
}


plurality <- function(v,m){
  winner <- apply(v,2,which.max) # all seats in district go to winner
  winner <- factor(winner,levels=1:nrow(v))
  seats <- t(model.matrix(~winner-1))
  rownames(seats) <- colnames (seats) <- NULL  # clean up names and attributes
  attr (seats, "assign") <- attr (seats, "contrasts") <- NULL
  return (seats*m) # return clean matrix
}

####### Simulations
registerDoMC(48)

##Entire chain
hyp.data.full <- expand.grid(dm=c(1:10,100,120)
                            ,thresh.l = c(0.05,0.01,0.02)
                            ,thresh.p = c(0.5)
                            ,sa.formula.l = c("dhondt"
                                           ,"hare"
                                            ,"limited_nom"
                                           ,"plurality"
                                            ,"sainte_lague"
                            )
                            ,sa.formula.p = c("plurality"
                                              ,"majority"
                            )
                            ,n.parties = c(2,3,4,5,10,15)
                            ,strat=seq(1,0.5, length.out=6)
                            ,pres.pow=seq(0.25, 0.75, length.out = 5)
                            ,stringsAsFactors = FALSE)
hyp.data.full <- hyp.data.full[with(hyp.data.full,!((sa.formula.l=="dhondt"
                                                  |sa.formula.l=="hare"
                                                  |sa.formula.l=="sainte_lague")&(thresh.l>0.05|dm<2)
                                                 |(sa.formula.l=="limited_nom"&(dm<3|thresh.l>0))
                                                 |(sa.formula.l=="majority")&(thresh.l<0.45))),]


sim.results.full <- maply(hyp.data.full,sim.fun.all
                                  ,n.voters=5000
                                  ,.expand=FALSE
                                  ,.parallel=TRUE)

hyp.data.full <- cbind(hyp.data.full, sim.results.full)
save(hyp.data.full, file="Datasets/FinishedOutput/SimulationResultsFUllChain.RData")
load("Datasets/FinishedOutput/SimulationResultsFUllChain.RData")

hyp.data.full.long <- hyp.data.full %>%
  gather(Variable, val, mo_cong_cpl:resp_pe)
##Citizen to PM
#Congruence
hyp.data.full.long.cpm_cong <- hyp.data.full.long %>%
  filter(Variable %in% c("mm_cong_cpl", "mo_cong_cpl", "oo_cong_cpl", "mo_cong_cpp","oo_cong_cpp")) %>%
  mutate(Variable = case_when(Variable=="mm_cong_cpl" ~ "Distribution-to-Distribution_Legislature",
                              Variable=="mo_cong_cpl" ~ "Distribution-to-Median_Legislature",
                              Variable=="oo_cong_cpl" ~ "Median-to-Median_Legislature",
                              Variable=='mo_cong_cpp' ~ "Distribution-to-Median_Executive",
                              Variable=="oo_cong_cpp" ~ "Median-to-Median_Executive"
                                )) %>%
  separate(Variable, c("Measure", "Branch"), sep = "_") %>%
  droplevels()
pdf("Graphs/SimCongCitPM.pdf", width=10, height=3.5)
ggplot(hyp.data.full.long.cpm_cong, aes(x=Measure, y = val)) +
  facet_grid(~Branch, scales="free_x")+
  geom_boxplot(outlier.color = NA) +
  xlab("Congruence Measure") +
  ylab("")+
  theme_bw()
dev.off()
#Responsiveness
hyp.data.full.long.cpm_resp <- hyp.data.full.long %>%
  filter(Variable %in% c("resp_lc", "resp_ec")) %>%
  mutate(Branch = case_when(Variable=="resp_lc" ~ "Responsiveness: Legislature",
                              Variable=="resp_ec" ~ "Responsiveness: Executive"
  )) %>%
  droplevels()
pdf("Graphs/SimRespCitPM.pdf", width=10, height=3.5)
ggplot(hyp.data.full.long.cpm_resp, aes(x=Branch, y = val)) +
  geom_boxplot(outlier.color = NA) +
  ylab("Probability of moving in same direction") +
  xlab("")+
  theme_bw()
dev.off()

##PM to policy
#Congruence
hyp.data.full.long.pmp_cong <- hyp.data.full.long %>%
  filter(Variable %in% c("mo_cong_lp", "winset_cong")) %>%
  mutate(Variable = case_when(Variable=="mo_cong_lp" ~ "Congruence: Distribution-to-Policy_Legislature",
                              Variable=="winset_cong" ~ "Congruence: Winset_Both"
  )) %>%
  separate(Variable, c("Measure", "Branch"), sep = "_") %>%
  droplevels()
pdf("Graphs/SimCongPMPol.pdf", width=8, height=3.5)
ggplot(hyp.data.full.long.pmp_cong, aes(x=Measure, y = val)) +
  facet_grid(~Branch, scales="free_x")+
  geom_boxplot(outlier.color = NA) +
  xlab("") +
  ylab("")+
  ylim(c(0,1))+
  theme_bw()
dev.off()
#Responsiveness
hyp.data.full.long.pmp_resp <- hyp.data.full.long %>%
  filter(Variable %in% c("resp_pl", "resp_pe")) %>%
  mutate(Branch = case_when(Variable=="resp_pl" ~ "Legislature",
                            Variable=="resp_pe" ~ "Executive"

  )) %>%
  droplevels()
pdf("Graphs/SimRespPMPol.pdf", width=6, height=3.5)
ggplot(hyp.data.full.long.pmp_resp, aes(x=Branch, y = val)) +
  geom_boxplot(outlier.color = NA) +
  ylab("Probability of policy moving in same direction") +
  xlab("")+
  theme_bw()+
  ylim(c(0.2, 1.0))
dev.off()

##Citizen to policy
#Congruence
hyp.data.full.long.cp_cong <- hyp.data.full.long %>%
  filter(Variable %in% c("mo_cong_cp", "oo_cong_cp","resp_pc")) %>%
  mutate(Variable = case_when(Variable=="mo_cong_cp" ~ "Congruence: Distribution-to-Policy",
                              Variable=="oo_cong_cp" ~ "Congruence: Median-to-Policy",
                              Variable=="resp_pc" ~ "Simulated Responsiveness"
                              )) %>%
  droplevels()
write.csv(hyp.data.full.long.cp_cong, file = "CitizenToPolicySim.csv", row.names = FALSE)
pdf("Graphs/SimCongCPol.pdf", width=5, height=3.5)
ggplot(subset(hyp.data.full.long.cp_cong, Variable!="Simulated Responsiveness"), aes(x=Variable, y = val)) +
  geom_boxplot(outlier.color=NA) +
  xlab("") +
  ylab("")+
  theme_bw()
dev.off()

pdf("Graphs/SimRespCPol.pdf", width=4, height=3.5)
ggplot(subset(hyp.data.full.long.cp_cong, Variable=="Simulated Responsiveness"), aes(x=Variable, y = val)) +
  geom_boxplot(outlier.color = NA) +
  xlab("") +
  ylab("")+
  ylim(c(0.27, 0.65))+
  theme_bw()
dev.off()



 
## Clustering using trees
hyp.data.leg.mean <- ddply(hyp.data.leg,c("dm","thresh","sa.formula","n.votes"),.fun=function(x)c(avg=mean(x$sim.results)))
hyp.data.leg.mean$elec_family <- with(hyp.data.leg.mean,ifelse(sa.formula%in%c("dhondt", "hare","saint_lague", "limited_nom"), "PR"
                                               ,ifelse(sa.formula=="plurality"&dm>1,"Other","Plurality/Majority")))
hyp.data.leg.mean$sa.formula <- with(hyp.data.leg.mean,ifelse(sa.formula=="plurality"&thresh>0,"super-plurality", sa.formula))
hyp.data.leg.mean$sa.formula <- as.factor(hyp.data.leg.mean$sa.formula)
set.seed(831213)
tot_obs <- nrow(hyp.data.leg.mean)
rpart.grid <- list(cp=seq(0.001,0.2,length.out=500)
                   ,minbucket=round(c(0.11*tot_obs, 0.16*tot_obs, 0.21*tot_obs)))
clust.cv <- cross.val( hyp.data.leg.mean
                       ,hyp.data.leg.mean
                       ,.formula=avg~dm+thresh+sa.formula+elec_family
                       ,.model="rpart"
                       ,.params=rpart.grid
                       ,.folds=15
                       ,.predict=FALSE
                       ,.parallel=TRUE
                       ,.n.cores=22
)
clust.model <- rpart(avg~dm+thresh+sa.formula+elec_family
                     ,data=hyp.data.leg.mean
                     ,control=clust.cv$mod.res)
clust.model$variable.importance

pdf("Graphs/Classification.pdf")
rpart.plot(clust.model)
dev.off()
save(clust.model,file="Datasets/FinishedOutput/Classifier.RData")
