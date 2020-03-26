##############################################
## Pooled joint McGann-GRM
## GR, February 2018
## Code to carry out goodness of fit tests
## based on moods from STAN blender run
##############################################

library (tidyverse)
library (gtools)
library (reshape2)
library (dplyr)
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
library (purrr)
library (abind)

graphicsPath <- "Graphs/"  # Save copies here as well

#### Comparing item parameters and policy orientations
load (file="Datasets/FinishedOutput/JointIRT_Flexible_1990.RData")

# Load MCMC object
chainsConv.joint <- rstan::As.mcmc.list(common_model)

load("Datasets/FinishedOutput/sharedItemParameterData.RData")

## Prepare data
finalData$ActorCtyYr <- as.integer(as.factor(with(finalData,
                                                  paste(type, country, year))))
finalData <- finalData[order(finalData$ActorCtyYr),]
finalData <- filter(finalData, year >= 1990)

##cit/pm-country-years
pmct <- finalData %>%
  filter(type!="Government")
pmct$CtyQuestID <- as.integer(as.factor(paste(pmct$country, pmct$QuestionName)))
country_question <- with(pmct,unique(cbind(country,QuestionName)))

##
##p-country-years
pct1 <- finalData %>%
  filter(type=="Government" &
           QuestionName %in% c("privatization.beneficial",
                               "capital.mobility.good")) %>%
  mutate_at("value", function(x)case_when(x < 3 ~ 0, TRUE ~ 1))

pct1 <- merge(pct1,subset(pmct, country%in%pct1$country & QuestionName%in%pct1$QuestionName, c(country, QuestionName,CtyQuestID)))

pct2 <- finalData %>%
  filter(type=="Government" &
           !(QuestionName %in% c("privatization.beneficial",
                                 "capital.mobility.good")))
pct2$CtyQuestID <- as.integer(as.factor(paste(pct2$country, pct2$QuestionName)))
country_question2 <- with(pct2,unique(cbind(country,QuestionName)))

# Indexes
ii = as.integer(as.factor(pmct$ActorCtyYr)) ##Select right actor and expand to N_acyq
qq_1 = pmct$CtyQuestID ##Select right country-question and expand to N_acyq
pp_1 = pct1$CtyQuestID ##Select right country-(policy/question) and expand to N_gyq
gg_1 = as.integer(as.factor(pct1$cy)) ##Select right government and expand to N_gyq
pp_2 = as.integer(as.factor(pct2$CtyQuestID[drop=TRUE])) ##Select right country-policy and expand to N_gyp
pp_3 = as.integer(as.factor(pct2$QuestionName[drop=TRUE])) ##Select right policy and expand to N_gyp
gg_2 = as.integer(as.factor(pct2$cy)) ##Select right government and expand to N_gyp
cc = as.integer(as.factor(country_question[,"country"])) ##Select right country and expand to N_cq
qq = as.integer(as.factor(country_question[,"QuestionName"])) ##Select right question and expand to N_cq)
cc_2 = as.integer(as.factor(country_question2[,"country"]))
pp = as.integer(as.factor(country_question2[,"QuestionName"]))

#############################################
#### Summaries of posterior distribution ####
#############################################
# 14 items
# We monitor the following parameters
# "mu_a": policy moods for all actors  # length:492
# (dealt with in 2017citizenMoodPlots.R and 2017politiciansPolicyMoods.R)
# "mu_g": policy orientations (dealt with in 2017policies.R) # length:540
# "sigma": heterogeneity of moods (polarization) # length:492  
# "nu_lq": hyperparameter for questions (lambda)  # length:28
# "lambda2": Discrimination parameters of all policy-items   # length:246
# "lambda": Discrimination parameters of all question-items (for actors)  # length:503
# "nu_lp": hyperparameter for policies (lambda2)  # length:14
# tau: cutpoints for GRM policies
# alpha: difficulty parameter of questions
# b: concentration parameter of beta function
# nu_lc: hyperparameters for countries (lambda)
# nu_l2c: hyperparameters for countries (lambda2)
# The "lp__" parameter floating around in the chains is a log Posterior measure

# Bridges are capital.mobility.good and privatization.beneficial, which are coded as aggregate percentages
# finalData$QuestionName contains all names of questions and policies

# Extract parameters
Tau <- rstan::extract (common_model, "tau")[[1]]
Alpha <- rstan::extract (common_model, "alpha")[[1]]
B <- rstan::extract (common_model, "b")[[1]]
logPosterior <- rstan::extract (common_model, "lp__")[[1]]
Polarization <- rstan::extract (common_model, "sigma")[[1]]
PolicyMoods  <- rstan::extract (common_model, "mu_a")[[1]]
PolicyOrient <- rstan::extract (common_model, "mu_g")[[1]]
Lambda.policies <- rstan::extract (common_model, "lambda2")[[1]]
Lambda.question <- rstan::extract (common_model, "lambda")[[1]]

policy.names <- c(levels (as.factor(pct2$QuestionName[drop=TRUE])),"kaopen","Privatize1")
question.names <- levels (as.factor(pct2$QuestionName))[1:28]
question.names[grep("capital.mobility.good", question.names)] <- "Capital openness/capital.mobility.good"
question.names[grep("privatization.beneficial", question.names)] <- "Privatization/privatization.beneficial"
order.question.names <- c(2:6,8:28,1,7)
policy.names.nice <- c("Reserve requirement", "Job termination cost"
                       , "Work flexibility", "Hiring flexibility", "Corporate tax"
                       , "Personal tax", "Financial transaction", "Average tariff"
                       , "Value added tax", "Trade dispersion", "Minimum wage"
                       , "Interest rate", "Privatizated assets", "Social security tax"
                       ,"Capital openness/capital.mobility.good","Privatization/privatization.beneficial")



###############################################################
#### Fitted values of Y for aggregate percentage responses ####
###############################################################
# Y[actor,country,question,year]

# Step 1: collect posterior distribution of shape parameters for beta distribution
# Equation 5.4
m <- pnorm((PolicyMoods[,ii] - Alpha[,qq_1]) / (sqrt (Lambda.question[,qq_1]^2 + Polarization[,ii]^2)))
# Equation 5.3
beta.sh <- apply (m, 2, function (x) B*x/(1-x))

# Step 2: Predictions for y: We need to draw from a beta distribution with shape1 beta.shape and shape2 B
# Function to draw from beta distribution
betaProbs <- function (a,b){
  y <- rbeta(length(a), a,  b)
  return (y)}
# Draws from beta distribution
Y.preds <- apply (beta.sh, 2, betaProbs, b=B)

# Draw sample for plotting purposes
buildData <- function (samp, output, predictor, tit) {
  Y.lo <- apply (predictor[,samp], 2, quantile, prob=0.1)
  Y.hi <- apply (predictor[,samp], 2, quantile, prob=0.9)
  Y    <- output[samp]
  covered <- ifelse (Y > Y.lo & Y < Y.hi, 1, 0) 
  pct.covered <- 100*(sum (covered)/length (Y))
  plot (Y, pch=19, ylim=c(0,1), col="red", main=paste0 (tit, "\n coverage=", round(pct.covered,2)))
  segments (x0=1:length(samp), x1=1:length(samp), y0=Y.lo, y1=Y.hi, lwd=1)
}

sample.r <- sort (base::sample (1:ncol(Y.preds), 300))
pdf (paste0(graphicsPath, "sampleCoverage.pdf"), h=7, w=10)
buildData (samp=sample.r, output=pmct$value, predictor=Y.preds, tit="Random sample (300 observations)")
dev.off()

# Plot coverage by collections of actors
sample.c <- c(1:nrow(pmct))[pmct$actor.number==1]
sample.l <- c(1:nrow(pmct))[pmct$actor.number==2]
sample.s <- c(1:nrow(pmct))[pmct$actor.number==4]
sample.p <- c(1:nrow(pmct))[pmct$actor.number==3]

par (mfrow=c(2,2))
buildData (samp=sample.c, output=pmct$value, predictor=Y.preds, tit="Citizens")
buildData (samp=sample.l, output=pmct$value, predictor=Y.preds, tit="Legislator")
buildData (samp=sample.s, output=pmct$value, predictor=Y.preds, tit="Senator")
buildData (samp=sample.p, output=pmct$value, predictor=Y.preds, tit="President")
par (mfrow=c(1,1))

# R2-like measures
y.bar <- mean (pmct$value)
point.estimates <- apply (Y.preds, 2, median) #median is probably better than mean
sampling.error  <- pmct$value - point.estimates
res.sum.squares <- sum ((pmct$value-point.estimates)^2)
reg.sum.squares <- sum ((point.estimates - y.bar)^2)
tot.sum.squares <- sum ((pmct$value-y.bar)^2)
r2 <- reg.sum.squares/tot.sum.squares  
print (r2)


#########################################
#### Fitted values of Y for policies ####
#########################################
# Y[actor,country,question,year]

z1 <- pct1$value #outcome percentage policies
z2 <- pct2$value #outcome ordered categorical policies

eta_1 <- Lambda.question[,pp_1] * (PolicyOrient[,gg_1] - Alpha[,pp_1])
eta_2 <- Lambda.policies[,pp_2] * PolicyOrient[,gg_2]

# Bernoulli part
tmp.Y <- rbernoulli(length(eta_1), gtools::inv.logit(eta_1))*(1)
prop.Y <- apply (tmp.Y, 2, function (x) sum(x)/length(x))
modal.predicted <- length (z1[z1==median(z1)])
modal.prediction <- modal.predicted/length(z1)
predicted.z1 <- ifelse (prop.Y>0.5, 1, 0)
correct.pred <- ifelse (predicted.z1==z1, 1, 0)
adj.count.r2 <- (sum(correct.pred) - modal.predicted) / (length(z1) - modal.predicted)   # Long, p.107
r2.tjur <- mean (prop.Y[z1==1]) -  mean (prop.Y[z1==0]) #https://statisticalhorizons.com/r2logistic

# Ordered category part
Pr1 <- 1 - gtools::inv.logit(eta_2 - Tau[,pp_3,1]) 
Pr2 <- gtools::inv.logit(eta_2 - Tau[,pp_3,1]) - gtools::inv.logit(eta_2 - Tau[,pp_3,2]) 
Pr3 <- gtools::inv.logit(eta_2 - Tau[,pp_3,2]) - gtools::inv.logit(eta_2 - Tau[,pp_3,3]) 
Pr4 <- gtools::inv.logit(eta_2 - Tau[,pp_3,3]) 

prop.Pr1 <- apply (Pr1, 2, function (x) sum(x)/length(x))
prop.Pr2 <- apply (Pr2, 2, function (x) sum(x)/length(x))
prop.Pr3 <- apply (Pr3, 2, function (x) sum(x)/length(x))
prop.Pr4 <- apply (Pr4, 2, function (x) sum(x)/length(x))

#Tjur-like means: These numbers better be high, hopefully larger than 0.5, certainly larger than the sample frequency of the result
mean (prop.Pr1[z2==1]); length(z2[z2==1])/length(z2)
mean (prop.Pr2[z2==2]); length(z2[z2==2])/length(z2)
mean (prop.Pr3[z2==3]); length(z2[z2==3])/length(z2)
mean (prop.Pr4[z2==4]); length(z2[z2==4])/length(z2)

# Confusion matrix
Prop.Pr <- cbind (prop.Pr1, prop.Pr2, prop.Pr3, prop.Pr4)
pred.Cat <- apply (Prop.Pr, 1, which.max)
ftable (z2, pred.Cat)
correct.pred.cat <- sum (diag(ftable (z2, pred.Cat))) / length (z2)
modal.predicted.cat <- length (z2[z2==3])
