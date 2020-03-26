##############################################
## Pooled joint McGann-GRM
## GR, December 2017
## Plots a few more graphs for item parameters
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

finalData$QuestionID <- as.integer(as.factor(with(finalData,
                                                  paste(country, QuestionName))))


##cit/pm-country-years
pmct <- finalData %>%
   filter(type!="Government")
pmct$CtyQuestID <- as.integer(as.factor(paste(pmct$country, pmct$QuestionName)))
country_question <- with(pmct,unique(cbind(country,QuestionName)))

unique(pmct$CtyQuestID)

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





# Summaries of posterior distribution
# 14 items
# We monitor the following parameters
# "mu_a": policy moods for all actors (dealt with in 2017citizenMoodPlots.R and 2017politiciansPolicyMoods.R)
# "mu_g": policy orientations (dealt with in 2017polices.R)
# "sigma": heterogeneity of moods (polarization)
# "nu_lq": hyperparameter for questions (lambda)
# "lambda2": Discrimination parameters of all policy-items
# "lambda": Discrimination parameters of all question-items (for actors)
# "nu_lp": hyperparameter for policies (lambda2)

# We did not monitor (but could, if needed)
# tau: cutpoints for GRM policies
# alpha: difficulty parameter of questions
# b: concentration parameter of beta function
# nu_lc: hyperparameters for countries (lambda)
# nu_l2c: hyperparameters for countries (lambda2)

# Bridges are capital.mobility.good and privatization.beneficial, which are coded as aggregate percentages (how?)
# finalData$QuestionName contains all names of questions and policies


All.Lambda <- rbind ( chainsConv.joint[[1]][,grep("lambda", colnames(chainsConv.joint[[1]]))]
                   , chainsConv.joint[[2]][,grep("lambda", colnames(chainsConv.joint[[2]]))]
                   , chainsConv.joint[[3]][,grep("lambda", colnames(chainsConv.joint[[3]]))]
                   , chainsConv.joint[[4]][,grep("lambda", colnames(chainsConv.joint[[4]]))]
                   , chainsConv.joint[[5]][,grep("lambda", colnames(chainsConv.joint[[5]]))])
                  

Lambda.policies <- All.Lambda[,grep ("lambda2", colnames(All.Lambda))]
Lambda.question <- All.Lambda[,-grep ("lambda2", colnames(All.Lambda))]


median (colMeans (Lambda.question))

Nu.policies <- rbind ( chainsConv.joint[[1]][,grep("nu_lp", colnames(chainsConv.joint[[1]]))]
                      , chainsConv.joint[[2]][,grep("nu_lp", colnames(chainsConv.joint[[2]]))]
                      , chainsConv.joint[[3]][,grep("nu_lp", colnames(chainsConv.joint[[3]]))]
                      , chainsConv.joint[[4]][,grep("nu_lp", colnames(chainsConv.joint[[4]]))]
                      , chainsConv.joint[[5]][,grep("nu_lp", colnames(chainsConv.joint[[5]]))])
                      

Nu.question <- rbind ( chainsConv.joint[[1]][,grep("nu_lq", colnames(chainsConv.joint[[1]]))]
                       , chainsConv.joint[[2]][,grep("nu_lq", colnames(chainsConv.joint[[2]]))]
                       , chainsConv.joint[[3]][,grep("nu_lq", colnames(chainsConv.joint[[3]]))]
                       , chainsConv.joint[[4]][,grep("nu_lq", colnames(chainsConv.joint[[4]]))]
                       , chainsConv.joint[[5]][,grep("nu_lq", colnames(chainsConv.joint[[5]]))])
                       

Nu.policies.exp <- cbind (Nu.policies, Nu.question[,c(1,7)])

policy.names <- c(levels (as.factor(pct2$QuestionName[drop=TRUE])),"kaopen","Privatize1")
question.names <- levels (as.factor(pct2$QuestionName))[1:28]
policy.names.nice <- c("Reserve requirement", "Job termination cost"
                       , "Work flexibility", "Hiring flexibility", "Corporate tax"
                       , "Personal tax", "Financial transaction", "Average tariff"
                       , "Value added tax", "Trade dispersion", "Minimum wage"
                       , "Interest rate", "Privatizated assets", "Social security tax"
                       ,"Capital openness/capital.mobility.good","Privatization/privatization.beneficial")

# Plot policy-specific discrimination hyperparameters
pdf (paste0 (graphicsPath, "policyDiscriminationHyperparameters.pdf"), h=5, w=7)
par (mar=c(14,4,1,1), las=2, mfrow=c(1,1))
plot (c(1,ncol(Nu.policies.exp)), c(0,5)
      , type="n", axes=F, ylab="", xlab="")
axis (1, at=1:ncol(Nu.policies.exp), labels=NA)
axis (2)
abline (h=1, lty=2)
mtext (policy.names.nice, side=1, line=1
       , at=1:ncol(Nu.policies.exp)
       , font=c(rep(1,(length(policy.names.nice)-2)),2,2)
       , cex=0.8)
points (xy.coords(1:ncol(Nu.policies.exp), colMeans(Nu.policies.exp))
        , pch=19, col="black")
segments (x0=1:ncol(Nu.policies.exp), x1=1:ncol(Nu.policies.exp)
          , y0=apply (Nu.policies.exp, 2, quantile, prob=0.25)
          , y1=apply (Nu.policies.exp, 2, quantile, prob=0.75)
         , lwd=3, col="black")
mtext ("Value of policy discrimination", side=2, line=3, las=0)
mtext (expression (paste ("hyperparameters (", nu[eta],")")) , side=2, line=1.8, las=0)
dev.off()

question.names[grep("capital.mobility.good", question.names)] <- "Capital openness/capital.mobility.good"
question.names[grep("privatization.beneficial", question.names)] <- "Privatization/privatization.beneficial"
order.question.names <- c(2:6,8:28,1,7)

# Plot question specific discrimination hyperparameters
pdf (paste0 (graphicsPath, "questionDiscriminationHyperparameters.pdf"), h=5, w=7)
par (mar=c(14,3,1,1), las=2, mfrow=c(1,1))
plot (c(1,ncol(Nu.question)), c(0,7)
      , type="n", axes=F, ylab="", xlab="")
axis (1, at=1:ncol(Nu.question), labels=NA)
axis (2)
abline (h=1, lty=2)
mtext (question.names[order.question.names]
       , side=1, line=1, at=1:ncol(Nu.question)
       , font=c(rep(1,(length(question.names)-2)),2,2)
       , cex=0.8
       , col="black")
points (xy.coords(1:ncol(Nu.question), colMeans(Nu.question[,order.question.names])), pch=19)
segments (x0=1:ncol(Nu.question), x1=1:ncol(Nu.question)
          , y0=apply (Nu.question, 2, quantile, prob=0.25)[order.question.names]
          , y1=apply (Nu.question, 2, quantile, prob=0.75)[order.question.names], lwd=3)
dev.off()

pdf (paste0 (graphicsPath, "BlenderItemDiscriminationLegislatorPreferences.pdf"), h=10, w=7)
item.no <- as.integer(as.factor(country_question[,"QuestionName"]))
item.no <- item.no
par (mar=c(2,14,1,1), las=1, mfrow=c(1,1))
plot (c(0, 12), c(0, length(question.names)+1)
      , type="n", axes=F, ylab="")
axis (1)
abline (v=c(0,1,5), lty=2)
mtext (question.names[order.question.names]
       , side=2, at=1:length(question.names)
       , font=c(rep(1,(length(question.names)-2)),2,2), cex=0.8)
for (i in 1:length(question.names)){
   xvec <- Lambda.question[,item.no==order.question.names[i]]
   points (xy.coords(apply(xvec, 2, median), rep(i,ncol(xvec))), pch=19, col="gray")
   points (xy.coords(median(apply(xvec, 2, median)), i), pch=19, col="black", cex=1.2)
   segments (x0=quantile(apply(xvec, 2, median), 0.25), x1=quantile(apply(xvec, 2, median), 0.75), y0=i, y1=i, lwd=3)
}
dev.off()

allpoints <- c()
for (i in 1:length(question.names)){
  xvec <- apply (Lambda.question[,item.no==order.question.names[i]], 2, median)
  allpoints <- c(allpoints, xvec)
}

