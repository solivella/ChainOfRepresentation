##########################
##########################
##  Joint IRT
##  moods and policies
##  in Latin America
##########################
##########################

library(tidyverse)
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

##Form list
data_list <- list( K = 4,
                   N_acy = length(unique(pmct$ActorCtyYr)), ##Nr. of actor-country-years
                   N_gy = length(unique(pct1$cy)), ##Nr. of government-years
                   N_cq = length(unique(pmct$CtyQuestID)), ##Nr. of country-questions
                   N_cp = length(unique(pct2$CtyQuestID)), ##Nr. of country-(nonquestion)questions
                   N_c = length(unique(country_question[,"country"])),
                   N_q = length(unique(country_question[,"QuestionName"])),
                   N_p = length(unique(pct2$QuestionName)), ##Nr. of (non-question) policies
                   N_acyq = nrow(pmct), ##Nr. of actor-country-year-questions
                   N_gyq = nrow(pct1), ##Nr. of government-year-questioins
                   N_gyp = nrow(pct2), ##Nr. of government-year-policies
                   ii = as.integer(as.factor(pmct$ActorCtyYr)), ##Select right actor and expand to N_acyq
                   qq_1 = pmct$CtyQuestID, ##Select right country-question and expand to N_acyq
                   pp_1 = pct1$CtyQuestID, ##Select right country-(policy/question) and expand to N_gyq
                   gg_1 = as.integer(as.factor(pct1$cy)), ##Select right government and expand to N_gyq
                   pp_2 = as.integer(as.factor(pct2$CtyQuestID[drop=TRUE])), ##Select right country-policy and expand to N_gyp
                   pp_3 = as.integer(as.factor(pct2$QuestionName[drop=TRUE])), ##Select right policy and expand to N_gyp
                   gg_2 = as.integer(as.factor(pct2$cy)), ##Select right government and expand to N_gyp
                   cc = as.integer(as.factor(country_question[,"country"])), ##Select right country and expand to N_cq
                   qq = as.integer(as.factor(country_question[,"QuestionName"])), ##Select right question and expand to N_cq)
                   cc_2 = as.integer(as.factor(country_question2[,"country"])),
                   pp = as.integer(as.factor(country_question2[,"QuestionName"])),
                   y = pmct$value,
                   z1 = pct1$value,
                   z2 = pct2$value
                   )

##Estimate model
library (rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 20)
common_model <- stan("BrianIRT.stan",
                    model_name = "BrianIRT",
                    data = data_list,
                    chains = 20,
                    iter = 2875,
                    warmup = 2250,
                    cores = 20,
                    init_r = 0.25,
                    seed = 831213,
		    control = list(adapt_delta=0.95),
                    pars = c("mu_a",
                             "mu_g",
                             "sigma",
                             "nu_lq",
                             "lambda2",
                             "lambda",
                             "nu_lp",
                             "alpha",
                             "b",
                             "tau"
                             ))
# common_model_vb <- vb(stan_model("BrianIRT.stan"),
#                       data = data_list,
#                       seed=831213,
#                       pars = c("mu_a",
#                                "mu_g",
#                                "sigma",
#                                "nu_lq",
#                                "lambda2",
#                                "lambda",
#                                "nu_lp",
#                                "alpha",
#                                "b",
#                                "tau"
#                       )
#                       )
# 
# 
# save(common_model_vb, file="Datasets/FinishedOutput/JointIRT_Flexible_1990_VB.RData")

load("Datasets/FinishedOutput/JointIRT_Flexible_1990.RData")


pdf("Graphs/TraceplotsActors(Flexible).pdf", width=100, height=100)
traceplot(common_model_vb, "mu_a")
dev.off()
pdf("Graphs/TraceplotsPolicies(Flexible).pdf", width=100, height=100)
traceplot(common_model_vb, "mu_g")
dev.off()
# #
# #
 actor_cty_yr <- unique(pmct[,c("type","country","year")])
 actor_cty_yr$mood <- colMeans(rstan::extract(common_model, "mu_a")[[1]])
 gov_yr <- unique(finalData[finalData$type=="Government",c("type","country","year")])
 gov_yr$mood <- colMeans(rstan::extract(common_model, "mu_g")[[1]])
 common_data <- rbind(actor_cty_yr,
                      gov_yr
                      )
 common_data$type[common_data$type=="Government"] <- "Policy"

# ## Store latent traits
 common_data$Heterogeneity <- c(colMeans(rstan::extract(common_model, "sigma")[[1]]),rep(1, nrow(gov_yr)))
 write.csv(common_data, file = "Datasets/FinishedOutput/AllMoodsFinal1990.csv")
#
