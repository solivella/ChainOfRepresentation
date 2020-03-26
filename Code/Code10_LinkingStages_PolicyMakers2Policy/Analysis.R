########################
# Models for Ch 9:
# Policy-makers to
# Policies.
########################

#library(rstanarm)
library(lme4)
library(texreg)
library(doMC)
library(tidyverse)
library(AUC)

registerDoMC(12)

set.seed(831213)


## Helper functions
bootWrap <- function(.model,.newdata){
  pred_fun <- function(.) {
    predict(., newdata=.newdata, allow.new.levels=TRUE, type="response")
  }
  bootpred <- lme4::bootMer(.model, pred_fun, nsim=250, use.u=TRUE, type="parametric",parallel = "multicore",ncpus=12)
  data.frame(pred = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
             LB = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.1, na.rm=TRUE))),
             UB = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.9, na.rm=TRUE)))
  )
}

## Import data
pow_pref_pol <- read.csv("Datasets/FinishedOutput/PowersPrefPolicies.csv")

pow_pref_pol$LaggedWinCongConst <- with(pow_pref_pol, ave(winset_cong_c_B, country, type, FUN=function(x)c(NA, x[-length(x)])))
pow_pref_pol$LaggedWinCongSpline <- with(pow_pref_pol, ave(winset_cong_s_B, country, type, FUN=function(x)c(NA, x[-length(x)])))
pow_pref_pol$ImprovedWinCongConst <- with(pow_pref_pol, (LaggedWinCongConst < winset_cong_c_B) | ((LaggedWinCongConst==1) & (winset_cong_c_B==1)))
pow_pref_pol$ImprovedWinCongSpline <- with(pow_pref_pol, (LaggedWinCongSpline < winset_cong_s_B) | ((LaggedWinCongSpline==1) & (winset_cong_s_B==1)))

pow_pref_pol$LaggedMOCongConst <- with(pow_pref_pol, ave(mo_cong_c_L, country, type, FUN=function(x)c(NA, x[-length(x)])))
pow_pref_pol$LaggedMOCongSpline <- with(pow_pref_pol, ave(mo_cong_s_L, country, type, FUN=function(x)c(NA, x[-length(x)])))
pow_pref_pol$ImprovedMOCongConst <- with(pow_pref_pol, LaggedMOCongConst < mo_cong_c_L)
pow_pref_pol$ImprovedMOCongSpline <- with(pow_pref_pol, LaggedMOCongSpline < mo_cong_s_L)



## Observed levels of congruence
coll_pref_pol <- pow_pref_pol %>%
  group_by(country, ProcessNumberAll) %>%
  summarize("Congruence: Winset_Both" = mean(winset_cong_s_B, na.rm=TRUE),
            "Congruence: Distribution-to-Policy_Legislature" = mean(mo_cong_s_L,na.rm=TRUE)) %>%
  gather(var, val, c("Congruence: Winset_Both","Congruence: Distribution-to-Policy_Legislature"))%>%
  separate(var, into=c("Measure","Branch"), sep="_")


pdf("Graphs/ObsCongPMPol.pdf", width=8, height=3.5)
ggplot(coll_pref_pol, aes(x=Measure, y = val)) +
  facet_grid(~Branch, scales="free")+
  geom_boxplot(outlier.color = NA) +
  xlab("") +
  ylab("")+
  theme_bw()
dev.off()


## Models

## Winset one-to-one
##Evolving
winset_model_s <- glmer(winset_cong_s_B ~
                        + infl2y
                        + unemp2y
                          + gdp2y
                        + ExecParty_pref_spline
                        + MedianParty_pref_spline
                        + dist_spline
                        + ProcessNumberAll
                        + I(ProcessNumberAll^2)
                        + (1 |ProcessNumberAll)
                        , family=binomial()
                        ,data = pow_pref_pol)
auc(roc(predict(winset_model_s, type="response"), as.factor(model.frame(winset_model_s)[,1])))


##Constant
winset_model_c <- glmer(winset_cong_c_B ~
                        + infl2y
                        + unemp2y
                          + gdp2y
                        + ExecParty_pref_const
                        + MedianParty_pref_const
                        + dist_const
                        + ProcessNumberAll
                        + I(ProcessNumberAll^2)
                        + (1 |ProcessNumberAll)
                        , family=binomial()
                        ,data = pow_pref_pol)
auc(roc(predict(winset_model_c, type="response"), as.factor(model.frame(winset_model_c)[,1])))


pred_data_1_s <- expand.grid(ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_spline = mean(pow_pref_pol$ExecParty_pref_spline, na.rm=TRUE),
                             MedianParty_pref_spline = mean(pow_pref_pol$MedianParty_pref_spline, na.rm=TRUE),
                             dist_spline = mean(pow_pref_pol$dist_spline, na.rm=TRUE))
pred_1_s_boot <- bootWrap(winset_model_s, pred_data_1_s)
pred_data_1_s <- pred_data_1_s %>%
  mutate(Interpolation = "Evolving") %>%
  bind_cols(pred_1_s_boot)

pred_data_1_c <- expand.grid(ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_const = mean(pow_pref_pol$ExecParty_pref_const, na.rm=TRUE),
                             MedianParty_pref_const = mean(pow_pref_pol$MedianParty_pref_const, na.rm=TRUE),
                             dist_const = mean(pow_pref_pol$dist_const, na.rm=TRUE))
pred_1_c_boot <- bootWrap(winset_model_c, pred_data_1_c)

pred_data_1_c <- pred_data_1_c %>%
  mutate(Interpolation = "Constant") %>%
  bind_cols(pred_1_c_boot)

pred_data_1 <- rbind(pred_data_1_s[,c("ProcessNumberAll","pred","UB","LB","Interpolation")], pred_data_1_c[,c("ProcessNumberAll","pred","UB","LB","Interpolation")])
pred_data_1$CongruenceType = "Winset Congruence (probability of being in winset)"

## MANY TO ONE
## Evolving
cong_model_s_mo <- lmer(mo_cong_s_L ~
                        + infl2y
                        + unemp2y
                        + gdp2y
                        + ExecParty_pref_spline
                        + MedianParty_pref_spline
                        + dist_spline
                        + ProcessNumberAll
                        + I(ProcessNumberAll^2)
                        + (1 |ProcessNumberAll)
                        ,data = pow_pref_pol)
cor(model.frame(cong_model_s_mo)[,1], predict(cong_model_s_mo))^2


cong_model_c_mo <- lmer(mo_cong_c_L ~
                        + infl2y
                        + unemp2y
                        + gdp2y
                        + ExecParty_pref_const
                        + MedianParty_pref_const
                        + dist_const
                        + ProcessNumberAll
                        + I(ProcessNumberAll^2)
                        + (1 |ProcessNumberAll)
                        ,data = pow_pref_pol)
cor(model.frame(cong_model_c_mo)[,1], predict(cong_model_c_mo))^2

pred_data_2_s <- expand.grid(ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_spline = mean(pow_pref_pol$ExecParty_pref_spline, na.rm=TRUE),
                             MedianParty_pref_spline = mean(pow_pref_pol$MedianParty_pref_spline, na.rm=TRUE),
                             dist_spline = mean(pow_pref_pol$dist_spline, na.rm=TRUE))
pred_2_s_boot <- bootWrap(cong_model_s_mo, pred_data_2_s)

pred_data_2_s <- pred_data_2_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_2_s_boot)

pred_data_2_c <- expand.grid(ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_const = mean(pow_pref_pol$ExecParty_pref_const, na.rm=TRUE),
                             MedianParty_pref_const = mean(pow_pref_pol$MedianParty_pref_const, na.rm=TRUE),
                             dist_const = mean(pow_pref_pol$dist_const, na.rm=TRUE))
pred_2_c_boot <- bootWrap(cong_model_c_mo, pred_data_2_c)

pred_data_2_c <- pred_data_2_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_2_c_boot)

pred_data_2 <- rbind(pred_data_2_s[,c("ProcessNumberAll","pred","UB","LB","Interpolation")], pred_data_2_c[,c("ProcessNumberAll","pred","UB","LB","Interpolation")])
pred_data_2$CongruenceType = "Distribution-to-Policy (Legislatures Only)"

pred_data <- rbind(pred_data_1, pred_data_2)

pdf("Graphs/PMPCongruenceModel.pdf", width=8, height=3)
PMPCongruenceModel <- ggplot(pred_data, aes(x=ProcessNumberAll, y=pred, pch=Interpolation)) +
  facet_wrap(~CongruenceType, scale="free_y")+
  geom_smooth(col="gray70", method="loess",span=2, se=FALSE) +
  geom_point(aes(ymin=LB, ymax=UB), cex=2.7, position=position_dodge(0.3))+
  theme_bw() +
  ylab("Predicted Congruence") +
  scale_x_continuous("(Legislative <--) Policy-Making Process (--> Executive)", 1:13)
PMPCongruenceModel
dev.off()
PMPCongruenceModel


texreg(list(winset_model_s,
            winset_model_c,
            cong_model_s_mo,
            cong_model_c_mo),
       custom.coef.names = c("(Intercept)",
                             "Inflation",
                             "Unemployment",
                             "GDP growth",
                             "Executive Party",
                             "Median Legislative Party",
                             "Distance: Exec. to Median Leg.",
                             "PMP",
                             "PMP$^2$",
                             "Executive Party",
                             "Median Legislative Party",
                             "Distance: Exec. to Median Leg."
                             ),
       booktabs = TRUE,
       dcolumn = TRUE,
       stars = .1)

## Responsiveness
##Model with two branches


pow_pref_pol$SameDirChangeLeg_s <- with(pow_pref_pol, sign(ChangePolicy*ChangeLPrefSpline)>0)
pow_pref_pol$SameDirChangeExec_s <- with(pow_pref_pol, sign(ChangePolicy*ChangeEPrefSpline)>0)
pow_pref_long <- pow_pref_pol %>%
  dplyr::select(country,
         year,
         Resp_Legislature = SameDirChangeLeg_s,
         Resp_Executive = SameDirChangeExec_s,
         gdp2y,
         infl2y,
         unemp2y,
         ExecParty_pref_const,
         MedianParty_pref_const,
         ExecParty_pref_spline,
         MedianParty_pref_spline,
         ImprovedWinCongConst,
         ImprovedWinCongSpline,
         ImprovedMOCongConst,
         ImprovedMOCongSpline,
         ProcessNumberAll,
         dist_const,
         dist_spline) %>%
  gather(Variable, SameDir, c("Resp_Legislature", "Resp_Executive")) %>%
  separate(Variable, into = c("Variable","Branch"))

pow_pref_long_1st <- subset(pow_pref_long, !duplicated(pow_pref_long[,c("country", "dist_const","Branch")]))


resp_pp_s <- glmer(SameDir ~  ExecParty_pref_spline
                   +MedianParty_pref_spline
                   + infl2y
                   + unemp2y
                   +gdp2y
                   + ProcessNumberAll
                   + I(ProcessNumberAll^2)
                   + Branch
                   +(1|ProcessNumberAll),
                   family = binomial,
                   data = pow_pref_long)
auc(roc(predict(resp_pp_s, type="response"), as.factor(model.frame(resp_pp_s)[,1])))


resp_pp_c <- glmer(SameDir ~ ExecParty_pref_const
                   + MedianParty_pref_const
                   + infl2y
                   + unemp2y
                   + gdp2y
                   + ProcessNumberAll
                   + I(ProcessNumberAll^2)
                   + Branch
                   +(1|ProcessNumberAll),
                   family = binomial,
                   data = pow_pref_long_1st)
auc(roc(predict(resp_pp_c, type="response"), as.factor(model.frame(resp_pp_c)[,1])))


pred_data_3_s <- expand.grid(Branch = c("Executive", "Legislature"),
                             ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_spline = mean(pow_pref_pol$ExecParty_pref_spline, na.rm=TRUE),
                             MedianParty_pref_spline = mean(pow_pref_pol$MedianParty_pref_spline, na.rm=TRUE),
                             dist_spline = mean(pow_pref_pol$dist_spline, na.rm=TRUE))
pred_3_s_boot <- bootWrap(resp_pp_s, pred_data_3_s)

pred_data_3_s <- pred_data_3_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_3_s_boot)

pred_data_3_c <- expand.grid(Branch = c("Executive", "Legislature"),
                             ProcessNumberAll = 1:13,
                             infl2y = mean(pow_pref_pol$infl2y, na.rm=TRUE),
                             gdp2y = mean(pow_pref_pol$gdp2y, na.rm=TRUE),
                             unemp2y = mean(pow_pref_pol$unemp2y, na.rm=TRUE),
                             ExecParty_pref_const = mean(pow_pref_pol$ExecParty_pref_const, na.rm=TRUE),
                             MedianParty_pref_const = mean(pow_pref_pol$MedianParty_pref_const, na.rm=TRUE),
                             dist_const = mean(pow_pref_pol$dist_const, na.rm=TRUE))
pred_3_c_boot <- bootWrap(resp_pp_c, pred_data_3_c)

pred_data_3_c <- pred_data_3_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_3_c_boot)


pred_data_3 <- rbind(pred_data_3_s[,c("ProcessNumberAll","pred","Branch","Interpolation")],
       pred_data_3_c[,c("ProcessNumberAll","pred","Branch","Interpolation")])

pdf("Graphs/PMPResponsivenessModel.pdf", width=8, height=3)
PMPResponsivenessModel <- ggplot(pred_data_3, aes(x=ProcessNumberAll, y=pred, pch=Branch))+
  facet_wrap(~Interpolation)+
  geom_smooth(aes(lty=Branch),se=FALSE, col="gray70", method="loess", span=2) +
  geom_point(cex=2.7,position=position_dodge(0.3))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme_bw() +
  ylab("Probability that policy and mood\nmove in the same direction") +
  scale_x_continuous("(Legislative <--) Policy-Making Process (--> Executive)", 1:13)
PMPResponsivenessModel
dev.off()
PMPResponsivenessModel

texreg(list(resp_pp_s,
            resp_pp_c),
       custom.coef.names = c("(Intercept)",
                             "Executive Party",
                             "Median Legislative Party",
                             "Inflation",
                             "Unemployment",
                             "GDP growth",
                             "PMP",
                             "PMP$^2$",
                             "Legislature?",
                             "Executive Party",
                             "Median Legislative Party"
       ),
       booktabs = TRUE,
       dcolumn = TRUE,
       stars = .1)


## Do responsive changes generate more or less congruence?
round(prop.table(xtabs(~ImprovedWinCongConst + SameDir, data=pow_pref_long)), 2)
round(prop.table(xtabs(~ImprovedWinCongSpline + SameDir, data=pow_pref_long)), 2)

round(prop.table(xtabs(~ImprovedMOCongConst + SameDir, data=pow_pref_long)), 2)
round(prop.table(xtabs(~ImprovedMOCongSpline + SameDir, data=pow_pref_long)), 2)


