#############################
# Models for Ch. 8: Linking
# Stages: Citizens to Policy
# Makers.
#############################

library(lme4)
library(ggplot2)
library(wesanderson)
library(gridExtra)
library(texreg)
library(tidyverse)
library(doMC)
library(xtable)
library(AUC)

registerDoMC(12)
## Import data
cit_pm_long <- read.csv("Datasets/FinishedOutput/FullDataLong.csv")
cit_pm_long$Branch <- c(Legislator="Lower House", Senator="Upper House", President="Executive")[as.character(cit_pm_long$type)]
cit_pm_long$IsExec <-cit_pm_long$Branch=="Executive"
cit_pm_long$SameDir <- with(cit_pm_long,sign(ChangeMoodSpline*lag_ChangeCitMood)>0)
cit_pm_long$LaggedOOCongConst <- with(cit_pm_long, ave(oo_cong_const, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$LaggedOOCongSpline <- with(cit_pm_long, ave(oo_cong_spline, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$ImprovedOOCongConst <- with(cit_pm_long, LaggedOOCongConst < oo_cong_const)
cit_pm_long$ImprovedOOCongSpline <- with(cit_pm_long, LaggedOOCongSpline < oo_cong_spline)

cit_pm_long$LaggedMOCongConst <- with(cit_pm_long, ave(mo_cong_const, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$LaggedMOCongSpline <- with(cit_pm_long, ave(mo_cong_spline, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$ImprovedMOCongConst <- with(cit_pm_long, LaggedMOCongConst < mo_cong_const)
cit_pm_long$ImprovedMOCongSpline <- with(cit_pm_long, LaggedMOCongSpline < mo_cong_spline)

cit_pm_long$LaggedMMCongConst <- with(cit_pm_long, ave(mm_cong_const, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$LaggedMMCongSpline <- with(cit_pm_long, ave(mm_cong_spline, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_pm_long$ImprovedMMCongConst <- with(cit_pm_long, LaggedMMCongConst < mm_cong_const)
cit_pm_long$ImprovedMMCongSpline <- with(cit_pm_long, LaggedMMCongSpline < mm_cong_spline)

## Helper functions
bootWrap <- function(.model,.newdata){
  pred_fun <- function(.) {
    predict(., newdata=.newdata, allow.new.levels=TRUE, type="response")
  }
  bootpred <- lme4::bootMer(.model, pred_fun, nsim=250, use.u=TRUE, type="parametric", parallel = "multicore",ncpus=12)
  data.frame(pred = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
             LB = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.1, na.rm=TRUE))),
             UB = apply(bootpred$t, 2, function(x) as.numeric(quantile(x, probs=.9, na.rm=TRUE)))
  )
}

##Example code to get cases for examples
cit_pm_wide <- cit_pm_long %>%
  dplyr::select(country, year, type, group, cong=oo_cong_spline) %>%
  gather(var, val,  group, cong) %>%
  unite(tmp, type, var, sep="_") %>%
  spread(tmp, val=val) %>%
    mutate(avgcong = rowMeans(.[,c(3,5,7)], na.rm=TRUE)) %>%
  dplyr::select(country, year,avgcong,
         Legislator=Legislator_group, President = President_group, Senator = Senator_group)

cit_pm_wide %>%
  filter(Legislator%in%c(2:4)  & (is.na(Senator)|Senator%in%c(2:4)) & President==4)%>%
  group_by(country) %>%
  summarize(mean(avgcong,na.rm=TRUE), sd(avgcong,na.rm=TRUE))

## Summary stats
# Congruence
pdf("Graphs/ObsCongCitPM.pdf", width=11, height=3.5)
ggplot(cit_pm_long %>%
  dplyr::select(Branch,mm_cong_spline:mo_cong_const) %>%
    gather(Measure, val,mm_cong_spline:mo_cong_const) %>%
    mutate(Measure=dplyr::recode(Measure, "oo_cong_spline" = "\nMedian-to-Median_Evolving",
           "oo_cong_const" = "\nMedian-to-Median_Constant",
           "mo_cong_spline" = "Distribution-to-Median_Evolving",
           "mo_cong_const" = "Distribution-to-Median_Constant",
           "mm_cong_spline" = "\nDistribution-to-Distribution_Evolving",
           "mm_cong_const" = "\nDistribution-to-Distribution_Constant"))%>%
    separate(Measure, into=c("Measure", "Interpolation"), sep="_")%>%
    filter(!(Branch=="Executive"&Measure=="\nDistribution-to-Distribution"))%>%
    droplevels(),
 aes(x=factor(Measure, levels=c("\nDistribution-to-Distribution",
                               "Distribution-to-Median",
                               "\nMedian-to-Median")), y = val)) +
  facet_grid(Interpolation~Branch, scales="free_x")+
  geom_boxplot(outlier.color = NA) +
  xlab("Congruence Measure") +
  ylab("")+
  theme_bw()
dev.off()
# Responsiveness
cit_pm_long %>%
  group_by(Branch) %>%
     summarize(MeanResp=mean(SameDir, na.rm=TRUE))

## Model estimation
##Congruence
##Median-to-median
cong_model_oo_spline <- lmer(oo_cong_spline ~
                               + CitizenHet
                             + infl2y
                             + unemp2y
                             + gdp2y
                              + exec_party_spline
                              + median_party_spline
                             +  group
                             +I(group^2)
                             + Branch
                             + (1 | group)
                             , data = cit_pm_long)
summary(cong_model_oo_spline)
cor(model.frame(cong_model_oo_spline)[,1], predict(cong_model_oo_spline))^2


##Constant
cong_model_oo_const <-  lmer(oo_cong_const ~
                                +CitizenHet
                             + infl2y
                             + unemp2y
                             + gdp2y
                             + exec_party_const
                             + median_party_const
                             + group
                             + I(group^2)
                             + (1  | group)
                             + Branch
                             , data = cit_pm_long)
summary(cong_model_oo_const)
cor(model.frame(cong_model_oo_const)[,1], predict(cong_model_oo_const))^2

pred_data_1_s <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             IsExec = c(TRUE,FALSE),
                           group = 1:6,
                           year = "none",
                           CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                           infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                           gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                           unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                           exec_party_spline = mean(cit_pm_long$exec_party_spline, na.rm=TRUE),
                           median_party_spline = mean(cit_pm_long$median_party_spline, na.rm=TRUE),
                           lag_mood_spline = mean (cit_pm_long$lag_mood_spline, na.rm=TRUE),
                           country = unique(cit_pm_long$country)
                           )
pred_data_1_s <- filter(pred_data_1_s,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_1_s_boot <- bootWrap(cong_model_oo_spline, pred_data_1_s)

pred_data_1_s <- pred_data_1_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_1_s_boot)
pred_data_1_c <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             IsExec = c(TRUE,FALSE),
                             group = 1:6,
                             year = "none",
                             CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_const = mean(cit_pm_long$exec_party_const, na.rm=TRUE),
                             median_party_const = mean(cit_pm_long$median_party_const, na.rm=TRUE),
                             lag_mood_const = mean (cit_pm_long$lag_mood_const, na.rm=TRUE),
                             country = "none"
                             )
pred_data_1_c <- filter(pred_data_1_c,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_1_c_boot <- bootWrap(cong_model_oo_const, pred_data_1_c)

pred_data_1_c <- pred_data_1_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_1_c_boot)

pred_data_1 <- rbind(pred_data_1_s[,c("Branch","group","pred","UB","LB","Interpolation")], pred_data_1_c[,c("Branch","group","pred","UB","LB","Interpolation")])
pred_data_1 <- pred_data_1 %>%
  group_by(Branch, Interpolation, group) %>%
  dplyr::summarize(pred = mean(pred),
            LB = mean(LB),
            UB = mean(UB)
  )
pdf("Graphs/CongruenceModelM-M.pdf", width=8, height=3)
CongruenceModelM_M <- ggplot(pred_data_1, aes(x=group, y=pred, pch=Branch)) +
  facet_wrap(~Interpolation, scales="free_y")+
  geom_smooth(aes(x=group, y = pred),col="gray70", method="loess", span=5.5, se=FALSE) +
  geom_point(aes(ymin=LB, ymax=UB), cex=2.5, position=position_dodge(0.3))+
  theme_bw() +
  ylab("Median-to-Median Congruence") +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", 1:6)
CongruenceModelM_M
dev.off()
#CongruenceModelM_M


##Distribution-to-median
cong_model_mo_spline <- lmer(mo_cong_spline ~
                             + infl2y
                             + unemp2y
                             + gdp2y
                             + exec_party_spline
                             + median_party_spline
                              + group
                            + I(group^2)
                             + Branch
                            + (1|group)
                             , data = cit_pm_long)
summary(cong_model_mo_spline)
cor(model.frame(cong_model_mo_spline)[,1], predict(cong_model_mo_spline))^2

##Constant
cong_model_mo_const <-  lmer(mo_cong_const ~
                             + infl2y
                             + unemp2y
                             + gdp2y
                             + exec_party_const
                             + median_party_const
                             +   group
                             + I(group^2)
                             + Branch
                             + (1 | group)
                             , data = cit_pm_long)
summary(cong_model_mo_const)
cor(model.frame(cong_model_mo_const)[,1], predict(cong_model_mo_const))^2

pred_data_2_s <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             group = 1:6,
                             IsExec = c(TRUE, FALSE),
                             CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_spline = mean(cit_pm_long$exec_party_spline, na.rm=TRUE),
                             median_party_spline = mean(cit_pm_long$median_party_spline, na.rm=TRUE),
                             lag_mood_spline = mean (cit_pm_long$lag_mood_spline, na.rm=TRUE),
                             country = "none"
)
pred_data_2_s <- filter(pred_data_2_s,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_2_s_boot <- bootWrap(cong_model_mo_spline, pred_data_2_s)

pred_data_2_s <- pred_data_2_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_2_s_boot)

pred_data_2_c <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             group = 1:6,
                             IsExec = c(TRUE, FALSE),
                             CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_const = mean(cit_pm_long$exec_party_const, na.rm=TRUE),
                             median_party_const = mean(cit_pm_long$median_party_const, na.rm=TRUE),
                             lag_mood_const = mean (cit_pm_long$lag_mood_const, na.rm=TRUE),
                             country = "none"
)
pred_data_2_c <- filter(pred_data_2_c,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_2_c_boot <- bootWrap(cong_model_mo_const, pred_data_2_c)

pred_data_2_c <- pred_data_2_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_2_c_boot)

pred_data_2 <- rbind(pred_data_2_s[,c("Branch","group","pred","UB","LB","Interpolation")], pred_data_2_c[,c("Branch","group","pred","UB","LB","Interpolation")])

pdf("Graphs/CongruenceModelD-M.pdf", width=8, height=3)
CongruenceModelD_M <- ggplot(pred_data_2, aes(x=group, y=pred, pch=Branch)) +
  facet_wrap(~Interpolation, scales="free_y")+
  geom_smooth(aes(x=group, y = pred),col="gray70", method="loess", span=2, se=FALSE) +
  geom_point(cex=1.7, position=position_dodge(0.3))+
  theme_bw() +
  ylab("Distribution-to-Median Congruence") +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", 1:6)
CongruenceModelD_M
dev.off()
CongruenceModelD_M

##Distribution-to-distribution
cong_model_mm_spline <- lmer(mm_cong_spline ~
                             + infl2y
                             + unemp2y
                             + gdp2y
                             + exec_party_spline
                             + median_party_spline
                             + group
                             + I(group^2)
                             + Branch
                             + (1|group)
                             , data = cit_pm_long)
summary(cong_model_mm_spline)
cor(model.frame(cong_model_mm_spline)[,1], predict(cong_model_mm_spline))^2

##Constant
cong_model_mm_const <-  lmer(mm_cong_const ~
                             + infl2y
                             + unemp2y
                             + gdp2y
                             + exec_party_const
                             + median_party_const
                             + group
                             + I(group^2)
                             + Branch
                             + (1|group)
                             , data = cit_pm_long)
summary(cong_model_mm_const)
cor(model.frame(cong_model_mm_const)[,1], predict(cong_model_mm_const))^2

pred_data_3_s <- expand.grid(Branch = c("Lower House", "Upper House"),
                             group = 1:5,
                             CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_spline = mean(cit_pm_long$exec_party_spline, na.rm=TRUE),
                             median_party_spline = mean(cit_pm_long$median_party_spline, na.rm=TRUE),
                             lag_mood_spline = mean (cit_pm_long$lag_mood_spline, na.rm=TRUE),
                             country = "none")

pred_3_s_boot <- bootWrap(cong_model_mm_spline, pred_data_3_s)

pred_data_3_s <- pred_data_3_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_3_s_boot)

pred_data_3_c <- expand.grid(Branch = c("Lower House", "Upper House"),
                             group = 1:5,
                             CitizenHet = mean(cit_pm_long$CitizenHet, na.rm=TRUE),
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_const = mean(cit_pm_long$exec_party_const, na.rm=TRUE),
                             median_party_const = mean(cit_pm_long$median_party_const, na.rm=TRUE),
                             lag_mood_const = mean (cit_pm_long$lag_mood_const, na.rm=TRUE),
                             country = "none")
pred_3_c_boot <- bootWrap(cong_model_mm_const, pred_data_3_c)

pred_data_3_c <- pred_data_3_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_3_c_boot)

pred_data_3 <- rbind(pred_data_3_s[,c("Branch","group","pred","UB","LB","Interpolation")],
                     pred_data_3_c[,c("Branch","group","pred","UB","LB","Interpolation")])

pdf("Graphs/CongruenceModelD-D.pdf", width=8, height=3)
CongruenceModelD_D <- ggplot(pred_data_3, aes(x=group, y=pred, pch=Branch)) +
  facet_wrap(~Interpolation, scales="free_y")+
  geom_smooth(aes(x=group, y = pred),col="gray70", method="loess", span = 2, se=FALSE) +
  geom_point(aes(ymin=LB, ymax=UB), cex=2.7, position=position_dodge(0.3))+
  scale_shape_manual(values=c(17,15)) +
  theme_bw() +
  ylab("Distribution-to-Distribution Congruence") +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", 1:6)
CongruenceModelD_D
dev.off()
CongruenceModelD_D


texreg(list(cong_model_oo_spline,
       cong_model_oo_const,
       cong_model_mo_spline,
       cong_model_mo_const,
       cong_model_mm_spline,
       cong_model_mm_const),
       booktabs =TRUE,
       dcolumn = TRUE,
       stars = .1)

## Citizen heterogeneity
by_country <- cit_pm_long %>%
  select(country, year, type, group, CitizenHet,  infl2y, unemp2y, gdp2y) %>%
  spread(key=type, value=c(group)) %>%
  mutate(ELUGroup=case_when(
    Legislator<=3&President<=5&(is.na(Senator)|Senator<=3) ~ "All weak",
    (Legislator>3|Senator>3)&President<=5 ~ "At least one legislative\n branch strong,\n president weak",
    (Legislator<=3|Senator<=3)&President>5 ~ "At least one legislative\n branch weak,\n president strong",
    Legislator>3&President>5&(is.na(Senator)|Senator>3) ~ "All strong"
  )
  ) %>%
  mutate(ELUGroup = factor(ELUGroup, levels=c("All strong", "At least one legislative\n branch weak,\n president strong", "All weak","At least one legislative\n branch strong,\n president weak")))

het_model <-lmer(CitizenHet ~ -1
                 + infl2y
                 + unemp2y
                 + gdp2y
                 + (1|ELUGroup)
                 , data = by_country
)
ranef_het <- as.data.frame(lme4::ranef(het_model, condVar=TRUE))
ranef_het <- ranef_het %>%
  filter(grepl("ELUGroup", grpvar))

pdf("Graphs/CitizenHetModel.pdf", width=6, height=4)
ggplot(ranef_het, aes(x=grp, y=(condval))) +
  geom_errorbar(aes(ymin=(condval-(condsd*.5)),ymax=(condval+(condsd*.5))),
                width=.1)+
  geom_point(cex=2.5)+
  theme_bw() +
  theme(legend.position="bottom") +
  ylab("Expected Citizen Heterogeneity") +
  xlab("Electoral System Group")
dev.off()




## Responsiveness dichotomized
dichres_model_s <- glmer(SameDir ~
                      + infl2y
                      + unemp2y
                      + gdp2y
                      + exec_party_spline
                      + median_party_spline
                     +  group
                     + Branch
                     + (1|group)
                  , data = cit_pm_long
                    ,family=binomial
                     , control = glmerControl(optimizer = "Nelder_Mead")
                    )
summary(dichres_model_s)
auc(roc(predict(dichres_model_s, type="response"), as.factor(model.frame(dichres_model_s)[,1])))


cit_pm_long_1st <- subset(cit_pm_long, !duplicated(mood_const))
dichres_model_c <- glmer(SameDir ~
                    + infl2y
                    + unemp2y
                    + gdp2y
                    + exec_party_const
                    + median_party_const
                    +  group
                    + Branch
                    + (1| group)
                  , data = cit_pm_long_1st
                    ,family=binomial
                    )
summary(dichres_model_c)
auc(roc(predict(dichres_model_c, type="response"), as.factor(model.frame(dichres_model_c)[,1])))


texreg(list(dichres_model_s,
            dichres_model_c),
       booktabs =TRUE,
       dcolumn = TRUE,
       stars = .1)

pred_data_4_s <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             IsExec = c(TRUE, FALSE),
                             group = 1:6,
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_spline = mean(cit_pm_long$exec_party_spline, na.rm=TRUE),
                             median_party_spline = mean(cit_pm_long$median_party_spline, na.rm=TRUE),
                             country = "none"
)
pred_data_4_s <- filter(pred_data_4_s,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_4_s_boot <- bootWrap(dichres_model_s, pred_data_4_s)

pred_data_4_s <- pred_data_4_s %>%
  mutate(Interpolation = "Evolving")%>%
  bind_cols(pred_4_s_boot)

pred_data_4_c <- expand.grid(Branch = c("Executive", "Lower House", "Upper House"),
                             IsExec = c(TRUE, FALSE),
                             group = 1:6,
                             infl2y = mean(cit_pm_long$infl2y, na.rm=TRUE),
                             gdp2y = mean(cit_pm_long$gdp2y, na.rm=TRUE),
                             unemp2y = mean(cit_pm_long$unemp2y, na.rm=TRUE),
                             exec_party_const = mean(cit_pm_long$exec_party_const, na.rm=TRUE),
                             median_party_const = mean(cit_pm_long$median_party_const, na.rm=TRUE),
                             country = "none")
pred_data_4_c <- filter(pred_data_4_c,
                        !((Branch=="Executive"&IsExec==FALSE)|(Branch!="Executive"&IsExec==TRUE))
                        & !(Branch=="Executive"&group < 4)
                        &!(Branch!="Executive" & group > 5))
pred_4_c_boot <- bootWrap(dichres_model_c, pred_data_4_c)

pred_data_4_c <- pred_data_4_c %>%
  mutate(Interpolation = "Constant")%>%
  bind_cols(pred_4_c_boot)

pred_data_4 <- rbind(pred_data_4_s[,c("Branch","group","pred","UB","LB","Interpolation")], pred_data_4_c[,c("Branch","group","pred","UB","LB","Interpolation")])


pdf("Graphs/DichResponsivenessModel.pdf", width=8, height=3)
DichResponsivenessModel <- ggplot(pred_data_4, aes(x=group, y=pred, pch=Branch)) +
  facet_wrap(~Interpolation)+
  geom_smooth(aes(x=group, y = pred),col="gray70", method="loess", span=2, se=FALSE) +
  geom_point(aes(ymin=LB, ymax=UB), cex=2.7, position=position_dodge(0.3))+
  theme_bw() +
  ylab("Probability of moving in the same direction") +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", 1:6)
DichResponsivenessModel
dev.off()
DichResponsivenessModel

## Do responsive changes generate more or less congruence?
round(prop.table(xtabs(~ImprovedOOCongConst + SameDir, data=cit_pm_long)), 2)
round(prop.table(xtabs(~ImprovedOOCongSpline + SameDir, data=cit_pm_long)), 2)

round(prop.table(xtabs(~ImprovedMOCongConst + SameDir, data=cit_pm_long)), 2)
round(prop.table(xtabs(~ImprovedMOCongSpline + SameDir, data=cit_pm_long)), 2)

round(prop.table(xtabs(~ImprovedMMCongConst + SameDir, data=cit_pm_long)), 2)
round(prop.table(xtabs(~ImprovedMMCongSpline + SameDir, data=cit_pm_long)), 2)

