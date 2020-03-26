#############################
# Models for Ch. 11:  Citizens
# to Policy
#############################


library(lme4)
library(ggplot2)
library(texreg)
library(AUC)
library(dplyr)
library(tidyverse)

library(doMC)

registerDoMC(4)
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

cit_data <- read.csv("Datasets/FinishedOutput/CitPrefESPow.csv")
cit_data$PMP <- cit_data$ProcessNumberAll
cit_data$SenGroup <- replace_na(cit_data$SenGroup,"X")
cit_data$SenGroupNum <- as.numeric(replace(cit_data$SenGroup, cit_data$SenGroup=="X", "0"))
cit_data$InstExec <- as.factor(with(cit_data, paste(PMP,ExecGroup,sep="_")))
cit_data$InstLow <- as.factor(with(cit_data,  paste(PMP,LegGroup,sep="_")))
cit_data$InstUp <- as.factor(with(cit_data,  paste(PMP,SenGroup,sep="_")))
cit_data$Institutions <- as.factor(with(cit_data,  paste(PMP,ExecGroup,LegGroup,replace_na(SenGroup,"X"),sep="_")))
cit_data$sameDirChange <- with(cit_data,sign(ChangePolicy*lag_ChangeCitPref)>0)

cit_data$LaggedOOCong <- with(cit_data, ave(oo_cong, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_data$LaggedMOCong <- with(cit_data, ave(mo_cong, country, type, FUN=function(x)c(NA, x[-length(x)])))
cit_data$ImprovedOOCong <- with(cit_data, LaggedOOCong < oo_cong)
cit_data$ImprovedMOCong <- with(cit_data, LaggedMOCong < mo_cong)





cit_data_sim <- read.csv("Datasets/FinishedOutput/CitizenToPolicySim.csv")
cit_data_sim <- filter(cit_data_sim, Variable != "Simulated Responsiveness")
cit_data_sim$Source <- "Simulated" 
##Descriptive graphs
pdf("Graphs/ObsCongCPol.pdf", width=10, height=3)
ggplot(cit_data %>%
         select("Congruence: Median-to-Policy" = oo_cong,
                "Congruence: Distribution-to-Policy" = mo_cong) %>%
         gather(Variable, val, "Congruence: Median-to-Policy":"Congruence: Distribution-to-Policy") %>%
         mutate(Source = "Observed") %>%
         rbind(select(cit_data_sim, c("Variable","Source", "val"))) %>%
         mutate(Source=factor(Source, levels=c("Simulated", "Observed"))),
       aes(x=Variable,y=val)) +
  geom_boxplot(outlier.color = NA) +
  facet_wrap(~Source)+
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()
pdf("Graphs/ObsRespCPol.pdf", width=4, height=3.5)
ggplot(cit_data %>%
         group_by(country) %>%
         summarize(val = mean(sameDirChange, na.rm=TRUE)) %>%
         mutate(var = "Observed Responsiveness"), aes(x=var,y=val-0.05)) +
  geom_boxplot(outlier.color = NA) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()


#one to one
oo_cong_model <- lmer(oo_cong ~
                        +ExecParty_pref
                      + MedianParty_pref
                      + infl2y
                      + unemp2y
                      + gdp2y
                      + I(PMP^2)
                      + I(LegGroup^2)
                      + I(ExecGroup^2)
                      + I(SenGroupNum^2)
                      + PMP * LegGroup
                      + PMP * ExecGroup
                      + PMP * SenGroupNum
                      +(1|PMP)
                      + (1|ExecGroup)
                      +(1|LegGroup)
                      +(1|SenGroup)
                        ,data = cit_data
                        , REML = TRUE
                        , control=lmerControl(optimizer = "Nelder_Mead"))
summary(oo_cong_model)
cor(model.frame(oo_cong_model)[,1], predict(oo_cong_model))^2


## PMP for Leg, How do rules of Leg affect  congruence?
pred_data_1 <- expand.grid(PMP = 1:13,#c(1:7),
                           ExecGroup = c(4:6),
                           LegGroup = c(1:5),
                           SenGroupNum = c(0:5),
                           SenGroup = c("1","2","3","4","5", "X"),
                           LEDist =  mean(cit_data$LEDist, na.rm=TRUE),
                           gdp2y = 2.3,
                           infl2y = -0.3,
                           unemp2y = 7.4,
                           ExecParty_pref = mean(cit_data$ExecParty_pref, na.rm=TRUE),
                           MedianParty_pref = mean(cit_data$MedianParty_pref, na.rm=TRUE)
                           )

pred_data_1 <- pred_data_1 %>%
  filter((SenGroup == "X" & SenGroupNum == 0) | (SenGroupNum == SenGroup)) %>%
  bind_cols(bootWrap(oo_cong_model, .)) %>%
  mutate(Institutions = paste(PMP,ExecGroup,LegGroup,SenGroup,sep="_"),
         PMP = paste("PMP", PMP, sep=" "))

pred_data_1_e <- pred_data_1 %>%
  group_by(PMP, Group=ExecGroup) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  mutate(Branch="Executive")
pred_data_1_l <- pred_data_1 %>%
  group_by(PMP, Group=LegGroup) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  mutate(Branch="Lower House")

pred_data_1_s <- pred_data_1 %>%
  group_by(PMP, Group=SenGroupNum) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  filter(Group!=0)%>%
  mutate(Branch="Upper House")
pred_data_1 <- rbind(pred_data_1_l,pred_data_1_e,pred_data_1_s)

pdf("Graphs/CongruenceCitPol_MM.pdf", width=7, height=6)
pred_data_1$PMP <- factor(pred_data_1$PMP, levels=paste("PMP",1:13))
CongruenceCitPol_MM <- ggplot(pred_data_1, aes(x=Group, y=pred, pch=Branch)) +
  facet_wrap(~PMP, ncol=3)+
  geom_smooth(method = "loess", col="gray70", span=2, se=FALSE)+
  geom_point(aes(ymin=LB, ymax=UB), cex=2.7, position=position_dodge(0.3))+
  theme_bw()+
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", breaks = 1:6)+
  ylab("(Incongruent <--) Marginal Predicted Citizen-to-Policy Congruence (--> Congruent)")
CongruenceCitPol_MM
dev.off()
CongruenceCitPol_MM


#many to one m
mo_cong_model <- lmer(mo_cong ~
                        + ExecParty_pref
                        + MedianParty_pref
                        + infl2y
                        + unemp2y
                        + gdp2y
                      + I(PMP^2)
                      + I(LegGroup^2)
                      + I(ExecGroup^2)
                      + I(SenGroupNum^2)
                      + PMP
                      * LegGroup
                      + PMP*ExecGroup
                      + PMP*SenGroupNum
                      +(1|PMP)
                      + (1|ExecGroup)
                      +(1|LegGroup)
                      +(1|SenGroup)
                        ,data = cit_data
                        , REML = TRUE
                        , control=lmerControl(optimizer = "bobyqa"))

summary(mo_cong_model)
cor(model.frame(mo_cong_model)[,1], predict(mo_cong_model))^2


pred_data_3 <- expand.grid(PMP = 1:13,
                           ExecGroup = c(4:6),
                           LegGroup = c(1:5),
                           SenGroupNum = c(0:5),
                           SenGroup = c("1","2","3","4","5","X"),
                           LEDist = mean(cit_data$LEDist, na.rm=TRUE),
                           gdp2y = 2.3,
                           infl2y = -0.3,
                           unemp2y = 7.4,
                           ExecParty_pref = mean(cit_data$ExecParty_pref, na.rm=TRUE),
                           MedianParty_pref = mean(cit_data$MedianParty_pref, na.rm=TRUE))

pred_data_3 <- filter(pred_data_3,(SenGroup == "X" & SenGroupNum == 0) | (SenGroupNum == SenGroup))
pred_data_3 <- pred_data_3 %>%
  bind_cols(bootWrap(mo_cong_model, .))%>%
  mutate(PMP = paste("PMP", PMP, sep=" ")) %>%
  group_by(PMP, LegGroup, SenGroup) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  ungroup()
pred_data_3_l <- pred_data_3 %>%
  group_by(PMP, Group=LegGroup) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  ungroup()%>%
  mutate(Branch="Lower House")
pred_data_3_s <- pred_data_3 %>%
  group_by(PMP, Group=SenGroup) %>%
  summarize(pred = mean(pred),
            UB = mean(UB),
            LB = mean(LB)) %>%
  ungroup()%>%
  filter(Group!="X")%>%
  mutate(Branch="Upper House")
pred_data_3 <- rbind(pred_data_3_s, pred_data_3_l)

pdf("Graphs/CongruenceCitPol_DM.pdf", width=7, height=7)
pred_data_3$PMP <- factor(pred_data_3$PMP, levels=paste("PMP",1:13))
CongruenceCitPol_DM <- ggplot(pred_data_3, aes(x=as.numeric(Group), y=pred, pch=Branch)) +
  facet_wrap(~PMP, ncol=3)+
  geom_smooth(aes(x=as.numeric(Group)),method = "loess", span=2, se=FALSE,col="gray70")+
  geom_point(aes(ymin=LB, ymax=UB), cex=2.7, position=position_dodge(0.3))+
  scale_shape_manual(values=c(17,15))+
  theme_bw() +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", breaks = 1:5)+
  ylab("(Incongruent <--) Marginal Predicted Distribution-to-Policy Congruence (--> Congruent)")
CongruenceCitPol_DM
dev.off()
CongruenceCitPol_DM

texreg(list(oo_cong_model,mo_cong_model),
       custom.model.names = c("Median-to-Policy","Distribution-to-Policy"),
       custom.coef.names = c("Intercept",
                             "Executive Party",
                             "Median Legislative Party",
                             "Inflation",
                             "Unemployment",
                             "GDP growth",                             
                             "PMP$^2$",
                             "Elec. Group of Lower$^2$",
                             "Elec. Group of President$^2$",
                             "Elec. Group of Upper$^2$",
                             "PMP",
                             "Elec. Group of Lower",
                             "Elec. Group of President",
                             "Elec. Group of Upper",
                             "PMP x Elec. Group of Lower",
                             "PMP x Elec. Group of President",
                             "PMP x Elec. Group of Upper"),
       booktabs = TRUE,
       dcolumn = TRUE,
       stars = .1,
       include.aic = TRUE,
       digits = 2)


##Responsiveness
resp_model <- glmer(sameDirChange~
                    ExecParty_pref
                  + MedianParty_pref
                  + infl2y
                  + unemp2y
                  + gdp2y
                  + PMP * LegGroup
                  + PMP*SenGroupNum
                  + PMP*ExecGroup
                  +(1|PMP)
                  + (1|ExecGroup)
                  +(1|LegGroup)
                  +(1|SenGroup)
                  , glmerControl(optimizer = "Nelder_Mead")
                  ,data=cit_data
                  ,family=binomial)

summary(resp_model, probs=c(.1,.9))
auc(roc(predict(resp_model, type="response"), as.factor(model.frame(resp_model)[,1])))

pred_data_4 <- expand.grid(PMP = 1:13,#c(1:7),
                           ExecGroup = c(4:6),
                           LegGroup = c(1:5),
                           SenGroupNum = c(0:5),
                           SenGroup = c("1","2","3","4","5","X"),
                           gdp2y = 2.3,
                           infl2y = -0.3,
                           unemp2y = 7.4,
                           ExecParty_pref = mean(cit_data$ExecParty_pref, na.rm=TRUE),
                           MedianParty_pref = mean(cit_data$MedianParty_pref, na.rm=TRUE)
                           )
pred_data_4 <- pred_data_4 %>%
  filter((SenGroup == "X" & SenGroupNum == 0) | (SenGroupNum == SenGroup))%>%
  mutate(Institutions = paste(PMP,ExecGroup,LegGroup,SenGroup,sep="_")) %>%
  mutate(pred = predict(resp_model, ., type="response",allow.new.levels=TRUE))%>%
  mutate(PMP = paste("PMP", PMP, sep=" "))



pred_data_4_e <- pred_data_4 %>%
  group_by(PMP, Group=ExecGroup) %>%
  summarize(pred = mean(pred)) %>%
  mutate(Branch="Executive")
pred_data_4_l <- pred_data_4 %>%
  group_by(PMP, Group=LegGroup) %>%
  summarize(pred = mean(pred)) %>%
  mutate(Branch="Lower House")

pred_data_4_s <- pred_data_4 %>%
  group_by(PMP, Group=SenGroupNum) %>%
  summarize(pred = mean(pred)) %>%
  filter(Group!=0)%>%
  mutate(Branch="Upper House")

pred_data_4 <- rbind(pred_data_4_l,pred_data_4_e,pred_data_4_s)

pdf("Graphs/ResponsivenessCitPol.pdf", width=7, height=6)
pred_data_4$PMP <- factor(pred_data_4$PMP, levels=paste("PMP",1:13))
ResponsivenessCitPol <- ggplot(pred_data_4, aes(x=Group, y=pred, pch=Branch)) +
  facet_wrap(~PMP, ncol=3)+
  geom_smooth(method = "loess", span=2, se=FALSE,col="gray70") +
  geom_point( cex=2.7,position=position_dodge(0.3)) +
  theme_bw() +
  ylab("Predicted Probability of Change in the Same Direction\n") +
  scale_x_continuous("(Weak <--) Electoral System Group (--> Strong)", breaks = 1:6)
ResponsivenessCitPol
dev.off()
ResponsivenessCitPol

texreg(list(resp_model),
       custom.coef.names = c("Intercept",
                             "Executive Party",
                             "Median Legislative Party",
                             "Inflation",
                             "Unemployment",
                             "GDP growth",
                             "PMP",
                             "Elec. Group of Lower",
                             "Elec. Group of President",
                             "Elec. Group of Upper",
                             "PMP x Elec. Group of Lower",
                             "PMP x Elec. Group of President",
                             "PMP x Elec. Group of Upper"),
       booktabs = TRUE,
       dcolumn = TRUE,
       stars = .1,
       include.aic = TRUE,
       digits = 2)




round(prop.table(xtabs(~ImprovedOOCong + sameDirChange, data=cit_data)), 2)
round(prop.table(xtabs(~ImprovedMOCong + sameDirChange, data=cit_data)), 2)
