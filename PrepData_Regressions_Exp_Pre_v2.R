rm(list = ls())

#Option to prevent scientific notation i.e to prevent numbers in exponential format
options(scipen=999)

#install.packages("stringr")
library(rjson)
library(stringr)
library(readstata13)
library(data.table)
library("AER")
library(stargazer)

load("main_data_czone.Rdata")
load("main_data_industry.Rdata")

load("czone_data_wage_stats_adh.RData")
load("czone_data_pop_stats_adh.RData")
load("czone_data_pop_stats_cbp.RData")
load("czone_data_pop_stats.RData")
czone_pop_1990_2012 <- read.dta13("czone_pop_1990_2012.dta")
cbp_czone_merged <- read.dta13("cbp_czone_merged.dta")

load("emp_czone_data_fibk.RData")
main_data_czone <- subset(main_data_czone, select = c("czone", "period", "pop1991", "t2", "d_emppop", "d_mfgpop","d_nmfgpop","mfgsh","d_tradeusch_pw","d_tradeotch_pw_lag", "region2", "region3","region4", "region5","region6", "region7","region8", "region9","l_sh_popedu_c" ,"l_sh_popfborn" ,"l_sh_empl_f" ,"l_sh_routine33" ,"l_task_outsource","shock_us" ,"shock_USexp" ,"shock_ot", "shock_OTHexp", "shock_PREexp", "workagepop1991","workagepop1999" ,"workagepop2007","workagepop2011"))

main_data_czone <- merge(main_data_czone, czone_data_wage_stats_adh , by = c("czone","period"), all.x = TRUE)
main_data_czone <- merge(main_data_czone, czone_data_pop_stats , by = c("czone","period"), all.x = TRUE)

colnames(main_data_czone)[29] <- "yr"
main_data_czone <- merge(main_data_czone, emp_czone_data_fibk , by = c("czone","yr"), all.x = TRUE)

main_data_czone_1991_2007 <- subset(main_data_czone, period != "1999_2011")
main_data_czone_1991_2011 <- subset(main_data_czone, period != "1999_2007")

reg_iv_1 <-ivreg(lnchg_popworkage ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(lnchg_popworkage_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

# reg_iv_21 <-ivreg(lnchg_popworkage_edu_sc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
#                    t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
#                  data = main_data_czone_1991_2011,weights=pop1991)
# summary(reg_iv_21)
# robust_se21 <-coeftest(reg_iv_21, vcov = vcovHC(reg_iv_21,type="HC1", cluster = "czone"))[,2]


reg_iv_3 <-ivreg(lnchg_popworkage_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_popworkage_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(lnchg_popworkage_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(lnchg_popworkage_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2, reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")

reg_iv_1 <-ivreg(lnchg_no_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_no_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_no_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_no_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=wt)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

main_data_czone_1991_2007_v2 <- subset(main_data_czone_1991_2007, lnchg_no_unempl_mfg != -Inf)
reg_iv_5 <-ivreg(lnchg_no_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011_v2,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(lnchg_no_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(lnchg_no_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")



reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

test <- main_data_czone_1991_2011
pfactor = 0.56
test$pred_emp <-fitted(reg_iv_1,test)
test$lambda <- ifelse(test$period == "1991_1999",100 / (1999 - 1991),0)
test$lambda <- ifelse(test$period == "1999_2011",100 / (2011 - 1999),test$lambda)
test$pop <- ifelse(test$period == "1991_1999",test$workagepop1999,0)
test$pop <- ifelse(test$period == "1999_2011",test$workagepop2011,test$pop)
test$pred_emp_2 <- test$pop*(1/test$lambda)*coef(reg_iv_1)[[17]]*pfactor*test$shock_us
sum(test$pred_emp_2)
#-2040025
test$pred_emp_2 <- test$pop*(1/test$lambda)*coef(reg_iv_1)[[18]]*pfactor*test$shock_USexp
sum(test$pred_emp_2)
#2155846

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

test <- main_data_czone_1991_2011
pfactor = 0.56
test$pred_unemp <-fitted(reg_iv_4,test)
test$lambda <- ifelse(test$period == "1991_1999",100 / (1999 - 1991),0)
test$lambda <- ifelse(test$period == "1999_2011",100 / (2011 - 1999),test$lambda)
test$pop <- ifelse(test$period == "1991_1999",test$workagepop1999,0)
test$pop <- ifelse(test$period == "1999_2011",test$workagepop2011,test$pop)
test$pred_unemp_2 <- test$pop*(1/test$lambda)*coef(reg_iv_4)[[17]]*pfactor*test$shock_us
sum(test$pred_unemp_2)
#565710
test$pred_unemp_2 <- test$pop*(1/test$lambda)*coef(reg_iv_4)[[18]]*pfactor*test$shock_USexp
sum(test$pred_unemp_2)
#-627035.5

reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")


reg_iv_1 <-ivreg(d_sh_empl_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(d_sh_unempl_mfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")

reg_iv_1 <-ivreg(d_sh_empl_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl_edu_sc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(d_sh_unempl_mfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")

reg_iv_1 <-ivreg(d_sh_empl_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf_age1634 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")



reg_iv_1 <-ivreg(d_sh_empl_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf_age3549 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")


reg_iv_1 <-ivreg(d_sh_empl_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf_age5064 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")




reg_iv_1 <-ivreg(lnchg_popworkage_age1634_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(lnchg_popworkage_age3549_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_popworkage_age5064_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

# reg_iv_11 <-ivreg(lnchg_popworkage_age1634_edu_sc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
#                    t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
#                  data = main_data_czone_1991_2011,weights=pop1991)
# summary(reg_iv_11)
# robust_se11 <-coeftest(reg_iv_11, vcov = vcovHC(reg_iv_11,type="HC1", cluster = "czone"))[,2]

# reg_iv_21 <-ivreg(lnchg_popworkage_age3549_edu_sc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
#                    t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
#                  data = main_data_czone_1991_2011,weights=pop1991)
# summary(reg_iv_21)
# robust_se21 <-coeftest(reg_iv_21, vcov = vcovHC(reg_iv_21,type="HC1", cluster = "czone"))[,2]

# reg_iv_31 <-ivreg(lnchg_popworkage_age5064_edu_sc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
#                    t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
#                  data = main_data_czone_1991_2011,weights=pop1991)
# summary(reg_iv_31)
# robust_se31 <-coeftest(reg_iv_31, vcov = vcovHC(reg_iv_31,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_popworkage_age1634_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(lnchg_popworkage_age3549_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(lnchg_popworkage_age5064_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")

reg_iv_1 <-ivreg(lnchg_wage ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(lnchg_wage_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp| 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(lnchg_wage_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5),out="regressions.tex")



reg_iv_1 <-ivreg(lnchg_wage_mfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us+shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_wage_mfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp| 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_nmfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_nmfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4, se=list(robust_se1, robust_se2, robust_se3, robust_se4),out="regressions.tex")


reg_iv_1 <-ivreg(lnchg_wage_age_1 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us+shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_wage_age_2 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp| 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_age_3 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3, se=list(robust_se1, robust_se2, robust_se3),out="regressions.tex")


reg_iv_1 <-ivreg(lnchg_wage_mfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us+shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_wage_mfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp| 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_ind_edu_age_111 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_ind_edu_age_112 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(lnchg_wage_ind_edu_age_113 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(lnchg_wage_ind_edu_age_101 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(lnchg_wage_ind_edu_age_102 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

reg_iv_8 <-ivreg(lnchg_wage_ind_edu_age_103 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_8)
robust_se8 <-coeftest(reg_iv_8, vcov = vcovHC(reg_iv_8,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, reg_iv_8, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7, robust_se8),out="regressions.tex")

reg_iv_1 <-ivreg(lnchg_wage_nmfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us+shock_USexp   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_wage_nmfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp| 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot + shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_ind_edu_age_011 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_ind_edu_age_012 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(lnchg_wage_ind_edu_age_013 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(lnchg_wage_ind_edu_age_001 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(lnchg_wage_ind_edu_age_002 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

reg_iv_8 <-ivreg(lnchg_wage_ind_edu_age_003 ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_8)
robust_se8 <-coeftest(reg_iv_8, vcov = vcovHC(reg_iv_8,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, reg_iv_8, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7, robust_se8),out="regressions.tex")












  

reg_iv_1 <-ivreg(lnchg_wage ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us    | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

workfile_china <- read.dta13("workfile_china.dta")

chck <- subset(main_data_czone_1991_2007, select =c("czone", "yr", "lnchg_wage_mfg"))
chck$lnchg_wage_mfg <- ifelse(chck$yr == 1990, chck$lnchg_wage_mfg*10, chck$lnchg_wage_mfg*10)
workfile_china <- merge(workfile_china, chck, by = c("czone","yr"), all.x = T)


reg_iv_1 <-ivreg(lnchg_wage_mfg ~t2+l_shind_manuf_cbp+reg_midatl+ reg_encen+reg_wncen+ reg_satl+reg_escen+ reg_wscen+reg_mount+ reg_pacif+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_tradeusch_pw  |
                   t2+l_shind_manuf_cbp+reg_midatl+ reg_encen+reg_wncen+ reg_satl+reg_escen+ reg_wscen+reg_mount+ reg_pacif+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_tradeotch_pw_lag,
                 data = workfile_china, weights=timepwt48)
summary(reg_iv_1)

reg_iv_2 <-ivreg(lnchg_wage_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]

reg_iv_5 <-ivreg(lnchg_wage_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5),out="regressions.tex")



reg_iv_1 <-ivreg(lnchg_wage_mfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(lnchg_wage_mfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(lnchg_wage_nmfg_edu_c ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(lnchg_wage_nmfg_edu_nc ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us   | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)
robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4, se=list(robust_se1, robust_se2, robust_se3, robust_se4),out="regressions.tex")




czone_trade_shocks_mfgempshare <- read.dta13("czone_trade_shocks_mfgempshare.dta")
czone_trade_shocks_empshare <- read.dta13("czone_trade_shocks_empshare.dta")
czone_trade_shocks <- read.dta13("czone_trade_shocks.dta")


library(tidyr)
czone_trade_shocks_mfgempshare_t=czone_trade_shocks_empshare%>%gather(key = "var",value = 'val',-c('year','czone'))
czone_trade_shocks_mfgempshare_t$var1=substring(czone_trade_shocks_mfgempshare_t$var,nchar(czone_trade_shocks_mfgempshare_t$var)-8)
czone_trade_shocks_mfgempshare_t$var2=substring(czone_trade_shocks_mfgempshare_t$var,1,nchar(czone_trade_shocks_mfgempshare_t$var)-9)
czone_trade_shocks_mfgempshare_t=subset(czone_trade_shocks_mfgempshare_t,select = -var)
czone_trade_shocks_mfgempshare_t=czone_trade_shocks_mfgempshare_t%>%spread(key = 'var2',value = 'val')

czone_trade_shocks_mfgempshare_t <- czone_trade_shocks_mfgempshare_t[,-1]
colnames(czone_trade_shocks_mfgempshare_t)[2] <- "period"

test <- merge(main_data_czone_1991_2011, czone_trade_shocks_mfgempshare_t, by = c("czone","period"), all.x = T)

reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport  +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_PRECHNimport+ d_czone_PREWLDexport, 
                 data = test,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport  + d_czone_ROWimport  +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_2,vcov = sandwich, diagnostics = TRUE)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource +d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")


reg_iv_1 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")

reg_iv_1 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")



test <- merge(main_data_czone_1991_2011, czone_trade_shocks_mfgempshare_t, by = c("czone","period"), all.x = T)
test1 <- merge(main_data_czone_1991_2007, czone_trade_shocks_mfgempshare_t, by = c("czone","period"), all.x = T)
library(foreign)
write.dta(test, "C:/Users/shubh/Dropbox (UFL)/SA_TG_UF/Research2/Regressions/test.dta")
write.dta(test1, "C:/Users/shubh/Dropbox (UFL)/SA_TG_UF/Research2/Regressions/test1.dta")

reg_iv_1 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")


reg_iv_1 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_1,vcov = vcovHC(reg_iv_1, type = "HC1", cluster = "czone"), diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

pfactor = 0.56
test$pred_emp <-fitted(reg_iv_1,test)
test$lambda <- ifelse(test$period == "1991_1999",100 / (1999 - 1991),0)
test$lambda <- ifelse(test$period == "1999_2011",100 / (2011 - 1999),test$lambda)
test$pop <- ifelse(test$period == "1991_1999",test$workagepop1999,0)
test$pop <- ifelse(test$period == "1999_2011",test$workagepop2011,test$pop)
test$pred_emp_2 <- test$pop*(1/test$lambda)*coef(reg_iv_1)[[12]]*pfactor*test$d_czone_CHNimport

reg_iv_2 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_emppop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"), diagnostics = TRUE)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_nmfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")

cor(test$d_czone_ROWimport, test$d_czone_USexport)
cor(test$d_czone_CHNimport, test$d_czone_USexport)

workfile_china_2 <- read.dta13("workfile_china_2.dta")
sic87dd_trade_data <- read.dta13("sic87dd_trade_data.dta")
load("dt_imp_us_adh.RData")

reg_iv_4 <-ivreg(d_mfgpop ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = test,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]




reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+ shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+l_sh_empl_fibk+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+l_sh_empl_fibk+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp+shock_PREexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7, vcov = sandwich, diagnostics = TRUE)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")

reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2, vcov = sandwich, diagnostics = TRUE)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+l_sh_empl_fibk+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+l_sh_empl_fibk+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_OTHexp, 
                 data =main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")





reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")


reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_1,vcov = vcovHC(reg_iv_1, type = "HC1", cluster = "czone"), diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"), diagnostics = TRUE)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]


stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")



reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+d_czone_TARCHN1import+ d_czone_TARROW1import +d_czone_OTROW1import+d_czone_OTCHN1import +d_czone_OTCHN1export+d_czone_TARCHN1export+d_czone_OTROW1export +d_czone_TARROW1export, 
                 data = test1,weights=pop1991)
summary(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"), diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6),out="regressions.tex")


reg_iv_1 <-lm(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+ d_czone_CHNimport + d_czone_ROWimport +d_czone_USexport, 
                 data = test1,weights=pop1991)
summary(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"), diagnostics = TRUE)




reg_iv_1 <-lm(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]

reg_iv_2 <-lm(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-lm(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-lm(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-lm(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-lm(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-lm(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp 
                   , 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")



reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp +d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp+d_czone_ROWimport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")



reg_iv_1 <-ivreg(d_sh_empl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_1, vcov = sandwich, diagnostics = TRUE)
robust_se1 <-coeftest(reg_iv_1, vcov = vcovHC(reg_iv_1,type="HC1", cluster = "czone"))[,2]


reg_iv_2 <-ivreg(d_sh_empl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_2)
robust_se2 <-coeftest(reg_iv_2, vcov = vcovHC(reg_iv_2,type="HC1", cluster = "czone"))[,2]

reg_iv_3 <-ivreg(d_sh_empl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_3)
robust_se3 <-coeftest(reg_iv_3, vcov = vcovHC(reg_iv_3,type="HC1", cluster = "czone"))[,2]

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp +d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_4)

robust_se4 <-coeftest(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"))[,2]


reg_iv_5 <-ivreg(d_sh_unempl_mfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_5)
robust_se5 <-coeftest(reg_iv_5, vcov = vcovHC(reg_iv_5,type="HC1", cluster = "czone"))[,2]

reg_iv_6 <-ivreg(d_sh_unempl_nmfg ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp +d_czone_ROWimport | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_6)
robust_se6 <-coeftest(reg_iv_6, vcov = vcovHC(reg_iv_6,type="HC1", cluster = "czone"))[,2]

reg_iv_7 <-ivreg(d_sh_nilf ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp+d_czone_ROWimport  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp+d_czone_ROWimport, 
                 data = test1,weights=pop1991)
summary(reg_iv_7)
robust_se7 <-coeftest(reg_iv_7, vcov = vcovHC(reg_iv_7,type="HC1", cluster = "czone"))[,2]

stargazer(reg_iv_1, reg_iv_2,reg_iv_3,reg_iv_4,reg_iv_5,reg_iv_6, reg_iv_7, se=list(robust_se1, robust_se2, robust_se3, robust_se4,robust_se5, robust_se6, robust_se7),out="regressions.tex")

reg_iv_4 <-ivreg(d_sh_unempl ~t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_us +shock_USexp  | 
                   t2+mfgsh+region2+ region3+region4+ region5+region6+ region7+region8+ region9+l_sh_popedu_c +l_sh_popfborn +l_sh_empl_f +l_sh_routine33 +l_task_outsource+shock_ot+ shock_PREexp, 
                 data = main_data_czone_1991_2011,weights=pop1991)
summary(reg_iv_4, vcov = vcovHC(reg_iv_4,type="HC1", cluster = "czone"), diagnostics = TRUE)
