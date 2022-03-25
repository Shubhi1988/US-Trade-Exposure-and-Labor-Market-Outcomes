rm(list = ls())

#Option to prevent scientific notation i.e to prevent numbers in exponential format
options(scipen=999)

#install.packages("stringr")
library(rjson)
library(stringr)
library(readstata13)
library(data.table)
library("AER")
library("Hmisc")

load("main_data_czone.Rdata")
load("main_data_industry.Rdata")
workfile_china <- read.dta13("workfile_china.dta")
czone_pop_1990_2012 <- read.dta13("czone_pop_1990_2012.dta")


#Doing the stats for IPUMS Data
load("czone_data_pop_stats.RData")
load("czone_data_pop_stats_decadal.RData")

load("emp_czone_data_1990.RData")
load("emp_czone_data_2000.RData")
load("emp_czone_data_2007.RData")
load("emp_czone_data_2011.RData")

czone_data <- rbind(emp_czone_data_1990, emp_czone_data_2000, emp_czone_data_2007, emp_czone_data_2011)


descriptive_stats_level <- as.data.frame(matrix(rep(0, 9*105), nrow = 105))
colnames(descriptive_stats_level) <- c("variable", "mean_1990", "sd_1990","mean_2000", "sd_2000", "mean_2007", "sd_2007","mean_2011", "sd_2011")
descriptive_stats_level$variable <- as.vector(colnames(czone_data)[3:107])
year <- c("1990", "2000", "2007","2011")
for(j in 1:4){
  dt <- as.data.table(czone_data)[yr == year[j]]
  dt <- as.data.frame(dt)
  for(i in 3:107){
  descriptive_stats_level[i-2,2*j] <- round(wtd.mean(dt[,i], dt[,108]),2)
  descriptive_stats_level[i-2,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,108])),2)
  }
}


descriptive_stats_diff <- as.data.frame(matrix(rep(0, 7*62), nrow = 62))
colnames(descriptive_stats_diff) <- c("variable", "mean_1", "sd_1","mean_2", "sd_2", "mean_3", "sd_3")
descriptive_stats_diff$variable <- as.vector(colnames(czone_data_pop_stats)[4:65])
p <- c("1991_1999", "1999_2007", "1999_2011")
for(j in 1:3){
  dt <- as.data.table(czone_data_pop_stats)[period == p[j]]
  dt <- as.data.frame(dt)
  for(i in 4:65){
    descriptive_stats_diff[i-3,2*j] <- round(wtd.mean(dt[,i], dt[,66]),2)
    descriptive_stats_diff[i-3,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,66])),2)
  }
}

descriptive_stats_level <- descriptive_stats_level[,c(1,2,4,6,8,3,5,7,9)]
descriptive_stats_diff <- descriptive_stats_diff[,c(1,2,4,6,3,5,7)]

write.table(descriptive_stats_level, file="descriptive_stats_level.csv")
write.table(descriptive_stats_diff, file="descriptive_stats_diff.csv")

library(ggplot2)
ggplot(data_imp_exp_US_yr, aes(x=year)) + 
  #geom_line(aes(y = real_imports_US), color = "red") +
  #geom_line(aes(y = real_imports_US_CH), color="orange")+
  geom_line(aes(y = real_imports_US_CH_mfg), color="green")+
  #geom_line(aes(y = real_imports_US_mfg), color="yellow")+
  #geom_line(aes(y = real_exports_US), color="brown")+
  geom_line(aes(y = real_exports_US_mfg), color="blue")



#Doing the stats for BLS Data
unempl_labor_force <- read.csv('unempl_labor_force.csv', header = TRUE, stringsAsFactors = FALSE)

#Preparing data at the commuting zone level 
unemployment_czone <- subset(unempl_labor_force, year == 1991 | year==1999 | year ==2007 | year ==2011)
colnames(unemployment_czone)[3] <- c("cty_fips")
unemployment_czone <- subset(unemployment_czone, cty_fips <= 56045)

cw_cty_czone_mod <- read.dta13("cw_cty_czone_mod.dta")
unemployment_czone <- merge(unemployment_czone, cw_cty_czone_mod , by = "cty_fips", all.x = TRUE)
unemployment_czone <- subset(unemployment_czone, select = c("czone","year","employed","unemployed"))

unemployment_czone$employed <- as.numeric(unemployment_czone$employed)
unemployment_czone$unemployed <- as.numeric(unemployment_czone$unemployed)

czone_list <-as.vector(unique(main_data_czone$czone))

unemployment_czone <- subset(unemployment_czone,czone %in% czone_list)

unemployment_czone <- aggregate(.~ czone+year, data=unemployment_czone, FUN = "sum")
unemployment_czone <- merge(unemployment_czone, czone_pop_1990_2012 , by = c("czone","year"), all.x = TRUE)
unemployment_czone$nilf <- unemployment_czone$workagepop - unemployment_czone$employed - unemployment_czone$unemployed
unemployment_czone_w <- reshape(unemployment_czone,
                                direction = "wide",
                                timevar = c("year"),
                                idvar = "czone",
                                sep = "_")

#Creating the dependent variable change in unemployment/working age population ratio
unemployment_czone_w$unemppop_1991 <-100 * (unemployment_czone_w$unemployed_1991/unemployment_czone_w$workagepop_1991)
unemployment_czone_w$unemppop_1999 <-100 * (unemployment_czone_w$unemployed_1999/unemployment_czone_w$workagepop_1999)
unemployment_czone_w$unemppop_2007 <-100 * (unemployment_czone_w$unemployed_2007/unemployment_czone_w$workagepop_2007)
unemployment_czone_w$unemppop_2011 <-100 * (unemployment_czone_w$unemployed_2011/unemployment_czone_w$workagepop_2011)

# unemployment_czone_w$unemppop_1990 <-100 * (unemployment_czone_w$unemployed_1990/unemployment_czone_w$workagepop_1990)
# unemployment_czone_w$unemppop_2000 <-100 * (unemployment_czone_w$unemployed_2000/unemployment_czone_w$workagepop_2000)
# unemployment_czone_w$unemppop_2007 <-100 * (unemployment_czone_w$unemployed_2007/unemployment_czone_w$workagepop_2007)
# unemployment_czone_w$unemppop_2011 <-100 * (unemployment_czone_w$unemployed_2011/unemployment_czone_w$workagepop_2011)
# 
# unemployment_czone_w <- unemployment_czone_w[,c(1,22:25)]
# bls_czone_level <- reshape(unemployment_czone_w,
#                            direction = "long",
#                            varying = colnames(unemployment_czone_w)[2:5],
#                            #timevar = c("year"),
#                            idvar = "czone",
#                            sep = "_")
# rownames(bls_czone_level) <- NULL

unemployment_czone_w$emppop_1991 <-100 * (unemployment_czone_w$employed_1991/unemployment_czone_w$workagepop_1991)
unemployment_czone_w$emppop_1999 <-100 * (unemployment_czone_w$employed_1999/unemployment_czone_w$workagepop_1999)
unemployment_czone_w$emppop_2007 <-100 * (unemployment_czone_w$employed_2007/unemployment_czone_w$workagepop_2007)
unemployment_czone_w$emppop_2011 <-100 * (unemployment_czone_w$employed_2011/unemployment_czone_w$workagepop_2011)


unemployment_czone_w$nilfpop_1991 <-100 * (unemployment_czone_w$nilf_1991/unemployment_czone_w$workagepop_1991)
unemployment_czone_w$nilfpop_1999 <-100 * (unemployment_czone_w$nilf_1999/unemployment_czone_w$workagepop_1999)
unemployment_czone_w$nilfpop_2007 <-100 * (unemployment_czone_w$nilf_2007/unemployment_czone_w$workagepop_2007)
unemployment_czone_w$nilfpop_2011 <-100 * (unemployment_czone_w$nilf_2011/unemployment_czone_w$workagepop_2011)

unemployment_czone_w$dunemppop_1 <- (unemployment_czone_w$unemppop_1999 - unemployment_czone_w$unemppop_1991)/(1999-1991)
unemployment_czone_w$dunemppop_2 <- (unemployment_czone_w$unemppop_2007 - unemployment_czone_w$unemppop_1999)/(2007-1999)
unemployment_czone_w$dunemppop_3 <- (unemployment_czone_w$unemppop_2011 - unemployment_czone_w$unemppop_1999)/(2011-1999)

unemployment_czone_w$demppop_1 <- (unemployment_czone_w$emppop_1999 - unemployment_czone_w$emppop_1991)/(1999-1991)
unemployment_czone_w$demppop_2 <- (unemployment_czone_w$emppop_2007 - unemployment_czone_w$emppop_1999)/(2007-1999)
unemployment_czone_w$demppop_3 <- (unemployment_czone_w$emppop_2011 - unemployment_czone_w$emppop_1999)/(2011-1999)

unemployment_czone_w$dnilf_1 <- (unemployment_czone_w$nilfpop_1999 - unemployment_czone_w$nilfpop_1991)/(1999-1991)
unemployment_czone_w$dnilf_2 <- (unemployment_czone_w$nilfpop_2007 - unemployment_czone_w$nilfpop_1999)/(2007-1999)
unemployment_czone_w$dnilf_3 <- (unemployment_czone_w$nilfpop_2011 - unemployment_czone_w$nilfpop_1999)/(2011-1999)

bls_czone_level <- unemployment_czone_w[,c(1:33)]
bls_czone_level <- reshape(bls_czone_level,
                                direction = "long",
                                varying = colnames(bls_czone_level)[2:33],
                                #timevar = c("year"),
                                idvar = "czone",
                                sep = "_")
rownames(bls_czone_level) <- NULL

bls_czone_diff <- unemployment_czone_w[,c(1, 34:42)]
bls_czone_diff <- reshape(bls_czone_diff,
                           direction = "long",
                           varying = colnames(bls_czone_diff)[2:10],
                           #timevar = c("year"),
                           idvar = "czone",
                           sep = "_")
rownames(bls_czone_diff) <- NULL

bls_czone_diff$period <-ifelse(bls_czone_diff$time == 1, "1991_1999", bls_czone_diff$time)
bls_czone_diff$period <-ifelse(bls_czone_diff$time == 2, "1999_2007", bls_czone_diff$period)
bls_czone_diff$period <-ifelse(bls_czone_diff$time == 3, "1999_2011", bls_czone_diff$period)

bls_czone_diff$time <- as.numeric(substr(bls_czone_diff$period, 1, 4))

popcount <- bls_czone_level[,c(1,2,5)]

bls_czone_diff <- merge(bls_czone_diff, popcount, by= c("czone", "time"), all.x =T)
bls_czone_level <- bls_czone_level[, c(1,2,5,3,4,6:10)]

descriptive_stats_level <- as.data.frame(matrix(rep(0, 9*7), nrow = 7))
colnames(descriptive_stats_level) <- c("variable", "mean_1991", "sd_1991","mean_1999", "sd_1999", "mean_2007", "sd_2007","mean_2011", "sd_2011")
descriptive_stats_level$variable <- as.vector(colnames(bls_czone_level)[4:10])
year <- c(1991, 1999, 2007,2011)
for(j in 1:4){
  dt <- as.data.table(bls_czone_level)[time == year[j]]
  dt <- as.data.frame(dt)
  for(i in 4:10){
    descriptive_stats_level[i-3,2*j] <- round(wtd.mean(dt[,i], dt[,3]),2)
    descriptive_stats_level[i-3,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,3])),2)
  }
}

descriptive_stats_diff <- as.data.frame(matrix(rep(0, 7*3), nrow = 3))
colnames(descriptive_stats_diff) <- c("variable", "mean_1", "sd_1","mean_2", "sd_2", "mean_3", "sd_3")
descriptive_stats_diff$variable <- as.vector(colnames(bls_czone_diff)[3:5])
p <- c("1991_1999", "1999_2007", "1999_2011")
for(j in 1:3){
  dt <- as.data.table(bls_czone_diff)[period == p[j]]
  dt <- as.data.frame(dt)
  for(i in 3:5){
    descriptive_stats_diff[i-2,2*j] <- round(wtd.mean(dt[,i], dt[,7]),2)
    descriptive_stats_diff[i-2,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,7])),2)
  }
}


descriptive_stats_level <- descriptive_stats_level[,c(1,2,4,6,8,3,5,7,9)]
descriptive_stats_diff <- descriptive_stats_diff[,c(1,2,4,6,3,5,7)]



#Doing the stats using the CBP Data

cbp_czone_merged <- read.dta13("cbp_czone_merged.dta")

cbp_czone_merged$manuf_ind <- ifelse(cbp_czone_merged$sic87dd>=2011 & cbp_czone_merged$sic87dd <=3999,"mfg","nmfg")
cbp_czone_merged <- cbp_czone_merged[, -c(2)]

cbp_czone_merged <- subset(cbp_czone_merged, year==1991 | year==1999 | year==2007 | year==2011)

cbp_czone_merged <- aggregate(.~ czone+year+manuf_ind, cbp_czone_merged, FUN = "sum" )

cbp_czone_merged <- reshape(cbp_czone_merged,
                            direction = "wide",
                            timevar = c("manuf_ind"),
                            idvar = c("czone","year"),
                            sep = "")

cbp_czone_merged$emp <- cbp_czone_merged$empmfg +cbp_czone_merged$empnmfg
czone_pop_1990_2012 <- read.dta13("czone_pop_1990_2012.dta")

emp_czone <- merge(cbp_czone_merged, czone_pop_1990_2012 , by = c("czone","year"), all.x = TRUE)

emp_czone_w <- reshape(emp_czone,
                       direction = "wide",
                       timevar = c("year"),
                       idvar = c("czone"),
                       sep = "_")

#Creating the dependent variable change in unemployment/working age population ratio
emp_czone_w$emppop_1991 <-100 * (emp_czone_w$emp_1991/emp_czone_w$workagepop_1991)
emp_czone_w$emppop_1999 <-100 * (emp_czone_w$emp_1999/emp_czone_w$workagepop_1999)
emp_czone_w$emppop_2007 <-100 * (emp_czone_w$emp_2007/emp_czone_w$workagepop_2007)
emp_czone_w$emppop_2011 <-100 * (emp_czone_w$emp_2011/emp_czone_w$workagepop_2011)

emp_czone_w$empmfgpop_1991 <-100 * (emp_czone_w$empmfg_1991/emp_czone_w$workagepop_1991)
emp_czone_w$empmfgpop_1999 <-100 * (emp_czone_w$empmfg_1999/emp_czone_w$workagepop_1999)
emp_czone_w$empmfgpop_2007 <-100 * (emp_czone_w$empmfg_2007/emp_czone_w$workagepop_2007)
emp_czone_w$empmfgpop_2011 <-100 * (emp_czone_w$empmfg_2011/emp_czone_w$workagepop_2011)

emp_czone_w$empnmfgpop_1991 <-100 * (emp_czone_w$empnmfg_1991/emp_czone_w$workagepop_1991)
emp_czone_w$empnmfgpop_1999 <-100 * (emp_czone_w$empnmfg_1999/emp_czone_w$workagepop_1999)
emp_czone_w$empnmfgpop_2007 <-100 * (emp_czone_w$empnmfg_2007/emp_czone_w$workagepop_2007)
emp_czone_w$empnmfgpop_2011 <-100 * (emp_czone_w$empnmfg_2011/emp_czone_w$workagepop_2011)

emp_czone_w$demppop_1 <- (emp_czone_w$emppop_1999 - emp_czone_w$emppop_1991)/(1999-1991)
emp_czone_w$demppop_2 <- (emp_czone_w$emppop_2007 - emp_czone_w$emppop_1999)/(2007-1999)
emp_czone_w$demppop_3 <- (emp_czone_w$emppop_2011 - emp_czone_w$emppop_1999)/(2011-1999)

emp_czone_w$dempmfgpop_1 <- (emp_czone_w$empmfgpop_1999 - emp_czone_w$empmfgpop_1991)/(1999-1991)
emp_czone_w$dempmfgpop_2 <- (emp_czone_w$empmfgpop_2007 - emp_czone_w$empmfgpop_1999)/(2007-1999)
emp_czone_w$dempmfgpop_3 <- (emp_czone_w$empmfgpop_2011 - emp_czone_w$empmfgpop_1999)/(2011-1999)

emp_czone_w$dempnmfgpop_1 <- (emp_czone_w$empnmfgpop_1999 - emp_czone_w$empnmfgpop_1991)/(1999-1991)
emp_czone_w$dempnmfgpop_2 <- (emp_czone_w$empnmfgpop_2007 - emp_czone_w$empnmfgpop_1999)/(2007-1999)
emp_czone_w$dempnmfgpop_3 <- (emp_czone_w$empnmfgpop_2011 - emp_czone_w$empnmfgpop_1999)/(2011-1999)

cbp_czone_level <- emp_czone_w[,c(1:33)]
cbp_czone_level <- reshape(cbp_czone_level,
                           direction = "long",
                           varying = colnames(cbp_czone_level)[2:33],
                           #timevar = c("year"),
                           idvar = "czone",
                           sep = "_")
rownames(cbp_czone_level) <- NULL

cbp_czone_diff <- emp_czone_w[,c(1, 34:42)]
cbp_czone_diff <- reshape(cbp_czone_diff,
                          direction = "long",
                          varying = colnames(cbp_czone_diff)[2:10],
                          #timevar = c("year"),
                          idvar = "czone",
                          sep = "_")
rownames(cbp_czone_diff) <- NULL

cbp_czone_diff$period <-ifelse(cbp_czone_diff$time == 1, "1991_1999", cbp_czone_diff$time)
cbp_czone_diff$period <-ifelse(cbp_czone_diff$time == 2, "1999_2007", cbp_czone_diff$period)
cbp_czone_diff$period <-ifelse(cbp_czone_diff$time == 3, "1999_2011", cbp_czone_diff$period)

cbp_czone_diff$time <- as.numeric(substr(cbp_czone_diff$period, 1, 4))

popcount <- cbp_czone_level[,c(1,2,6)]

cbp_czone_diff <- merge(cbp_czone_diff, popcount, by= c("czone", "time"), all.x =T)
cbp_czone_level <-cbp_czone_level[,c(1,2,6, 3:5,7:10)]

descriptive_stats_level <- as.data.frame(matrix(rep(0, 9*7), nrow = 7))
colnames(descriptive_stats_level) <- c("variable", "mean_1991", "sd_1991","mean_1999", "sd_1999", "mean_2007", "sd_2007","mean_2011", "sd_2011")
descriptive_stats_level$variable <- as.vector(colnames(cbp_czone_level)[4:10])
year <- c(1991, 1999, 2007,2011)
for(j in 1:4){
  dt <- as.data.table(cbp_czone_level)[time == year[j]]
  dt <- as.data.frame(dt)
  for(i in 4:10){
    descriptive_stats_level[i-3,2*j] <- round(wtd.mean(dt[,i], dt[,3]),2)
    descriptive_stats_level[i-3,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,3])),2)
  }
}

descriptive_stats_diff <- as.data.frame(matrix(rep(0, 7*3), nrow = 3))
colnames(descriptive_stats_diff) <- c("variable", "mean_1", "sd_1","mean_2", "sd_2", "mean_3", "sd_3")
descriptive_stats_diff$variable <- as.vector(colnames(cbp_czone_diff)[3:5])
p <- c("1991_1999", "1999_2007", "1999_2011")
for(j in 1:3){
  dt <- as.data.table(cbp_czone_diff)[period == p[j]]
  dt <- as.data.frame(dt)
  for(i in 3:5){
    descriptive_stats_diff[i-2,2*j] <- round(wtd.mean(dt[,i], dt[,7]),2)
    descriptive_stats_diff[i-2,2*j+1] <- round(sqrt(wtd.var(dt[,i], dt[,7])),2)
  }
}

descriptive_stats_level <- descriptive_stats_level[,c(1,2,4,6,8,3,5,7,9)]
descriptive_stats_diff <- descriptive_stats_diff[,c(1,2,4,6,3,5,7)]

#Doing the stats for ADH data 

workfile_china_1 <- subset(workfile_china, t2==1)
xm <- round(wtd.mean(workfile_china_1$d_tradeotch_pw_lag, workfile_china_1$l_popcount)/10,2)

sd <- round(sqrt(wtd.var(workfile_china_1$d_tradeotch_pw_lag, workfile_china_1$l_popcount))/10,2)

#Doing the stats on Feenstra data
main_data_czone$time <- as.numeric(substr(main_data_czone$period,1,4))
main_data_czone <- merge(main_data_czone, popcount, by= c("czone","time"), all.x = T)
colnames(main_data_czone)[53] <- "l_popcount"
main_data_czone_1 <- subset(main_data_czone, period=="1991_1999")

xm <- round(wtd.mean(main_data_czone_1$shock_PREexp, main_data_czone_1$l_popcount),2)

sd <- round(sqrt(wtd.var(main_data_czone_1$shock_PREexp, main_data_czone_1$l_popcount)),2)

#Calculating quantiles

main_data_czone$year <- as.numeric(substr(main_data_czone$period, 1,4))
main_data_czone <- merge(main_data_czone, czone_pop_1990_2012, by =c("czone","year"), all.x = T)
colnames(main_data_czone)[53] <- "l_popcount"

data1 <- as.data.table(main_data_czone)[period =="1991_1999"]
data2 <- as.data.table(main_data_czone)[period =="1999_2007"]
data3 <- as.data.table(main_data_czone)[period =="1999_2011"]

# Construct quantiles for each decade
t1_90 <- round(wtd.quantile(data1$shock_us, data1$l_popcount, probs = 0.90, na.rm = TRUE),2)
t1_75 <- round(wtd.quantile(data1$shock_us, data1$l_popcount, probs = 0.75, na.rm = TRUE),2)
t1_50 <- round(wtd.quantile(data1$shock_us, data1$l_popcount, probs = 0.50, na.rm = TRUE),2)
t1_25 <- round(wtd.quantile(data1$shock_us, data1$l_popcount, probs = 0.25, na.rm = TRUE),2)
t1_10 <- round(wtd.quantile(data1$shock_us, data1$l_popcount, probs = 0.10, na.rm = TRUE),2)

t2_90 <- round(wtd.quantile(data2$shock_us, data2$l_popcount, probs = 0.90, na.rm = TRUE),2)
t2_75 <- round(wtd.quantile(data2$shock_us, data2$l_popcount, probs = 0.75, na.rm = TRUE),2)
t2_50 <- round(wtd.quantile(data2$shock_us, data2$l_popcount, probs = 0.50, na.rm = TRUE),2)
t2_25 <- round(wtd.quantile(data2$shock_us, data2$l_popcount, probs = 0.25, na.rm = TRUE),2)
t2_10 <- round(wtd.quantile(data2$shock_us, data2$l_popcount, probs = 0.10, na.rm = TRUE),2)

t3_90 <- round(wtd.quantile(data3$shock_us, data3$l_popcount, probs = 0.90, na.rm = TRUE),2)
t3_75 <- round(wtd.quantile(data3$shock_us, data3$l_popcount, probs = 0.75, na.rm = TRUE),2)
t3_50 <- round(wtd.quantile(data3$shock_us, data3$l_popcount, probs = 0.50, na.rm = TRUE),2)
t3_25 <- round(wtd.quantile(data3$shock_us, data3$l_popcount, probs = 0.25, na.rm = TRUE),2)
t3_10 <- round(wtd.quantile(data3$shock_us, data3$l_popcount, probs = 0.10, na.rm = TRUE),2)

quantiles_imp <- cbind(rbind(t1_90, t1_75, t1_50, t1_25, t1_10), 
                       rbind(t2_90, t2_75, t2_50, t2_25, t2_10), 
                       rbind(t3_90, t3_75, t3_50, t3_25, t3_10))

colnames(quantiles_imp) <- c("t1","t2","t3")
rownames(quantiles_imp) <- c("90","75","50","25","10")

t1_90 <- round(wtd.quantile(data1$shock_USexp, data1$l_popcount, probs = 0.90, na.rm = TRUE),2)
t1_75 <- round(wtd.quantile(data1$shock_USexp, data1$l_popcount, probs = 0.75, na.rm = TRUE),2)
t1_50 <- round(wtd.quantile(data1$shock_USexp, data1$l_popcount, probs = 0.50, na.rm = TRUE),2)
t1_25 <- round(wtd.quantile(data1$shock_USexp, data1$l_popcount, probs = 0.25, na.rm = TRUE),2)
t1_10 <- round(wtd.quantile(data1$shock_USexp, data1$l_popcount, probs = 0.10, na.rm = TRUE),2)

t2_90 <- round(wtd.quantile(data2$shock_USexp, data2$l_popcount, probs = 0.90, na.rm = TRUE),2)
t2_75 <- round(wtd.quantile(data2$shock_USexp, data2$l_popcount, probs = 0.75, na.rm = TRUE),2)
t2_50 <- round(wtd.quantile(data2$shock_USexp, data2$l_popcount, probs = 0.50, na.rm = TRUE),2)
t2_25 <- round(wtd.quantile(data2$shock_USexp, data2$l_popcount, probs = 0.25, na.rm = TRUE),2)
t2_10 <- round(wtd.quantile(data2$shock_USexp, data2$l_popcount, probs = 0.10, na.rm = TRUE),2)

t3_90 <- round(wtd.quantile(data3$shock_USexp, data3$l_popcount, probs = 0.90, na.rm = TRUE),2)
t3_75 <- round(wtd.quantile(data3$shock_USexp, data3$l_popcount, probs = 0.75, na.rm = TRUE),2)
t3_50 <- round(wtd.quantile(data3$shock_USexp, data3$l_popcount, probs = 0.50, na.rm = TRUE),2)
t3_25 <- round(wtd.quantile(data3$shock_USexp, data3$l_popcount, probs = 0.25, na.rm = TRUE),2)
t3_10 <- round(wtd.quantile(data3$shock_USexp, data3$l_popcount, probs = 0.10, na.rm = TRUE),2)

quantiles_exp <- cbind(rbind(t1_90, t1_75, t1_50, t1_25, t1_10), 
                       rbind(t2_90, t2_75, t2_50, t2_25, t2_10), 
                       rbind(t3_90, t3_75, t3_50, t3_25, t3_10))

colnames(quantiles_exp) <- c("t1","t2","t3")
rownames(quantiles_exp) <- c("90","75","50","25","10")

test <- merge(czone_data_pop_stats, bls_czone_diff, by =c("czone","period"), all.x = T)
test1 <- subset(test, period != "1999_2011")
test2 <- subset(test, period != "1999_2007")

cor(test1$d_sh_unempl, test1$dunemppop)
#0.4945918
cor(test2$d_sh_unempl, test2$dunemppop)
#0.7818201

test <- czone_data[,c(1,2,60)]
test1 <- bls_czone_level[,c(1,2,8)]
colnames(test1)[2] <- "yr"
test1$yr <- ifelse(test1$yr == 1991, 1990, test1$yr)
test1$yr <- ifelse(test1$yr == 1999, 2000, test1$yr)
test <- merge(test, test1, by =c("czone","yr"), all.x = T)
test2 <- subset(test, yr == 2011)
cor(test2$l_sh_unempl, test2$unemppop)
#0.7247897

test <- czone_data[,c(1,2,60)]
test1 <- bls_czone_level[,c(1,2,3)]
colnames(test1)[2] <- "yr"
test <- merge(test, test1, by =c("czone","yr"), all.x = T)

cor(test$l_sh_unempl, test$unemppop)
#0.7754646