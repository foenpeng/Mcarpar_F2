setwd("/Volumes/GoogleDrive/My Drive/Research/Peng_Lab_Shared/Projects/M.carpar/F2_SP22")
library(data.table)
library(ggplot2)

phenotyping<-fread("./Phenotyping/F2_phenotyping_bee_pollination_final.csv", colClasses = c('Plant_ID'='character'))
pollination<-fread("./Pollination/visit_by_plant.csv", colClasses = c('Plant_ID'='character'))
pollination_2<-fread("./Pollination/visit_by_plant_date.csv", colClasses = c('Plant_ID'='character'))
pollen_load<-fread("./Pollination/pollen_load.csv", colClasses = c('Plant_ID'='character'))
seed_count<-fread("./SeedCounts/Mcarpar_F2_seed_counts_final.csv", colClasses = c('Plant_ID'='character'))



seed_count[,avg_seed_production:=Count/Observed_Fruit]

data_to_join<-c("phenotyping","pollination_2","pollen_load","seed_count")
meta_F2<-Reduce(function(x, y) merge(x,y, by = "Plant_ID", all=T, sort=T), mget(data_to_join))

to_norm<-c("R","G","B")
meta_F2[, paste0(to_norm, "_S") := lapply(.SD, scale), .SDcols = to_norm]

require(car)
require(MASS)
require(lme4)
qqp(meta_F2[,visits],"norm")
qqp(meta_F2[,visits],"lnorm")
nbinom <- fitdistr(meta_F2[!is.na(visits),visits], "Negative Binomial")
qqp(meta_F2[!is.na(visits),visits], "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(meta_F2[!is.na(visits),visits], "Poisson")
qqp(meta_F2[!is.na(visits),visits], "pois", lambda=poisson$estimate)


nbglmm<-glmer.nb(visits~No_total_flowers+time_span_date+nec_vol+nec_con+CTW+CTL+prj_area+B+G+dot_size+(1|Date/Group),data=meta_F2)
Anova(nbglmm)
summary(glm.nb(visits~No_total_flowers+Group+time_span_date+nec_vol+nec_con+CTW+CTL+prj_area+B+G+dot_size,data=meta_F2))



data_to_join<-c("phenotyping","pollination","pollen_load","seed_count")
meta_F2_byplant<-Reduce(function(x, y) merge(x,y, by = "Plant_ID", all=T, sort=T), mget(data_to_join))


require(car)
require(MASS)
require(lme4)
qqp(meta_F2[,Count],"norm")
qqp(meta_F2[,avg_seed_production],"lnorm")
nbinom <- fitdistr(meta_F2[!is.na(Count),Count], "Negative Binomial")
qqp(meta_F2[!is.na(Count),Count], "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(meta_F2[!is.na(Count),Count], "Poisson")
qqp(meta_F2[!is.na(Count),Count], "pois", lambda=poisson$estimate)

hist(meta_F2[,Count],breaks=20)
nbglmm<-glmer.nb(Count~Observed_Fruit+pollination_rate+nec_vol+nec_con+CTW+CTL+stm_len+pst_len+reflex+prj_area+B+G+dot_size+stg_close+(1|Group),data=meta_F2_byplant)
Anova(nbglmm)

summary(glm.nb(Count~Observed_Fruit+Group+pollination_rate+nec_vol+nec_con+CTW+CTL+stm_len+pst_len+reflex+prj_area+B+G+dot_size+stg_close,data=meta_F2_byplant))





summary(glm(visits~No_total_flowers+time_span_date+nec_vol+nec_con+reflex+prj_area+R+B+G+dot_size+CTW+CTL+CTW_mid,data=meta_F2,family=poisson()))

summary(lm(pollination_rate~nec_vol+nec_con+prj_area+R+G+B+CTW+CTL+CLL+CLW+pst_len+stm_len+stk_len+tooth_num+stg_close,data=meta_F2))

summary(lm(avg_seed_production~stg_close+nec_vol+nec_con+prj_area+R+G+B+CTW+CTL+CLL+CLW+pst_len+stm_len+stk_len+tooth_num+stg_close,data=meta_F2))

summary(lm(avg_pollen_load~pollination_rate+stg_close+nec_vol+nec_con+prj_area+R+G+B+CTW+CTL+CLL+CLW+pst_len+stm_len+stk_len+tooth_num+stg_close,data=meta_F2))
