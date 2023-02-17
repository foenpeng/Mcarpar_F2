setwd("/Volumes/GoogleDrive/My Drive/Research/Peng_Lab_Shared/Projects/M.carpar/2022/Pollination")
library(data.table)
library(ggplot2)

visit_raw<-fread("./bee_visit_raw_recording_2022_all.csv")

# make sure all records have a date info, if not, find the closest and fill it
visit_raw<-visit_raw[,c("Date"):=tstrsplit(Time, " ", fixed=TRUE)[[1]]][Plant_ID!=""]
visit_raw[Plant_ID=="f1",Plant_ID:="F1"]
visit_raw[,Plant_ID:=as.factor(Plant_ID)]
visit_raw[Time=="",.N]
# fill the ones with no date with the closest value, setnafill only work with numeric.
visit_raw[,nu_date:=as.numeric(tstrsplit(Date,split="/")[[2]])]
setnafill(visit_raw, "locf",col="nu_date")
visit_raw[is.na(Date),Date:=paste0("5/",nu_date,"/2022")]

# correct some typos
visit_raw[Plant_ID=="37",Plant_ID:="137"]
visit_raw[Plant_ID=="13",Plant_ID:="113"]

# load plant group assignment
plant_info<-fread("../Plant_A_B_arrangement_final.csv",drop=3)

# load flower counts each day
filelist = list.files(path = "./", pattern="Plants_info_for_bumblebee_experiments - 5", full.names = T)
files = sapply(filelist, function(x) strsplit(strsplit(strsplit(x,split = "/")[[1]][3],split=" - ")[[1]][2], split="\\.")[[1]][1])
flower_count_raw<- do.call(rbind,Map(cbind, lapply(filelist, data.table::fread, select = c(1:7)), date_group = files))

# get flower counts by plant by date
Date_match<-data.table(Date=unique(visit_raw[!is.na(Date),Date]),to_be_matched=files)
flower_count_raw[,Date:=Date_match[match(date_group,Date_match[,to_be_matched]),Date]]
flower_count_raw[,No_total_flowers:=sum(No_total_flowers),by=.(Plant_ID,Date)]
flower_count<-unique(flower_count_raw[Plant_ID!="",.(No_total_flowers,Plant_ID,Date, Group)],by=c("Plant_ID","Date"))

# get average pollen load per plant
temp<-setDT(melt(flower_count_raw[Plant_ID!="",c(1,6,7)],id="Plant_ID"))
pollen_load<-temp[!is.na(value),.("avg_pollen_load"=mean(value,na.rm=T),"pollen_load_flower_count"=.N),by="Plant_ID"]


visit_by_plant_date<-visit_raw[Plant_ID!="",.("visits"=.N),keyby=list(Plant_ID,Date)]
visit_by_plant_date<-visit_by_plant_date[flower_count,on=c("Plant_ID","Date")]
visit_by_plant_date[is.na(visits)|No_total_flowers==0,visits:=0]
visit_by_plant_date[,pollination_rate:=visits/No_total_flowers]
visit_by_plant_date[,rank:=frank(pollination_rate,na.last="keep"),by=Date]


visit_by_date<-visit_by_plant_date[,.("day_sum"=sum(visits)),keyby=Date]
time_span<-fread("bee_visit_raw_recording - Experiment_setup.csv",header = T)
time_span[,time_span:=as.numeric(tstrsplit(Time_span, split=":")[[2]])]
time_span_by_date<-time_span[,.("time_span_date"=sum(time_span,na.rm=T)/60),by=Date]
visit_by_plant_date<-visit_by_plant_date[time_span_by_date,on="Date"][visit_by_date,on="Date"]


visit_by_plant<-visit_by_plant_date[,.("total_visits"=sum(visits),"total_flowers"=sum(No_total_flowers),"total_time"=sum(time_span_date)),by=.(Plant_ID,Group)]
visit_by_plant[,pollination_rate:=total_visits/(total_flowers*total_time)]
visit_by_plant[,rank:=frank(pollination_rate,na.last="keep"),by=Group]

#fwrite(visit_by_plant_date,"visit_by_plant_date.csv")
#fwrite(visit_by_plant,"visit_by_plant.csv")
#fwrite(pollen_load,"pollen_load.csv")

data_plot<-visit_by_plant[Group=="A",.SD,keyby=pollination_rate]
data_plot[,Plants := ifelse( Plant_ID == "F1", "F1", "F2" ) ]
ggplot(data_plot, aes(x = reorder(Plant_ID,-pollination_rate), y= pollination_rate,fill=Plants)) +
  geom_bar(stat='identity', color=
             'black', width=0.8) + 
  scale_x_discrete(labels = NULL, breaks = NULL) +
  ylab(label="Bumblebee Visitation Rate\nper flower per hour") +
  xlab(label=  "Plants in experiment group A") +
  scale_fill_manual( values = c( "F1"="black", "F2"="grey")) +
  theme_classic() +
  theme(legend.justification = c(-0.5,1))
dev.off()

png("../Figures/Pollination_rate_B_hist.png",res=300,width=1200, height=600)
ggplot(visit_by_plant[Group=="B"], aes(x=pollination_rate)) +
  geom_histogram(binwidth=0.03,color="black", fill="white") +
  scale_x_continuous(limits = c(0, 0.4))+
  scale_y_continuous(limits = c(0, 15))+
  geom_vline(aes(xintercept=median(pollination_rate)),color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=visit_by_plant[Plant_ID=="F1"&Group=="B",pollination_rate]),color="black", linetype="solid", size=1) +
  theme_classic() +
  ylab(label="") +
  xlab(label="Bumblebee Visitation Rate\n per flower per hour")
dev.off()

wilcox.test(visit_by_plant_date[Date=="5/20/2022",.SD,keyby=Plant_ID][,pollination_rate],visit_by_plant_date[Date=="5/18/2022",.SD,keyby=Plant_ID][,pollination_rate])


