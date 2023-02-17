
## preparations
setwd("/Volumes/GoogleDrive/My Drive/Research/Peng_Lab_Shared/Projects/M.carpar/2022/")
library(data.table)
library(ggplot2)
library(reshape)
## data importing

# find all the files and file names
filelist = list.files(path = "./Phenotyping", pattern="McMpF2_phenotyping", full.names = T)
files = sapply(filelist, function(x) strsplit(strsplit(strsplit(x,split = "/")[[1]][3],split=" - ")[[1]][2], split="\\.")[[1]][1])

# read all the files
listed_trait_raw <- lapply(filelist, fread, sep=",", colClasses = c('Plant_ID'='character'))
# remove rows indicating a problem
listed_trait<-lapply(listed_trait_raw, function(x) x[is.na(`Problematic_measure(1 if it should be left out)`),])
# rename all the data.table with their group names
names(listed_trait)<-files

## Find all the plant IDs for plants used to seed the first generation 
plant_info<-fread("./Plant_A_B_arrangement_final.csv",drop=3)

## deal with shape measures first
# combine all the shape measurement
shape<-do.call(rbind,Map(cbind, 
                         listed_trait[c("A-shape1","A-shape2","B-shape1","B-shape2")], 
                         group = c("A-shape1","A-shape2","B-shape1","B-shape2")))
# convert measurement data type to numeric
shape[,(4:11):=lapply(.SD, as.numeric),.SDcols=c(4:11)]
shape_melt<-melt(shape)
boxplot(data=shape_melt, value~variable)


# take means
shape_mean<-shape[is.na(`Problematic_measure(1 if it should be left out)`),c(lapply(.SD, mean,na.rm=T),No_flower_shape=.N),by=Plant_ID,.SDcols=c(4:11)]
#shape_mean[,ID_integer:=as.integer(Plant_ID)]
#setdiff(Plant_ID_keep,shape_mean[N>1, ID_integer])


## then deal with nectar measures
nectar<-do.call(rbind,c(Map(cbind, 
                         listed_trait[c("A-nectar","B-nectar")], 
                         group = c("A-nectar","B-nectar")), list(fill=T)))
nectar[,(5:8):=lapply(.SD, as.numeric),.SDcols=c(5:8)]
nectar[,nectar_concentration:=ifelse((`Adjusted_concentration (%)`>0)&!is.na(`Adjusted_concentration (%)`),`Adjusted_concentration (%)`,`Sugar_concentration (%)`)]

nectar_melt<-melt(nectar)
boxplot(data=nectar_melt, value~variable)


nectar_mean<-nectar[is.na(`Problematic_measure(1 if it should be left out)`),c(lapply(.SD, mean,na.rm=T),No_flower=.N),by=Plant_ID,.SDcols=c(6,12)]

# go through stigma data
stigma<-listed_trait[["A-stigma"]][,c(1,4:6)]
stigma[,.N]==uniqueN(stigma,by="Plant_ID")
stigma[duplicated(stigma,by="Plant_ID"),Plant_ID]
stigma<-unique(stigma,by="Plant_ID")

# go through spot data
spot<-listed_trait[["A-spot"]][,c(1,4:8)]
spot[,.N]==uniqueN(spot,by="Plant_ID")
spot[duplicated(spot,by="Plant_ID"),Plant_ID]
spot<-unique(spot,by="Plant_ID")

# go through image data
image<-listed_trait[["A-image"]][,c(1,4:7)]
image[,.N]==uniqueN(image,by="Plant_ID")
image[duplicated(image,by="Plant_ID"),Plant_ID]
image<-image[Plant_ID!="",]

# go through leaf data
leaf<-listed_trait[["B-leaf"]][,c(1,4)]
leaf[,.N]==uniqueN(leaf,by="Plant_ID")
leaf[duplicated(leaf,by="Plant_ID"),Plant_ID]
leaf<-unique(leaf,by="Plant_ID")


## Join all the data.tables
traits_to_join<-c("nectar_mean","shape_mean","leaf","image","spot","stigma")
all_traits<-Reduce(function(x, y) merge(x,y, by = "Plant_ID", all=T, sort=T), mget(traits_to_join))
#fwrite(all_traits,"./Phenotyping/F2_phenotyping_all_final.csv")

plant_info[,Plant_ID:=as.character(Plant_ID)]
addlist<-list(c("F1","MP","MC"),c(NA,NA,NA))
plant_info<-rbindlist(list(plant_info,addlist))
phenotyping_for_bee_pollination<-all_traits[plant_info,on="Plant_ID"]
#fwrite(phenotyping_for_bee_pollination,"./Phenotyping/F2_phenotyping_bee_pollination_final.csv")







all_traits_pollination<-visit_by_plant[all_traits_pollination,on="Plant_ID"]
#fwrite(all_traits_pollination,"phenotyping_pollination_data_4_27.csv")




summary(glm(total_visits~total_flowers+total_time+nectar_volume+nectar_concentration+reflex+project_area+R_norm+B_norm+spot_size+CTW+CTL+CTW_mid,data=all_traits_pollination,family=poisson()))



