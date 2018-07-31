#calculates the statistics of the cataologue and writes them in a csv file in latex notation
library(ggplot2)

setwd("/home/mhaas/PhD/Routines/catalogue_stats/")

#revision: show table for gruenthal window
decl <- read.csv("Uhrhammer_declustered_catalogue.csv")
harm <- read.csv("catalogue.csv")
#decl <- read.csv("DESERVE_catalogue.csv")

# #make a plot showing the declustering effect
# mag_bin <- 0.5
# mag_min <- 3
# mag_max <- 8
# #nr_bins <- (mag_max-mag_min)/mag_bin + 1
# mag_bins <- seq(mag_min+mag_bin,mag_max,mag_bin)
# previous <- mag_min
# # decl_effect <- data.frame(harm$magnitude,decl$magnitude)
# # colnames(decl_effect) <- c('harmonized','declustered')
# # p <- ggplot(decl_effect, aes(magnitude,harmonized,declustered))
# # p + geom_histogram(aes(magnitude,harmonized),colour='blue') + ylab("Counts for bin")
# 
# counts <-c()
# count the events
# for (bin in mag_bins){
#  count1 <- length(which(harm$magnitude >= previous & harm$magnitude <= bin))
#  count2 <- length(which(decl$magnitude >= previous & decl$magnitude <= bin))
#  previous <- bin
#  counts <- rbind(counts,c(count1,count2))
# }
# df <- data.frame(mag_bins,counts[,1],counts[,2])
# colnames(df) <- c('magnitude','harmonized','declustered')
# p <- ggplot(df,aes(magnitude,harmonized,declustered))
# p + geom_point(aes(magnitude,harmonized)) + geom_point(aes(magnitude,declustered))+ylab("Event counts")
# # decl_effect <- data.frame(mag_bins,counts[,1],counts[,2])
# # colnames(decl_effect) <- c('magnitude','harmonized','declustered')
# # p <- ggplot(decl_effect, aes(magnitude,harmonized,declustered))
# # p + geom_bar(aes(harmonized),colour='blue') + geom_bar(aes(declustered),colour='red') + ylab("Counts for bin")

#calculate stats of declustered one
events <- c()
period <- c()
mag_range <- c()
lat_range <- c()
lon_range <- c()
missing_depth <- c()
catname <- c()
for (cat in levels(decl$Agency)){
  idxs <- which(decl$Agency == cat)
  catname <- rbind(catname,cat)
  events <- rbind(events,length(idxs))
  period <- rbind(period,paste(toString(min(decl$year[idxs])),'-',toString(max(decl$year[idxs]))))
  mag_range <- rbind(mag_range,paste(toString(min(decl$magnitude[idxs])),'-',toString(max(decl$magnitude[idxs]))))
  lat_range <- rbind(lat_range,paste(toString(min(decl$latitude[idxs])),'-',toString(max(decl$latitude[idxs]))))
  lon_range <- rbind(lon_range,paste(toString(min(decl$longitude[idxs])),'-',toString(max(decl$longitude[idxs]))))
  missing_depth <- rbind(missing_depth,length(which(decl$depth[idxs]==999)))
}
#ISC dropped out
hierarchy = c('AA14','MA03','HA09','AM89','AV02','SBEI','AM06','AM93','EMEC','GII','NCEDCB','ISC-GEM','IRIS','ISC','KOERI','NCEDCA','SHARE','GSHAP_TR')
sorting_order <- c()
for(cat in catname){
  sorting_order<-append(sorting_order,which(hierarchy == cat))
}

#stats <- data.frame(sorting_oder,catname, events, period, mag_range, lat_range, lon_range, missing_depth,rep('\\\\',length(events)))
#colnames(stats) <- c('ID','Source','Events', 'Period', 'lat_range', 'lon_range','Mw_range', 'Missing_z','\\\\') 
#removed extent
stats <- data.frame(sorting_order,catname, events, period, mag_range, missing_depth,rep('\\\\',length(events)))
colnames(stats) <- c('ID','Source','Events', 'Period', 'Mw_range', 'Missing_z','\\\\') 

#order with respect to hierarchy
stats <- stats[order(stats['ID']),]

#add Total rowP 
a<-sum(as.numeric(stats$Events))
b<-paste(toString(min(decl$year)),'-',toString(max(decl$year)))
c<-paste(toString(min(decl$magnitude)),'-',toString(max(decl$magnitude)))
#d<-paste(toString(min(decl$latitude)),'-',toString(max(decl$latitude)))
#e<-paste(toString(min(decl$longitude)),'-',toString(max(decl$longitude)))
f<-sum(as.numeric(stats$Missing_z))
#total <-cbind('Total','',a,b,c,d,e,f,'\\\\')
total <-cbind('Total','',a,b,c,f,'\\\\')

write.table(stats,file='stats.latex',sep='&',quote=FALSE,row.names=FALSE)
write.table(total,file='stats.latex',sep='&',quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)




