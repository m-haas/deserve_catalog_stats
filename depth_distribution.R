#depth distribution
library(ggplot2)
library(FNN)

setwd("/home/mhaas/PhD/Routines/catalogue_stats/")

cat <- read.csv('DESERVE_catalogue.csv')

#remove unknown depth
idxs<-which(cat$depth == 999 | cat$year < 1900)
cat <- cat[-idxs,]

#gridded depth distribution
#grid distance
ds <- 4
#grid
x<-y<- c()
i <- 0
s <- c()
n <- 4#round(max(cat$depth)-min(cat$depth))/5
#m <- round(((max(cat$longitude)-min(cat$longitude))/ds)*((max(cat$latitude)-min(cat$latitude))/ds))
m <- length(seq(min(cat$longitude),max(cat$longitude),ds))*length(seq(min(cat$latitude),max(cat$latitude),ds))
z <- matrix(0,m,n)
for(xs in seq(min(cat$longitude),max(cat$longitude),ds)){
  for (ys in seq(24.55,max(cat$latitude),ds)){
# old version
#     #determine events
#     idxs1 <- which(cat$longitude >=xs & cat$longitude < xs+ds)
#     idxs2 <- which(cat$latitude >= ys & cat$latitude < ys+ds)
#     idxs <- intersect(idxs1,idxs2)
#     #determine depth distribution with classes c(0,5,15,30,50,100,max)#in 5km steps
#     if(length(idxs)>0){
#     #n <- round(max(cat$depth[idxs])-min(cat$depth[idxs]))/5
#     #n <- max(n,1)
#     d <- hist(cat$depth[idxs],breaks=c(0,5,15,30,50,max(cat$depth)))}else{
#       d <- data.frame(counts<-0)
#     }
    #get 100 nearest neighbours
    nbs <- get.knnx(data=cbind(cat$longitude,cat$latitude),cbind(xs+ds/2,ys+ds/2),k=100,algorithm='kd_tree')
    d <- hist(cat$depth[nbs$nn.index],breaks=c(0,5,15,30,50,max(cat$depth)))
    #store xcentre,ycentre and depth distribution
    x <- append(x,xs)
    y <- append(y,ys)
    i <- i+1
    z[i,1:4] <- d$counts[1:4]/sum(d$counts[1:4])
    s <- append(s,sum(d$counts))
  }
}
#round to whole percent numbers
z <- round(z,2)

#make sure all depths have at least 1 percent
z[which(z==0)]<-0.01

#fix rows now off by 0.1 (add to last bin)
for (i in seq(dim(z)[1])){
  dx <- round(1-sum(z[i,]),2)
  if(dx!=1 & abs(dx) >= 0.01){
    for (j in seq(4,1)){
      if(z[i,j]+dx>=0){
        z[i,j]<-z[i,j]+dx
        break
      }
    }
  }
}


#centre point of depth distribution
x <- x + ds/2
y <- y + ds/2

# #create mesh with seismic source zone resolution
# ds <- 0.1
# ymin <- 24.55
# ymax <- 37.8
# xmin <- 29.95
# xmax <- 40.8
# i <- 0
# 
# xgrd<-ygrd<-c()
# n <-length(seq(xmin,xmax,0.1))*length(seq(ymin,ymax,0.1))
# zgrd <- matrix(0,n,6)
# for (xs in seq(xmin,xmax,0.1)){
#   for (ys in seq(ymin,ymax,0.1)){
#     #determine closest point of depth distribution grid
#     idxs1 <- which(abs(x-xs)==min(abs(x-xs)))
#     idxs2 <- which(abs(y-ys)==min(abs(y-ys)))
#     idx <- intersect(idxs1,idxs2)[1]
#     #store
#     xgrd <- append(xgrd,xs)
#     ygrd <- append(ygrd,ys)
#     i <- i +1
#     zgrd[i,] <- z[idx,]
#   }
# }

#get mesh from seismicity
grid_file <- '/home/mhaas/PhD/oq-deserve/seismicity/frankel/rate_b090_0_50km_5km.xyz'
grd <- read.csv(grid_file,header=FALSE)
i <- 0

xgrd<-ygrd<-c()
zgrd <- matrix(0,length(grd[,1]),4)
for (i in seq(length(grd[,1]))){
    xs <- grd[i,1]
    ys <- grd[i,2]
    #determine closest point of depth distribution grid
    idxs1 <- which(abs(x-xs)==min(abs(x-xs)))
    idxs2 <- which(abs(y-ys)==min(abs(y-ys)))
    idx <- intersect(idxs1,idxs2)[1]
    #store
    xgrd <- append(xgrd,xs)
    ygrd <- append(ygrd,ys)
    zgrd[i,] <- z[idx,]
}


df <- data.frame(xgrd,ygrd,zgrd)
colnames(df) <- c('lon','lat','2.5','10','22.5','40')
#df <- df[with(df, order(-ygrd, xgrd)), ]
write.table(df,'depth_grid.csv',row.names=FALSE)
