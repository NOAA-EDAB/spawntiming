## program to read data files,
# plot proportions pre -spawn-post spawn to evaluate changes in spawning phenology during SBTS
## program to read data files,
# plot proportions pre -spawn-post spawn to evaluate changes in spawning phenology during SBTS
####condense proportions maturity by temp and Day of year
# last modified 2/16/2021 for haddock assessment working paper

rm(list=ls(all=TRUE))  # clear the workspace of all existing variables
setwd("C:\\Users\\mark.wuenschel\\Documents\\Rfiles\\R\\Spawning_Phenology\\haddock") # set the path for the working directory
od= 'C:/Users/mark.wuenschel/Documents/Rfiles/R/Spawning_Phenology/haddock/'   #directory for writing output

library(MASS)


HADdata=as.data.frame(read.csv(file="haddock_1970_2019_export.csv", header=T, sep=","))
#Allspp_SPdata$mean[which(Allspp_SPdata$mean==0)] <- NA
data=subset(HADdata[HADdata$SVSPP=='74',]) #select species

#maxFL<-max(as.numeric(data[,7])) #need to change to appropriate column in new dataset

###add in stock by specifying strata
GBSTRAT<- c('1130','1140','1150','1160','1170','1180','1190','1200','1210','1220','1230','1240','1250','1290','1300')
GOMSTRAT<-c('1260','1270','1280','1360','1370','1380','1390','1400')


data$STOCK <-NA

sel <-which(data$STRATUM %in% GBSTRAT)
data$STOCK[sel]<- with (data[sel,], 'GB')


sel <-which(data$STRATUM %in% GOMSTRAT)
data$STOCK[sel]<- with (data[sel,], 'GOM')

###  summary(as.factor(data$STOCK))

###add in day of year from date
data$DOY <-strftime(as.Date(data$BEGIN_EST_TOWDATE, "%d-%b-%y"), format = "%j")
data$DOY<- as.numeric(data$DOY)


### calculate proportions pre-spawning, spawning active, post-spawning in each season/year/stock
###drop males and immatures and unknowns
data=subset(data[data$BOTTEMP!=0,])
data=subset(data[data$SEX=='2',])
data=subset(data[data$MATURITY!='Unknown',])
data=subset(data[data$MATURITY!='Immature',])
data=subset(data[data$EST_YEAR!=1969,])
# data$SPAWNCOND <- NA
# sel <-which(data$MATURITY=="Developing" )
# data$SPAWNCOND[sel]<- with (data[sel,], 'Prespawning')
# sel <-which(data$MATURITY=="Ripe"|data$MATURITY=="Ripe and Running" )
# data$SPAWNCOND[sel]<- with (data[sel,], 'Spawning')
# sel <-which(data$MATURITY=="Spent"|data$MATURITY=="Resting" )
# data$SPAWNCOND[sel]<- with (data[sel,], 'Postspawning')
data$SPAWNCOND <- NA
sel <-which(data$MATURITY=="Developing" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Developing')
sel <-which(data$MATURITY=="Ripe"|data$MATURITY=="Ripe and Running" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Ripe')
sel <-which(data$MATURITY=="Spent"|data$MATURITY=="Spent" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Spent')
sel <-which(data$MATURITY=="Resting" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Resting')


#data2=subset(data[data$STOCK==c("CC","SNE","GB"),])
data2<- data[!is.na(data$STOCK), ]

##concatenate cruise 6 and stock to ease calculations of proportions
data$CRUISE6_STOCK <- paste(data$CRUISE6,"_",data$STOCK)

data$PROPSPAWN <- NA
#data$PROPSPAWN<- rowsum(data$SPAWNCOND, c(data$EST_YEAR,data$SEASON,dta$STOCK), na.rm=T)
###loop to calculateproportions for each year, season, stock


####cleaning up the data a little#####
###may need to remove some small fish that are listed as mature (<20 cm) seems unlikely. 
data$EST_YEAR <- as.factor(data$EST_YEAR)
data$STOCK <- as.factor(data$STOCK)
#library(plyr); 
library(dplyr)
# results.by.year.season.stock <- ddply(.data=data, .var=C(data$EST_YEAR,data$SEASON, data$STOCK), .fun=function(x) {
#   data.frame(n=nrow(x),
#              num_prespawn = nrow(subset(x, data$SPAWNCOND %in%
#                                           "Prespawning")) 
#              
#              #prop_prespawn= nrow(subset(x, data$SPAWNCOND %in%
#              #                             "Prespawning")) / nrow(x)
#             )
# })


#results.by.year.season.stock <- ddply(data, C(data$EST_YEAR,data$SEASON, data$STOCK), mutate, nprespawn=(sum(count(data$SPAWNCOND=="Prespawning"))))


###aggregate by year and season

####this parts gets the summary date and temp data
data3<- data2 %>%
  group_by (EST_YEAR, SEASON, STOCK, SPAWNCOND) %>%
  summarize(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP),
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            totalMF=n()
            )

###this part tallies by spawn condition and adds colums for each, so there is one row per yearxseasonxstock
library(reshape2)
data3<-dcast(data3,EST_YEAR+SEASON+STOCK~ SPAWNCOND, value.var="totalMF")
  

data4<- data2 %>%
  group_by (EST_YEAR, SEASON, STOCK) %>%
  summarise(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP)
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            #totalMF=n()
  )

### merging to tack on the bio and env data to the summary of spawning info
data5<- merge(data3, data4, by = c("EST_YEAR", "SEASON", "STOCK"))

### add in columns for proportions
# data5$Postspawning[is.na(data5$Postspawning)]<-0
# data5$Prespawning[is.na(data5$Prespawning)]<-0
# data5$Spawning[is.na(data5$Spawning)]<-0
# 
# data5$PROPPRE<- 100*data5$Prespawning/(data5$Prespawning+data5$Spawning+data5$Postspawning)
# data5$PROPSP<- 100*data5$Spawning/(data5$Prespawning+data5$Spawning+data5$Postspawning)
# data5$PROPPOS<- 100*data5$Postspawning/(data5$Prespawning+data5$Spawning+data5$Postspawning)
# data5$MF<-(data5$Prespawning+data5$Spawning+data5$Postspawning)
# #data$mean[which(data$mean==0)] <- NA
# ##break data into spring and fall separately for plotting and analysis

data5$DURATION <-data5$maxJDAY-data5$minJDAY
### add in columns for proportions
data5$Developing[is.na(data5$Developing)]<-0
data5$Ripe[is.na(data5$Ripe)]<-0
data5$Spent[is.na(data5$Spent)]<-0
data5$Resting[is.na(data5$Resting)]<-0


data5$PROPD<- 100*data5$Developing/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPRI<- 100*data5$Ripe/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPSP<- 100*data5$Spent/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPRE<- 100*data5$Resting/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$MF<-(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
#data$mean[which(data$mean==0)] <- NA
##break data into spring and fall separately for plotting and analysis

data5$DURATION <-data5$maxJDAY-data5$minJDAY

#setting up the graphics window
graphics.off()
windows(height=16, width=14, record=T, rescale="fit")
#png(file=paste('C:/R/Regime shift/output/fall/yellowtail_spring.png', sep=""), width=14, height=16, units='in',res=800)  ###png
par(mfrow=c(3,1), mar=c(6,8,2,6), oma=c(2,2,3,3))

#names and maos colors for the barplot
#month.names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
b.names=c("","","","","","","","","","","","")
#red=PG, orange=EC, light blue=LC, pink=V1, purple=V2, green=GM, yellow=B1, dark blue=B2
#mat.col=c("orange", "cyan", "coral3")
mat.col=c("orange", "cyan","purple", "coral3")
#Barplots for each stock and env date plots in pairs

###GOM
GOMdata=subset(data5[data5$STOCK=='GOM'&data5$SEASON=='SPRING',]) #select stock and season
tGOMdata <- t(GOMdata)
b.names= tGOMdata[1,]
maxFL<-max(tGOMdata[11,])
#tSSdata$mean[which(tSSdata$mean==0)] <- NA
barplot (as.matrix(tGOMdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2, cex.names=1.6,cex.lab=1.8,las=1, space=0 )
text(seq(0.80,nrow(GOMdata),by=1),100, label=paste(tGOMdata[18,]), pos=2,srt=90, font= 2, cex=2)
legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()

mtext('Spawning condition (percent)',2, line=4, outer=F, cex=2)
mtext('Mean length (cm)',4, line=4, outer=F, cex=2)
par(new=T)
plot(seq(1,47,by=1),tGOMdata[11,],ylim=c(0,1.1*(as.numeric(maxFL))), col="black", type='l',lwd=2.5,xaxt='n', axes=F, xlab='',ylab='')
axis(4, pretty(c(0, 1.1*(as.numeric(maxFL)))), col='black',cex.axis=2)
mtext('Haddock - mature females - GOM Spring',3, line=0, outer=T, cex=2.2)
#mtext('SS',3, line=1, outer=F, adj=0, cex=2.2)


#####day of year and temp plot, xlim=c(1970, 2018)
GOMTEMPdata= subset(data[data$STOCK=='GOM'&data$SEASON=='SPRING',]) #select stock and season 
boxplot (GOMTEMPdata$BOTTEMP ~ GOMTEMPdata$EST_YEAR,xlim=c(1, 48), axes=T, yaxt='n', ylab='',xlab='', boxwex=1, cex.axis=1.6)
axis(4, pretty(c(2, 12)), col='darkblue',cex.axis=2)
mtext(expression(paste("Temperature (",degree,"C)")),4, line=4, outer=F, cex=2)

day.col=c("white", "gray", "White")
df.bar<-barplot (as.matrix(tGOMdata[c(8,13),]), beside=F, col=day.col, border=NA, ylim = c(60,160),names=b.names, cex.axis=2.2, cex.names=1.6,cex.lab=1.8,las=1, space=0, bty="o")
points(x=df.bar, y=GOMdata$meanJDAY, pch=18, col="black", cex=3)
box()
mtext('Day of Year Sampled',2, line=4, outer=F, cex=2)
#plot(seq(1,47,by=1), tSNEdata[11,],  col="darkblue", type='l', lwd=1.5,xaxt='n', axes=F, xlab='',ylab='', add=T)

#GB
GBdata=subset(data5[data5$STOCK=='GB'&data5$SEASON=='SPRING',]) #select stock and season
tGBdata <- t(GBdata)
b.names= tGBdata[1,]
maxFL<-max(tGBdata[11,])
#tSSdata$mean[which(tSSdata$mean==0)] <- NA
barplot (as.matrix(tGBdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2, cex.names=1.6,cex.lab=1.8,las=1, space=0 )
text(seq(0.80,nrow(GBdata),by=1),100, label=paste(tGBdata[18,]), pos=2,srt=90, font= 2, cex=2)
legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()

mtext('Spawning condition (percent)',2, line=4, outer=F, cex=2)
mtext('Mean length (cm)',4, line=4, outer=F, cex=2)
par(new=T)
plot(seq(1,49,by=1),tGBdata[11,],ylim=c(0,1.1*(as.numeric(maxFL))), col="black", type='l',lwd=2.5,xaxt='n', axes=F, xlab='',ylab='')
axis(4, pretty(c(0, 1.1*(as.numeric(maxFL)))), col='black',cex.axis=2)
mtext('Haddock - mature females - GB Spring',3, line=0, outer=T, cex=2.2)
#mtext('SS',3, line=1, outer=F, adj=0, cex=2.2)


#####day of year and temp plot, xlim=c(1970, 2018)
GBTEMPdata= subset(data[data$STOCK=='GB'&data$SEASON=='SPRING',]) #select stock and season 
boxplot (GBTEMPdata$BOTTEMP ~ GBTEMPdata$EST_YEAR,xlim=c(1, 48), axes=T, yaxt='n', ylab='',xlab='', boxwex=1, cex.axis=1.6)
axis(4, pretty(c(2, 12)), col='darkblue',cex.axis=2)
mtext(expression(paste("Temperature (",degree,"C)")),4, line=4, outer=F, cex=2)

day.col=c("white", "gray", "White")
df.bar<-barplot (as.matrix(tGBdata[c(8,13),]), beside=F, col=day.col, border=NA, ylim = c(60,160),names=b.names, cex.axis=2.2, cex.names=1.6,cex.lab=1.8,las=1, space=0, bty="o")
points(x=df.bar, y=GBdata$meanJDAY, pch=18, col="black", cex=3)
box()
mtext('Day of Year Sampled',2, line=4, outer=F, cex=2)
#plot(seq(1,47,by=1), tSNEdata[11,],  col="darkblue", type='l', lwd=1.5,xaxt='n', axes=F, xlab='',ylab='', add=T)


###this part added 2/18/2021
###################FALL####################
###GOM
GOMdata=subset(data5[data5$STOCK=='GOM'&data5$SEASON=='FALL',]) #select stock and season
tGOMdata <- t(GOMdata)
b.names= tGOMdata[1,]
maxFL<-max(tGOMdata[11,])
#tSSdata$mean[which(tSSdata$mean==0)] <- NA
barplot (as.matrix(tGOMdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2, cex.names=1.6,cex.lab=1.8,las=1, space=0 )
text(seq(0.80,nrow(GOMdata),by=1),100, label=paste(tGOMdata[18,]), pos=2,srt=90, font= 2, cex=2)
legend('bottom',legend=c("Developing", "Ripe", "Spent","Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()

mtext('Spawning condition (percent)',2, line=4, outer=F, cex=2)
mtext('Mean length (cm)',4, line=4, outer=F, cex=2)
par(new=T)
plot(seq(1,45,by=1),tGOMdata[11,],ylim=c(0,1.1*(as.numeric(maxFL))), col="black", type='l',lwd=2.5,xaxt='n', axes=F, xlab='',ylab='')
axis(4, pretty(c(0, 1.1*(as.numeric(maxFL)))), col='black',cex.axis=2)
mtext('Haddock - mature females - GOM FALL',3, line=0, outer=T, cex=2.2)
#mtext('SS',3, line=1, outer=F, adj=0, cex=2.2)


#####day of year and temp plot, xlim=c(1970, 2018)
GOMTEMPdata= subset(data[data$STOCK=='GOM'&data$SEASON=='FALL',]) #select stock and season 
boxplot (GOMTEMPdata$BOTTEMP ~ GOMTEMPdata$EST_YEAR, axes=T, yaxt='n', ylab='',xlab='', boxwex=1, cex.axis=1.6)
axis(4, pretty(c(2, 14)), col='darkblue',cex.axis=2)
mtext(expression(paste("Temperature (",degree,"C)")),4, line=4, outer=F, cex=2)

day.col=c("white", "gray", "White")
df.bar<-barplot (as.matrix(tGOMdata[c(8,13),]), beside=F, col=day.col, border=NA, ylim = c(260,360),names=b.names, cex.axis=2.2, cex.names=1.6,cex.lab=1.8,las=1, space=0, bty="o")
points(x=df.bar, y=GOMdata$meanJDAY, pch=18, col="black", cex=3)
box()
mtext('Day of Year Sampled',2, line=4, outer=F, cex=2)
#plot(seq(1,47,by=1), tSNEdata[11,],  col="darkblue", type='l', lwd=1.5,xaxt='n', axes=F, xlab='',ylab='', add=T)

###GB
GBdata=subset(data5[data5$STOCK=='GB'&data5$SEASON=='FALL',]) #select stock and season
tGBdata <- t(GBdata)
b.names= tGBdata[1,]
maxFL<-max(tGBdata[11,])
#tSSdata$mean[which(tSSdata$mean==0)] <- NA
barplot (as.matrix(tGBdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2, cex.names=1.6,cex.lab=1.8,las=1, space=0 )
text(seq(0.80,nrow(GBdata),by=1),100, label=paste(tGBdata[18,]), pos=2,srt=90, font= 2, cex=2)
legend('bottom',legend=c("Developing", "Ripe", "Spent","Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()

mtext('Spawning condition (percent)',2, line=4, outer=F, cex=2)
mtext('Mean length (cm)',4, line=4, outer=F, cex=2)
par(new=T)
plot(seq(1,47,by=1),tGBdata[11,],ylim=c(0,1.1*(as.numeric(maxFL))), col="black", type='l',lwd=2.5,xaxt='n', axes=F, xlab='',ylab='')
axis(4, pretty(c(0, 1.1*(as.numeric(maxFL)))), col='black',cex.axis=2)
mtext('Haddock - mature females - GB FALL',3, line=0, outer=T, cex=2.2)
#mtext('SS',3, line=1, outer=F, adj=0, cex=2.2)


#####day of year and temp plot, xlim=c(1970, 2018)
GBTEMPdata= subset(data[data$STOCK=='GB'&data$SEASON=='FALL',]) #select stock and season 

boxplot (GBTEMPdata$BOTTEMP ~ GBTEMPdata$EST_YEAR, axes=T, yaxt='n', ylab='',xlab='', boxwex=1, cex.axis=1.6)
axis(4, pretty(c(6, 16)), col='darkblue',cex.axis=2)
mtext(expression(paste("Temperature (",degree,"C)")),4, line=4, outer=F, cex=2)

day.col=c("white", "gray", "White")
df.bar<-barplot (as.matrix(tGBdata[c(8,13),]), beside=F, col=day.col, border=NA, ylim = c(240,340),names=b.names, cex.axis=2.2, cex.names=1.6,cex.lab=1.8,las=1, space=0, bty="o")
points(x=df.bar, y=GBdata$meanJDAY, pch=18, col="black", cex=3)
box()
mtext('Day of Year Sampled',2, line=4, outer=F, cex=2)
#plot(seq(1,47,by=1), tSNEdata[11,],  col="darkblue", type='l', lwd=1.5,xaxt='n', axes=F, xlab='',ylab='', add=T)



# 
# par(new=T)
# plot(seq(1,47,by=1), tSNEdata[11,], ylim = c(2,10), col="darkblue", type='l', lwd=1.5,xaxt='n', axes=F, xlab='',ylab='')
# axis(4, pretty(c(2, 10)), col='darkblue',cex.axis=2)
# mtext('Mean Temperature (C)',4, line=4, outer=F, cex=2.2)
# 

