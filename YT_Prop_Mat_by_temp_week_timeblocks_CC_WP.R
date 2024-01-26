## program to read data files,
# plot proportions pre -spawn-post spawn to evaluate changes in spawning phenology during SBTS
####condense proportions maturity by temp and Day of year
# last modified 10/10/2018

rm(list=ls(all=TRUE))  # clear the workspace of all existing variables
#setwd("C:\\R\\Spawning_Phenology") # set the path for the working directory
#od= 'C:/R/Spawning_Phenology/output/'   #directory for writing output

library(MASS)

YTdata=as.data.frame(read.csv(file="YT_63_23_BTS.csv", header=T, sep=","))
#Allspp_SPdata$mean[which(Allspp_SPdata$mean==0)] <- NA
data=subset(YTdata[YTdata$SVSPP=='105',]) #select species

#maxFL<-max(as.numeric(data[,7])) #need to change to appropriate column in new dataset

###add in stock by specifying strata
GBSTRAT<- c('1130','1140','1150','1160','1170','1180','1190','1200','1210')
SNESTRAT_SP<- c('1010','1020','1050','1060','1090','1100','1690','1730','1740')
SNESTRAT_FA<- c('1010','1020','1050','1060','1090','1100')
CCSTRAT_SP<- c('1250','1260','1270','1390','1400','3560','3570','3590','3600','3610','3620','3640','3650','3660')
CCSTRAT_FA<- c('1250','1260',       '1390','1400','3560','3570','3590','3600','3610','3620','3640','3650','3660')


data$STOCK <-NA

sel <-which(data$STRATUM %in% GBSTRAT)
data$STOCK[sel]<- with (data[sel,], 'GB')

sel <-which(data$STRATUM %in% SNESTRAT_SP)
data$STOCK[sel]<- with (data[sel,], 'SNE')


sel <-which(data$STRATUM %in% CCSTRAT_SP)
data$STOCK[sel]<- with (data[sel,], 'CC')

###  summary(as.factor(data$STOCK))

###add in day of year from date
data$DOY <-strftime(as.Date(data$BEGIN_EST_TOWDATE, "%d-%b-%y"), format = "%j")
data$DOY<- as.numeric(data$DOY)


### calculate proportions pre-spawning, spawning active, post-spawning in each season/year/stock
###drop males and immatures and unknowns
data=subset(data[data$EST_YEAR!='1977',])
data=subset(data[data$SEX=='2',])
data=subset(data[data$MATURITY!='Unknown',])
data=subset(data[data$MATURITY!='Immature',])
data=subset(data[data$EST_YEAR!=1969,])



data$SPAWNCOND <- NA
sel <-which(data$MATURITY=="Developing" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Prespawning')
sel <-which(data$MATURITY=="Ripe"|data$MATURITY=="Ripe and Running" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Spawning')
sel <-which(data$MATURITY=="Spent"|data$MATURITY=="Resting" )
data$SPAWNCOND[sel]<- with (data[sel,], 'Postspawning')

sel <-which(data$MATURITY=="Ripe"|data$MATURITY=="Ripe and Running" )
data$MATURITY[sel]<- with (data[sel,], 'Ripe')

data$EST_YEAR <- as.factor(data$EST_YEAR)
data$STOCK <- as.factor(data$STOCK)
data$PROPSPAWN <- NA
data$TEMPBIN <-round(data$BOTTEMP, digits=0)
data$TEMPBIN <-as.factor(data$TEMPBIN)
data$WOY <- data$DOY/7
data$WOY <-round(data$WOY, digits=0)
data$WOY <-as.factor(data$WOY)
data<-(data[!is.na(data$TEMPBIN),])

##concatenate cruise 6 and stock to ease calculations of proportions
#data$CRUISE6_STOCK <- paste(data$CRUISE6,"_",data$STOCK)

data$YEARBLOCK <- NA
YB70s<- c('1970','1971','1972','1973','1974','1975','1976','1977','1978','1979')
YB80s<- c('1980','1981','1982','1983','1984','1985','1986','1987','1988','1989')
YB90s<- c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999')
YB00s<- c('2000','2001','2002','2003','2004','2005','2006','2007','2008','2009')
YB10s<- c('2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')
YB20s<- c('2020','2021','2022','2023','2024','2025','2026','2027','2028','2029')
sel <-which(as.character(data$EST_YEAR) %in% YB70s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB70s')
sel <-which(as.character(data$EST_YEAR) %in% YB80s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB80s')
sel <-which(data$EST_YEAR %in% YB90s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB90s')
sel <-which(data$EST_YEAR %in% YB00s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB00s')
sel <-which(data$EST_YEAR %in% YB10s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB10s')
sel <-which(data$EST_YEAR %in% YB20s )
data$YEARBLOCK[sel]<- with (data[sel,], 'YB20s')

#data2=subset(data[data$STOCK==c("CC","SNE","GB"),])
data2<- data[!is.na(data$STOCK), ]

#data$PROPSPAWN<- rowsum(data$SPAWNCOND, c(data$EST_YEAR,data$SEASON,dta$STOCK), na.rm=T)
###loop to calculateproportions for each year, season, stock


####cleaning up the data a little#####
###may need to remove some small fish that are listed as mature (<20 cm) seems unlikely. 
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
  group_by (SEASON, STOCK, TEMPBIN, YEARBLOCK, MATURITY) %>%
  summarize(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP, na.rm=T),
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            totalMF=n()
  )


###this part tallies by spawn condition and adds colums for each, so there is one row per yearxseasonxstock
library(reshape2)
data3<-dcast(data3,SEASON+STOCK+TEMPBIN+YEARBLOCK~ MATURITY, value.var="totalMF")


data4<- data2 %>%
  group_by ( SEASON, STOCK, TEMPBIN,YEARBLOCK) %>%
  summarise(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP, na.rm=T)
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            #totalMF=n()
  )

### merging to tack on the bio and env data to the summary of spawning info
data5<- merge(data3, data4, by = c( "SEASON", "STOCK","TEMPBIN", "YEARBLOCK"))
### add in columns for proportions
data5$Developing[is.na(data5$Developing)]<-0
data5$Ripe[is.na(data5$Ripe)]<-0
data5$Spent[is.na(data5$Spent)]<-0
data5$Resting[is.na(data5$Resting)]<-0

data5$PROPD<- 100*data5$Developing/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPR<- 100*data5$Ripe/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPS<- 100*data5$Spent/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$PROPT<- 100*data5$Resting/(data5$Developing+data5$Ripe+data5$Spent+data5$Resting)
data5$MF<-(data5$Developing+data5$Ripe+data5$Spent+data5$Resting )
#data$mean[which(data$mean==0)] <- NA


#####doing similar merge to get WOY summary
####this parts gets the summary date and temp data
WOYdata3<- data2 %>%
  group_by (SEASON, STOCK, WOY, MATURITY, YEARBLOCK) %>%
  summarize(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP, na.rm=T),
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            totalMF=n()
  )


###this part tallies by spawn condition and adds colums for each, so there is one row per yearxseasonxstock
library(reshape2)
WOYdata3<-dcast(WOYdata3,SEASON+STOCK+WOY+YEARBLOCK~ MATURITY, value.var="totalMF")


WOYdata4<- data2 %>%
  group_by ( SEASON, STOCK, WOY,YEARBLOCK) %>%
  summarise(minJDAY= min(DOY),
            meanJDAY= mean(DOY),
            maxJDAY=max(DOY),
            meanLENGTH = mean(LENGTH),
            meanTEMP=mean(BOTTEMP, na.rm=T)
            #Prespawning= n (SPAWNCOND=="Prespawning"),
            #Prespawning= length (SPAWNCOND=="Prespawning"),
            #totalMF=n()
  )

### merging to tack on the bio and env data to the summary of spawning info
WOYdata5<- merge(WOYdata3, WOYdata4, by = c( "SEASON", "STOCK","WOY","YEARBLOCK"))


### add in columns for proportions
WOYdata5$Developing[is.na(WOYdata5$Developing)]<-0
WOYdata5$Ripe[is.na(WOYdata5$Ripe)]<-0
WOYdata5$Spent[is.na(WOYdata5$Spent)]<-0
WOYdata5$Resting[is.na(WOYdata5$Resting)]<-0

WOYdata5$PROPD<- 100*WOYdata5$Developing/(WOYdata5$Developing+WOYdata5$Ripe+WOYdata5$Spent+WOYdata5$Resting)
WOYdata5$PROPR<- 100*WOYdata5$Ripe/(WOYdata5$Developing+WOYdata5$Ripe+WOYdata5$Spent+WOYdata5$Resting)
WOYdata5$PROPS<- 100*WOYdata5$Spent/(WOYdata5$Developing+WOYdata5$Ripe+WOYdata5$Spent+WOYdata5$Resting)
WOYdata5$PROPT<- 100*WOYdata5$Resting/(WOYdata5$Developing+WOYdata5$Ripe+WOYdata5$Spent+WOYdata5$Resting)
WOYdata5$MF<-(WOYdata5$Developing+WOYdata5$Ripe+WOYdata5$Spent+WOYdata5$Resting )
#data$mean[which(data$mean==0)] <- NA
##break data into spring and fall separately for plotting and analysis

#setting up the graphics window
graphics.off()
windows(height=16, width=14, record=T, rescale="fit")
#png(file=paste('C:/R/Regime shift/output/fall/yellowtail_spring.png', sep=""), width=14, height=16, units='in',res=800)  ###png
par(mfrow=c(6,1), mar=c(2,8,2,6), oma=c(6,2,3,3))

b.names=c("","","","","","","","","","","","")
mat.col=c("orange", "cyan", "purple", "coral3")
temp.bins = data.frame(TEMPBIN= c(0,1,2,3,4,5,6,7,8,9,10))
#Barplots of spawning condition vs temp bin for each time period

#SNE
SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB70s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]

barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=2.5,las=1,space=0, new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
#legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Yellowtail Flounder - mature females - CC GOM Spring',3, line=0, outer=T, cex=2.2)
mtext('1970s',4, line=2, outer=F, cex=2.2)
#mtext('Temperature (C)',1, line=4, outer=F, cex=2.0)
SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB80s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]

barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=2.5,las=1,space=0, new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
#legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
#mtext('Yellowtail Flounder - mature females - GB Spring',3, line=0, outer=T, cex=2.2)
mtext('1980s',4, line=2, outer=F, cex=2.2)
#mtext('Temperature (C)',1, line=4, outer=F, cex=2.0)

SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB90s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, offset=1,space=0,   new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
box()
mtext('1990s',4, line=2, outer=F, cex=2.2)
#mtext('Temperature (C)',1, line=4, outer=F, cex=2.0)

SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB00s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
box()
mtext('2000s',4, line=2, outer=F, cex=2.2)
#mtext('Temperature (C)',1, line=4, outer=F, cex=2.0)


SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB10s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
#legend(2,-10,legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Spawning condition (percent)',2, line=-1, outer=T, cex=2)
mtext('2010s',4, line=2, outer=F, cex=2.2)

SNEdata=subset(data5[data5$STOCK=='CC'&data5$SEASON=='SPRING'&data5$YEARBLOCK=='YB20s',]) #select stock and season
SNEdata<-merge(temp.bins, SNEdata, by="TEMPBIN", all=TRUE)
SNEdata<- SNEdata[order(as.numeric(SNEdata$TEMPBIN)),]
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
legend(-0.5,90,legend=c("Dev", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = F, inset = c(0,-0.2), cex=2.2, bty = "n") 
#legend(2,-8,legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Spawning condition (percent)',2, line=-1, outer=T, cex=2)
mtext('2020s',4, line=2, outer=F, cex=2.2)
mtext('Temperature (C)',1, line=6, outer=F, cex=2.0)






####this part for week of year by timeblocks


SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING',]) #select stock and season
SNEdata<- SNEdata[order(SNEdata$WOY),]
tSNEdata <- t(SNEdata)

bb.names= tSNEdata[3,]

#setting up the graphics window
graphics.off()
windows(height=16, width=14, record=T, rescale="fit")
#png(file=paste('C:/R/Regime shift/output/fall/yellowtail_spring.png', sep=""), width=14, height=16, units='in',res=800)  ###png
par(mfrow=c(6,1), mar=c(2,8,2,6), oma=c(6,2,3,3))

b.names=c("","","","","","","","","","","","")
mat.col=c("orange", "cyan", "purple", "coral3")
woy.bins = data.frame(WOY= c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
#Barplots of spawning condition vs temp bin for each time period

#SNE
SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB70s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]

barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=2.5,las=1,space=0, new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
#legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Yellowtail Flounder - mature females - CC GOM Spring',3, line=0, outer=T, cex=2.2)
mtext('1970s',4, line=2, outer=F, cex=2.2)
#mtext('Week of Year',1, line=4, outer=F, cex=2.0)

SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB80s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]

barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=2.5,las=1,space=0, new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
#legend('bottom',legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()

mtext('1980s',4, line=2, outer=F, cex=2.2)
#mtext('Week of Year',1, line=4, outer=F, cex=2.0)

SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB90s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, offset=1,space=0,   new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
box()
mtext('1990s',4, line=2, outer=F, cex=2.2)
#mtext('Week of Year',1, line=4, outer=F, cex=2.0)

SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB00s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
box()
mtext('2000s',4, line=2, outer=F, cex=2.2)
#mtext('Week of Year',1, line=4, outer=F, cex=2.0)


SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB10s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
legend(2,-10,legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Spawning condition (percent)',2, line=-1, outer=T, cex=2)
mtext('2010s',4, line=2, outer=F, cex=2.2)

SNEdata=subset(WOYdata5[WOYdata5$STOCK=='CC'&WOYdata5$SEASON=='SPRING'&WOYdata5$YEARBLOCK=='YB20s',]) #select stock and season
SNEdata<-merge(woy.bins, SNEdata, by="WOY", all=TRUE)
SNEdata[is.na(SNEdata)] = 0
SNEdata$MF[which(SNEdata$MF==0)] <- " "
tSNEdata <- t(SNEdata)
b.names= tSNEdata[1,]
barplot (as.matrix(tSNEdata[14:17,]), beside=F, col=mat.col, names=b.names, cex.axis=2.5, cex.names=2.5,cex.lab=1.8,las=1, space=0,  new=T)
text(seq(0.80,nrow(SNEdata),by=1),100, label=paste(tSNEdata[18,]), pos=2,srt=90, font= 2, cex=2.5)
legend(-0.5,90,legend=c("Dev", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = F, inset = c(0,-0.2), cex=2.2, bty = "n") 
#legend(2,-10,legend=c("Developing", "Ripe", "Spent", "Resting"), fill=mat.col, xpd = TRUE, horiz = TRUE, inset = c(0,-0.2), cex=2.2, bty = "n") 
box()
mtext('Week of Year',1, line=4, outer=F, cex=2.0)
mtext('Spawning condition (percent)',2, line=-1, outer=T, cex=2)
mtext('2020s',4, line=2, outer=F, cex=2.2)




