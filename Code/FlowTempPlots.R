#Creating figures of changes in Flow & Temp in urban rivers from around US
#using data from USGS gauges.
#Data available at: https://maps.waterdata.usgs.gov/mapper/index.html

#Dependencies----
#library(googleVis)
#library(devtools)
# Hit an empty line in the Console to skip updates
library(dataRetrieval)
library(lubridate)
library(ggplot2)
library(chron)
library(zoo)

#Flow figures%%%---------
#Have to be careful and check time periods downloaded to ensure there 
#are actually solid data available for entire date range
flow <- "00060"

#List of rivers and gauges---------------------------------------
#listing individually & then combining allows record of gauges included

#Cedar River Renton, WA-->PNW
#discharge Dec 1986-Jan 2021
#temp Oct 2007-Jan2021
cedar <- "12119000"

#Red Butte Creek near Salt Lake City, UT-->SW
#discharge Oct 1986-Jan 2021
#temp Feb 2012-Jan 2021
redbt <- "10172200"

#Rouge River Detroit, MI-->Midwest
#discharge Oct 1989-Jan 2021
#temp Oct 2007-Jan 2021
rouge <- "04166500"

#Chattahoochee River Atlanta, GA-->South
#discharge Oct 1989-Jan 2021
#temp Oct 2007-Jan 2021
chatt <- "02336000"

#Accotink Creek Annandale, VA-->Mid-Atlantic
#discharge Oct 1990-Jan 2021
#temp Feb 2015-Jan 2021
acco <- "01654000"

#Was not able to find gauges in urban areas for
#Rockey Mtns or New Englan
rollmean(pre_flow_site$meanflow, 10, na.pad=TRUE)
  
#Additional Stations, for fun-------------------------------
#Trinity River Dallas, TX-->Because Texas
#discharge Jan 1988-Jan 2021
#temp Oct 2014-Jan 2021
trin <- "08057000"

#American River Fair Oaks, CA-->Because Cali
#discharge Oct 1987-Jan 2021
#temp Oct 2007-Jan 2021
amer <- "08057000"

#USGS KENAI R AT COOPER LANDING AK
#discharge 1987-10-02 	2021-01-15
#temp 2007-10-01 	2021-01-15
kenai <- "15258000"


#VILLAGE CREEK AT AVENUE W AT ENSLEY, AL
#discharge 1994-10-01 	2021-01-13
#temp 2007-10-01 	2021-01-13
#Upstream looks like rapid urbanization over past few decades.
#vilcrk <- "02458450"
#This site is too recent for the dates below.


#CO Gages ---------
# SOUTH PLATTE RIVER AT ENGLEWOOD, CO.
#temp 2007-10-01 	2021-01-15
#flow 1986-10-24 	2021-01-15
splatt <- "06711565"

#ARKANSAS RIVER AT MOFFAT STREET AT PUEBLO, CO
#temp 2007-10-01 	2021-01-15
#flow 1988-10-01 	2021-01-15
arkpeub <- "07099970"

#FOUNTAIN CREEK AT PUEBLO, CO.
#temp 2007-10-01 	2021-01-15
#flow 1988-10-01 	2021-01-15
fount <- "7106500" ###Throws errors in flow (IDK why) - didn't try temp.

#ARKANSAS RIVER NEAR AVONDALE, CO.
#temp 2007-10-01 	2021-01-15
#flow 1986-11-17 	2021-01-15
arkavon <- "07109500"

#ARKANSAS RIVER AT LAS ANIMAS, CO.
#temp 2007-10-01 	2021-01-15
#flow 1987-10-04 	2021-01-15
arklasa <- "07124000"



#East Coast:-----
#BRANDYWINE CREEK AT WILMINGTON, DE
#temp 2007-10-01 	2021-01-15
#flow 1989-06-01 	2021-01-15
brandy <- "01481500" #Flow doesn't have enough data for the earlier date


#Create list of gages for loop-----
gage.list <- list(cedar, redbt, rouge, chatt, acco, trin, amer)
#Make sure this is identical to the list prior...probably a more clever way to do this.
gage.names <- list("cedar", "redbt", "rouge", "chatt", "acco", "trin", "amer")

#Running additionals
#CO list
gage.list <- list(splatt,arkpeub,arkavon,arklasa)
#Make sure this is identical to the list prior...probably a more clever way to do this.
gage.names <- list("splatt","arkpeub","arkavon","arklasa")

#DE
gage.list <- list(reedy)
#Make sure this is identical to the list prior...probably a more clever way to do this.
gage.names <- list("reedy")



#Set working directory to deposit plots before starting loop:
#setwd("C:/Users/wilsonmatt/OneDrive - Susquehanna University/Manuscripts/LTF/plots")

#Save plots to "Figures" folder in repository
#pdf(paste("figures/", "Flow", ".pdf", sep=""))

###Start flow loop ------
for(i in seq_along(gage.list)) 
{  site <- gage.list[i]

#Rest of script - sub "site" for the gauge code in the functions#
#1993-95
flow_pre1 <- readNWISuv(site,flow,"1993-01-01","1993-12-31")
flow_pre1$year <- format(flow_pre1$dateTime, "%Y")
flow_pre1 <- subset(flow_pre1, year != "1994")
flow_pre1$jdate <- yday(as.Date(flow_pre1$dateTime))
pre1_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre1, mean)

flow_pre2 <- readNWISuv(site,flow,"1994-01-01","1994-12-31")
flow_pre2$year <- format(flow_pre2$dateTime, "%Y")
flow_pre2 <- subset(flow_pre2, year != "1995")
flow_pre2$jdate <- yday(as.Date(flow_pre2$dateTime))
pre2_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre2, mean)

flow_pre3 <- readNWISuv(site,flow,"1995-01-01","1995-12-31")
flow_pre3$year <- format(flow_pre3$dateTime, "%Y")
flow_pre3 <- subset(flow_pre3, year != "1996")
flow_pre3$jdate <- yday(as.Date(flow_pre3$dateTime))
pre3_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre3, mean)

df_list <- list(pre1_day, pre2_day, pre3_day)
pre_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

pre_flow_site <- cbind(pre_all, meanflow = rowMeans(pre_all[2:4]))

#flow, post site (2017-19)
flow_post1 <- readNWISuv(site,flow,"2017-01-01","2017-12-31")
flow_post1$year <- format(flow_post1$dateTime, "%Y")
flow_post1 <- subset(flow_post1, year != "2018")
flow_post1$jdate <- yday(as.Date(flow_post1$dateTime))
post1_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post1, mean)

flow_post2 <- readNWISuv(site,flow,"2018-01-01","2018-12-31")
flow_post2$year <- format(flow_post2$dateTime, "%Y")
flow_post2 <- subset(flow_post2, year != "2019")
flow_post2$jdate <- yday(as.Date(flow_post2$dateTime))
post2_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post2, mean)

flow_post3 <- readNWISuv(site,flow,"2019-01-01","2019-12-31")
flow_post3$year <- format(flow_post3$dateTime, "%Y")
flow_post3 <- subset(flow_post3, year != "2020")
flow_post3$jdate <- yday(as.Date(flow_post3$dateTime))
post3_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post3, mean)

df_list <- list(post1_day, post2_day, post3_day)
post_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

post_flow_site <- cbind(post_all, meanflow = rowMeans(post_all[2:4]))

#head(pre_flow_site)
#pre_date <- as.data.frame(month.day.year(pre_flow_site$jdate))
#pre_flow_site$monthday <- as.Date(with(pre_date, paste(month, day,sep="-")), "%m-%d")
#pre_flow_site$monthday <- as.Date(pre_flow_site$monthday)

#then just output all of the plots
jpeg(filename=paste("Figures/", "flow.avg", gage.names[i], ".jpg"), 
     width = 600, height = 600, units = "px", pointsize = 12,
     quality = 300)


print(ggplot(pre_flow_site, aes(jdate, meanflow))+ 
  #geom_line(data=pre_flow_site, 
  #          aes(jdate, meanflow, color="1993-95",linetype="dashed")) +
  #geom_line(data=post_flow_site, 
  #          aes(jdate, meanflow, color="2017-19",linetype="solid"))+
  geom_line(data=pre_flow_site, 
            aes(y=rollmean(meanflow, 20, na.pad=TRUE), color="1993-95"),
            size=1.5, na.rm = TRUE) +
  geom_line(data=post_flow_site, 
            aes(y=rollmean(meanflow, 20, na.pad=TRUE), color="2017-19"),
            size=1.5,na.rm = TRUE) +
    theme_bw() +
    scale_color_manual(values=c("#999999", "#000000"))+
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
    ylab(expression(bold(Discharge~(ft^3/s))))+
    xlab("Julian Date")+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=13))+
  theme(legend.position = c(0.05, 0.95),legend.justification = c(0, 1)))

dev.off()
} 



###Start temp loop -------------

temp <- "00010"
#Loop is set to only plot sites with temp data starting by 1 Jan 2008.
#Cedar River Renton, WA-->PNW
#discharge Dec 1986-Jan 2021
#temp Oct 2007-Jan2021
cedar <- "12119000"

#Rouge River Detroit, MI-->Midwest
#discharge Oct 1989-Jan 2021
#temp Oct 2007-Jan 2021
rouge <- "04166500"

#Chattahoochee River Atlanta, GA-->South
#discharge Oct 1989-Jan 2021
#temp Oct 2007-Jan 2021
chatt <- "02336000"

#American River Fair Oaks, CA-->Because Cali
#discharge Oct 1987-Jan 2021
#temp Oct 2007-Jan 2021
amer <- "08057000"

#USGS KENAI R AT COOPER LANDING AK
#discharge 1987-10-02 	2021-01-15
#temp 2007-10-01 	2021-01-15
kenai <- "15258000"


#Rouge, chatt, amer had too many gaps to plot effectively.

#Create list of gages for loop
gage.list <- list(cedar, amer, kenai)
#Make sure this is identical to the list prior...probably a more clever way to do this.
gage.names <- list("cedar", "amer", "kenai")

for(i in seq_along(gage.list)) 
{  site <- gage.list[i]

#Rest of script - sub "site" for the gauge code in the functions#
#2008-10
temp_pre1 <- readNWISuv(site,temp,"2008-01-01","2008-12-31")
temp_pre1$year <- format(temp_pre1$dateTime, "%Y")
temp_pre1 <- subset(temp_pre1, year != "2009")
temp_pre1$jdate <- yday(as.Date(temp_pre1$dateTime))
pre1_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre1, mean)

temp_pre2 <- readNWISuv(site,temp,"2009-01-01","2009-12-31")
temp_pre2$year <- format(temp_pre2$dateTime, "%Y")
temp_pre2 <- subset(temp_pre2, year != "2010")
temp_pre2$jdate <- yday(as.Date(temp_pre2$dateTime))
pre2_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre2, mean)

temp_pre3 <- readNWISuv(site,temp,"2010-01-01","2010-12-31")
temp_pre3$jdate <- yday(as.Date(temp_pre3$dateTime))
temp_pre3$year <- format(temp_pre3$dateTime, "%Y")
temp_pre3 <- subset(temp_pre3, year != "2011")
pre3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre3, mean)

df_list <- list(pre1_day, pre2_day, pre3_day)
pre_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

pre_temp_site <- cbind(pre_all, meantemp = rowMeans(pre_all[2:4]))

#temp, post site: 2017-19
temp_post1 <- readNWISuv(site,temp,"2017-01-01","2017-12-31")
temp_post1$year <- format(temp_post1$dateTime, "%Y")
temp_post1 <- subset(temp_post1, year != "2018")
temp_post1$jdate <- yday(as.Date(temp_post1$dateTime))
post1_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post1, mean)

temp_post2 <- readNWISuv(site,temp,"2018-01-01","2018-12-31")
temp_post2$year <- format(temp_post2$dateTime, "%Y")
temp_post2 <- subset(temp_post2, year != "2019")
temp_post2$jdate <- yday(as.Date(temp_post2$dateTime))
post2_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post2, mean)

temp_post3 <- readNWISuv(site,temp,"2019-01-01","2019-12-31")
temp_post3$year <- format(temp_post3$dateTime, "%Y")
temp_post3 <- subset(temp_post3, year != "2020")
temp_post3$jdate <- yday(as.Date(temp_post3$dateTime))
post3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post3, mean)

df_list <- list(post1_day, post2_day, post3_day)
post_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

post_temp_site <- cbind(post_all, meantemp = rowMeans(post_all[2:4]))

#head(pre_temp_site)
#pre_date <- as.data.frame(month.day.year(pre_temp_site$jdate))
#pre_temp_site$monthday <- as.Date(with(pre_date, paste(month, day,sep="-")), "%m-%d")
#pre_temp_site$monthday <- as.Date(pre_temp_site$monthday)

#then just output all of the plots
jpeg(filename=paste("Figures/", "temp.avg", gage.names[i], ".jpg"), 
     width = 600, height = 600, units = "px", pointsize = 12,
     quality = 300)


print(ggplot(pre_temp_site, aes(jdate, meantemp))+ 
        #geom_line(data=pre_temp_site, aes(jdate, meantemp, color="2008-10")) +
        #geom_line(data=post_temp_site, aes(jdate, meantemp, color="2017-19"))+
        geom_line(data=pre_temp_site, aes(y=rollmean(meantemp, 20, na.pad=TRUE), 
                                          color="2008-10"), size=1.5, na.rm = TRUE) +
        geom_line(data=post_temp_site, aes(y=rollmean(meantemp, 20, na.pad=TRUE), 
                                           color="2017-19"),size=1.5,na.rm = TRUE) +
        theme_bw() +
        scale_color_manual(values=c("#999999", "#000000"))+
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Julian Date")+
        theme(legend.title=element_blank(),
              legend.text=element_text(size=13))+
        theme(legend.position = c(0.05, 0.95),legend.justification = c(0, 1)))

dev.off()
} 

