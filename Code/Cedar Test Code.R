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
library(tidyverse)
library(openxlsx)
library(readxl)

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


#Rest of script - sub "site" for the gauge code in the functions#
#1993-1995 data ------------------
flow_pre1 <- readNWISuv(cedar,flow,"1993-01-01","1993-12-31")
flow_pre1$year <- format(flow_pre1$dateTime, "%Y")
flow_pre1 <- subset(flow_pre1, year != "1994")
flow_pre1$jdate <- yday(as.Date(flow_pre1$dateTime))
pre1_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre1, mean)

flow_pre2 <- readNWISuv(cedar,flow,"1994-01-01","1994-12-31")
flow_pre2$year <- format(flow_pre2$dateTime, "%Y")
flow_pre2 <- subset(flow_pre2, year != "1995")
flow_pre2$jdate <- yday(as.Date(flow_pre2$dateTime))
pre2_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre2, mean)

flow_pre3 <- readNWISuv(cedar,flow,"1995-01-01","1995-12-31")
flow_pre3$year <- format(flow_pre3$dateTime, "%Y")
flow_pre3 <- subset(flow_pre3, year != "1996")
flow_pre3$jdate <- yday(as.Date(flow_pre3$dateTime))
pre3_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre3, mean)

df_list <- list(pre1_day, pre2_day, pre3_day)
pre_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

pre_flow_cedar <- cbind(pre_all, meanflow = rowMeans(pre_all[2:4]))

#2017-2019 data-----------
flow_post1 <- readNWISuv(cedar,flow,"2017-01-01","2017-12-31")
flow_post1$year <- format(flow_post1$dateTime, "%Y")
flow_post1 <- subset(flow_post1, year != "2018")
flow_post1$jdate <- yday(as.Date(flow_post1$dateTime))
post1_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post1, mean)

flow_post2 <- readNWISuv(cedar,flow,"2018-01-01","2018-12-31")
flow_post2$year <- format(flow_post2$dateTime, "%Y")
flow_post2 <- subset(flow_post2, year != "2019")
flow_post2$jdate <- yday(as.Date(flow_post2$dateTime))
post2_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post2, mean)

flow_post3 <- readNWISuv(cedar,flow,"2019-01-01","2019-12-31")
flow_post3$year <- format(flow_post3$dateTime, "%Y")
flow_post3 <- subset(flow_post3, year != "2020")
flow_post3$jdate <- yday(as.Date(flow_post3$dateTime))
post3_day <- aggregate(X_00060_00000 ~ jdate, data = flow_post3, mean)


df_list <- list(post1_day, post2_day, post3_day)
post_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

post_flow_cedar <- cbind(post_all, meanflow = rowMeans(post_all[2:4]))

#head(pre_flow_cedar)
#pre_flow_cedar <- pre_flow_cedar
#pre_date <- as.data.frame(month.day.year(pre_flow_cedar$jdate))
#pre_flow_cedar$monthday <- as.Date(with(pre_date, paste(month, day,sep="-")), "%m-%d")
#pre_flow_cedar$monthday <- as.Date(pre_flow_cedar$monthday)

#Formatting for figures---------------------
#create rolling mean dataset
pre_rollmean <- pre_flow_cedar %>% 
        mutate(roll = rollmean(meanflow, k = 20, na.pad = TRUE))
post_rollmean <- post_flow_cedar %>% 
        mutate(roll = rollmean(meanflow, k = 20, na.pad = TRUE))

#formatting years for plots
pre_rollmean$year <- "1993-95"
post_rollmean$year <- "2017-19"
rollmean <- rbind(pre_rollmean,post_rollmean)

#tidy environment
rm(flow_post1, flow_post2, flow_post3, flow_pre1, flow_pre2, flow_pre3,
   post1_day,post2_day,post3_day,pre1_day,pre2_day,pre3_day,
   df_list,post_all,pre_all,pre_date,
   post_flow_cedar,pre_flow_cedar)

#plot-----------------
cedar<-ggplot(NULL, aes(x="jdate", y="roll", group="year"))+ 
        geom_line(data=na.omit(rollmean),linetype="year")+
        #geom_line(meanflow, color="grey",linetype="dashed")) +
        #geom_line(data=post_flow_cedar, 
        #          aes(jdate, meanflow, color="2017-19",linetype="solid"))+
        theme_bw() +
        #scale_color_manual(values=c("#999999", "#999999"))+
        scale_linetype_manual(values = c("dashed","solid"))+
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
        theme(legend.position = c(0.05, 0.95),legend.justification = c(0, 1))

cedar

###Start temp loop -------------

temp <- "00010"
#Loop is set to only plot sites with temp data starting by 1 Jan 2008.
#Cedar River Renton, WA-->PNW
#discharge Dec 1986-Jan 2021
#temp Oct 2007-Jan2021
cedar <- "12119000"


#Rest of script - sub "site" for the gauge code in the functions#
#2008-10
temp_pre1 <- readNWISuv(cedar,temp,"2008-01-01","2008-12-31")
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

temp_post3 <- readNWISuv(cedar,temp,"2019-01-01","2019-12-31")
temp_post3$year <- format(temp_post3$dateTime, "%Y")
temp_post3 <- subset(temp_post3, year != "2020")
temp_post3$jdate <- yday(as.Date(temp_post3$dateTime))
post3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post3, mean)

df_list <- list(post1_day, post2_day, post3_day)
post_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

post_temp_site <- cbind(post_all, meantemp = rowMeans(post_all[2:4]))

temp <- ggplot(pre_temp_site, aes(jdate, meantemp))+ 
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
        theme(legend.position = c(0.05, 0.95),legend.justification = c(0, 1))

temp

#Create flow and temp excel workbook for export-------------
#Download discharge data for 2008 to use w/2008 temp
flow_pre1 <- readNWISuv(cedar,flow,"2008-01-01","2008-12-31")
flow_pre1$year <- format(flow_pre1$dateTime, "%Y")
flow_pre1 <- subset(flow_pre1, year != "2009")
flow_pre1$jdate <- yday(as.Date(flow_pre1$dateTime))
flow_pre1_day <- aggregate(X_00060_00000 ~ jdate, data = flow_pre1, mean)

wb <- createWorkbook()

addWorksheet(wb, "Cedar River Discharge 2019")
writeDataTable(wb, sheet = "Cedar River Discharge 2019",
               x = flow_post3_day,
               withFilter = T, tableStyle = "none")

addWorksheet(wb, "Cedar River Temperature 2019")
writeDataTable(wb, sheet = "Cedar River Temperature 2019",
               x = post3_day,
               withFilter = T, tableStyle = "none")

addWorksheet(wb, "Cedar River Discharge 2008")
writeDataTable(wb, sheet = "Cedar River Discharge 2008",
               x = flow_pre1_day,
               withFilter = T, tableStyle = "none")

addWorksheet(wb, "Cedar River Temperature 2008")
writeDataTable(wb, sheet = "Cedar River Temperature 2008",
               x = pre1_day,
               withFilter = T, tableStyle = "none")

saveWorkbook(wb, paste0("Data/Cedar River Flow Temp_", 
                        format(Sys.time(), "%Y%m%d"),".xlsx"), 
             overwrite = T)
