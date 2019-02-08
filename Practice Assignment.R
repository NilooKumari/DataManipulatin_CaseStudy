# Data Manipulation with R

setwd("C:\\Git\\DataManipulatin_CaseStudy")
fd<- read.csv("FlightDelays.csv",stringsAsFactors = FALSE)

#The data has information on the flights over the year 2004 and if a particular
#flight was delayed or not.

names(fd)
dim(fd)
str(fd)

library(dplyr)
library(lubridate)

head(fd$date)
tail(fd$date)
fd$date<-mdy(fd$date)
unique(fd$delay)
unique(weekdays(fd$date))

#1. Find out the number of delayed flights for all weekdays
dat1<- fd%>%filter(fd$delay=="delayed", weekdays(fd$date)!="Saturday" & weekdays(fd$date)!="Sunday")%>%nrow()
dat1

#2. Find the average distance, total distance and count for all delayed flights on
#Friday.
dat2<- fd%>%filter(weekdays(fd$date)=="Friday",fd$delay=="delayed")%>%
  summarise(mean(distance),sum(distance),length(distance))
dat2

#3. Find out how many flights were on time on Week days and Weekends
#(Consider Saturday and Sunday as weekends)

dat3<- fd%>%filter(fd$delay=="ontime",weekdays(fd$date)!="Saturday" & weekdays(fd$date)!="Sunday")%>%nrow()
dat3

dat4<- fd%>%filter(fd$delay=="ontime",weekdays(fd$date)=="Saturday"| weekdays(fd$date)=="Sunday")%>%nrow()
dat4
#4. Find out the number of flights for each destination across all weekdays
names(fd)
unique(fd$dest)
dat5<- fd%>%filter(fd$dest=="JFK",weekdays(fd$date)!="Saturday" & weekdays(fd$date)!="Sunday")%>%nrow()
dat5

dat6<- fd%>%filter(fd$dest=="LGA",weekdays(fd$date)!="Saturday" & weekdays(fd$date)!="Sunday")%>%nrow()
dat6

dat7<- fd%>%filter(fd$dest=="EWR",weekdays(fd$date)!="Saturday" & weekdays(fd$date)!="Sunday")%>%nrow()
dat7

#5. Find out the number of times weather was bad across all weekdays. 
#(1 indicates bad weather)

dat8<-fd%>%filter(weekdays(date)!="Sunday"&weekdays(date)!="Saturday"&weather==1)%>%nrow()
dat8