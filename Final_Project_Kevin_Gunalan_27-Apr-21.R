# Importing libraries
library(tidyverse)
library(ggmap)
library(plotly)
library(lubridate)

#Setting up Working Directory
setwd('C:/Users/User/Desktop/MS in DAE/Spring 2021/AIT 580/Project_Assignment_4/')

#Reading the dataset
arrest <- read.csv('Cleaned.csv', stringsAsFactors = TRUE)

#Converting character into time object
arrest$Arrest.Date <- gsub(x=arrest$Arrest.Date,pattern=" 0:00",replacement="",fixed=T)
arrest$Time <- sprintf("%04d", arrest$Time)
arrest$Time <- format(strptime(arrest$Time, format="%H%M"), format = "%H:%M")

#Filtering low contributing descent
descent <- arrest[arrest$Descent.Code != 'L' & arrest$Descent.Code != 'S' & arrest$Descent.Code != 'U' & arrest$Descent.Code != 'Z',]

#Filtering out top 5 most arrested based on charges
cgd <- arrest[arrest$Charge.Group.Description == 'Miscellaneous Other Violations'| arrest$Charge.Group.Description == 'Aggravated Assault' | arrest$Charge.Group.Description == 'Driving Under Influence' | arrest$Charge.Group.Description == 'Narcotic Drug Laws' | arrest$Charge.Group.Description == 'Other Assaults' & arrest$Year == "2020",]

#Filtering based on "Miscellaneous Other Violations"
cgdm <- arrest[arrest$Charge.Group.Description == 'Miscellaneous Other Violations' & arrest$Year == "2020",]

#Filtering based on "Driving Under Influence"
cgddui <- arrest[arrest$Charge.Group.Description == 'Driving Under Influence' & arrest$Year == "2020",]
count_dui <- aggregate(Charge.Description ~ Month, cgddui, length )

#Elimininating Day 31st of the month 
cgd_day <- cgd[cgd$Day != 31,]
count_cgd <- aggregate(Charge.Group.Description ~ Day, cgd_day, length)

#Summary statistics based on Age
summary(arrest$Age)
boxplot(Age~Descent.Code, data = descent, xlab = "Descent Code", ylab = "Age (Years)", main = 'Boxplot for Descent Code Versus Age',
        col=c("gold","darkgreen","#eb6134","#ebab34","#a8eb34","#eb3462","#34a8eb","#34ebd0","#3459eb","#eb3434", "#ebd934","#49eb34","#3434eb"))

#Creating a .csv file for python processing
write.csv(arrest, file = "Time_processed.csv")

#Registering to google API to use their services
register_google(key = "AIzaSyBuV2QgiIi6XMjJXWjlnm0qFuajrOukyzo")

#Plotting point on Los Angeles map
map <- get_map( location = 'Los Angeles', zoom = 10, maptype = "roadmap" )
arrest_loc <- ggmap(map) + geom_point(data = cgd, aes(x = LON, y = LAT, 
              color = Charge.Group.Description),alpha = .15, size = 2, show.legend = FALSE) + guides(fill=guide_legend(title="Charge Group Description")) + xlab('Longitutde')+
              ylab('Latitude') + ggtitle('Miscellaneous Other Violations based on Los Angeles Map')

#Viewing map plots for top 5 most arrested charge
arrest_loc + facet_wrap( ~ Charge.Group.Description, ncol = 3)+ ggtitle('Charge Group Description Category based on Los Angeles Map')

#Viewing map plots for all 12 months of the year 2020
arrest_Mis <- ggmap(map) + geom_point(data = cgdm, aes(x = LON, y = LAT, 
              color = Charge.Group.Description),alpha = .15, size = 2, show.legend = FALSE) + xlab('Longitutde')+
  ylab('Latitude') + ggtitle('Month-wise Analysis of Miscellaneous Other Violations Category based on Los Angeles Map')

arrest_Mis + ggtitle('Miscellaneous Other Violations Category based on Los Angeles Map')

arrest_Mis + facet_wrap( ~Month, ncol = 4, labeller = label_both) + ggtitle('Month-wise Analysis of Miscellaneous Other Violations Category based on Los Angeles Map')

#Line plot for month-wise analysis of arrest frequency
Line_1 <- ggplot(count_dui, aes(x = Month, y = Charge.Description))+  ggtitle("Month-Wise Analysis Based on Charge Description in 2020") + 
  xlab("Month") + 
  ylab("Arrest Frequency") +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:12,
                   labels=c("Jan", "Feb", "Mar", "Apr",
                            "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
#Interactive Line Plot
ggplotly(Line_1)   

#Line plot for hour-wise analysis of arrest frequency
Line_2 <- ggplot(count_cgd, aes(x = Day, y = Charge.Group.Description))+  ggtitle("Hourly Trend Analysis Based on Charge Group Description") + 
  xlab("Time of the Day") + 
  ylab("Arrest Frequency") +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:31)

#Interactive Line Plot
ggplotly(Line_2)

