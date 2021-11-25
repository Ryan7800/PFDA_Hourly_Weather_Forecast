#RYAN WONG YUAN KHONG
#TP051888

install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("ggridges")
library(readxl) #package which include functions to ready excel files
library(ggplot2) #package which includes ggplot functions
library(dplyr)  #package which includes filter functions
library(ggpubr) #package which includes ggarrange function
library(ggridges)#load package ggridges, allow me to fill region under lines
Weather = read.csv("C:/Users/xryan/Desktop/HourlyWeatherData.csv")
View(Weather) #To view the excel file
summary(Weather)
#------------------------------------------------------------#

#-------------------------Analysis 1-------------------------#
AprilWeather = Weather %>% #taking data stored in Weather
  filter(month=="4")%>% #only get values for the month April
  group_by(origin,month,day) %>% #out of all data in Weather only show..
  summarise(#reduces multiple values to a single summary.
    AvgVisib = round(mean(visib,na.rm = TRUE),2)#get avg for visibility,if there is a  
  )                      #null value, remove it
View(AprilWeather)#view data in AVGVIS, AVGVIS is a sub data frame derived from Weather
VisibilityPass = 3 #new variable to hold the value 3,
#visibility has to be 3 miles to fly; less is prohibited
ggplot(AprilWeather,aes(x=day,y=AvgVisib,color=origin))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=seq(1,31,3))+#Extra Feature 1 
  scale_y_continuous(breaks=seq(0,10,1))+
  ggtitle("Average Daily Visibility April(JFK &LGA)")+#Extra Feature 2
  xlab("Day")+
  ylab("Average Visibility")+
  geom_hline(yintercept = VisibilityPass, linetype = "dotted", color="red")
#------------------------------------------------------------#


#--------------------------Analysis 2------------------------#
#Determine Visibility Issue on April 19 2013 and factors
Visibility4.19JFK = Weather %>% #taking data stored in Weather
  filter(origin=="JFK")%>% #only get values for JFK
  filter(month=="4")%>% #only get values for the month April
  filter(day=="19")%>% #only get values for the 19th of April
  select(hour,temp,dewp,precip,wind_speed,humid,visib)
View(Visibility4.19JFK)

TempMinusDewp = c((Visibility4.19JFK$temp)-(Visibility4.19JFK$dewp))
VisibFactors4.19JFK = cbind(Visibility4.19JFK,TempMinusDewp)
View(VisibFactors4.19JFK)

ggplot(VisibFactors4.19JFK,aes(x=hour, y=visib, col=temp))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=seq(0,23,1))+
  ggtitle("Hour vs Visibility 19th April JFK")+
  xlab("Hours")+
  ylab("Visibility")+
  geom_vline(xintercept=10, linetype="longdash", color="yellow")+
  geom_vline(xintercept=12, linetype="longdash", color="orange")+ 
  geom_vline(xintercept =14, linetype ="longdash", color="red")+ 
  geom_vline(xintercept =21, linetype ="longdash", color="purple")+
  geom_vline(xintercept =23, linetype ="longdash", color="blue")
#------------------------------------------------------------#


#--------------------------Analysis 3-----------------------------# 
#Temperature based on 12 months showing min and max temperatures

maxt = max(Weather$temp)#gets maximum temperature from data frame Weather
avgt = mean(Weather$temp)#gets average temperature from data frame Weather
mint = min(Weather$temp)#gets minimum temperature from data frame Weather
View(Weather)
View(maxt)
View(avgt)
View(mint)

ggplot(Weather, aes(day,temp , color = factor(month)))+ 
  geom_point() + 
  facet_wrap(~month) +
  geom_hline(yintercept = maxt, linetype = "longdash", color="red") + 
  geom_hline(yintercept = mint, linetype = "longdash", color="blue") +
  geom_hline(yintercept = avgt, linetype = "longdash", color="purple")+
  scale_x_continuous(breaks=seq(1,31,3))+
  scale_y_continuous(breaks=seq(0,100,10))+
  ggtitle("Temperature of all 12 Months in 2013")+
  xlab("Day")+
  ylab("Temperature")

#-----------------------------Analysis 4------------------------------#
#https://www.nyc.com/visitor_guide/weather_facts.75835/
Weather2.0=Weather
SeasonType = case_when(Weather2.0$month <=2~ "Winter",
                       Weather2.0$month <= 5 ~ "Spring", 
                       Weather2.0$month <= 8 ~ "Summer",
                       Weather2.0$month <= 11 ~ "Autumn",
                       Weather2.0$month >= 12 ~ "Winter")         

Weather2.0 = mutate(Weather2.0, SeasonType)
View(Weather2.0)
AverageMonthlyTemp = Weather2.0%>%
                     group_by(month,SeasonType)%>%
                     summarise(
                        avgtemps = mean(temp, narm=TRUE)
                     )%>%
                     select(month,avgtemps,SeasonType)
View(AverageMonthlyTemp)

ggplot(AverageMonthlyTemp, aes(x=month, y=avgtemps, color=month))+
  geom_line()+
  scale_x_continuous(breaks=seq(1,12,1))+
  ggtitle("Average Monthly Temperature 2013")+
  xlab("Month")+
  ylab("Average Temperature")+
  geom_vline(xintercept = 12, linetype = "longdash",color = "blue")+
  geom_vline(xintercept = 2.95, linetype = "longdash",color = "blue")+
  geom_vline(xintercept = 3, linetype = "longdash",color = "turquoise")+
  geom_vline(xintercept = 5.95, linetype = "longdash",color = "turquoise")+
  geom_vline(xintercept = 6, linetype = "longdash",color = "red")+
  geom_vline(xintercept = 8.95, linetype = "longdash",color = "red")+
  geom_vline(xintercept = 9, linetype = "longdash",color = "black")+
  geom_vline(xintercept = 11.95, linetype = "longdash",color = "black")



#------------------------------------Analysis 5-----------------------------------#
#Study Temperature for January
JanWeather1 = Weather %>% #taking data stored in Weather
              filter(month=="1")%>% #only January
              group_by(day)%>%
              summarise(
                mintemp = min(temp, na.rm=TRUE),
                avgtemp = mean(temp, na.rm=TRUE),
                maxtemp = max(temp,na.rm=TRUE)
                       )
View (JanWeather1)

ggplot(JanWeather1, aes(x=day,y=mintemp, color="mintemp"))+
  geom_line()+
  geom_line(aes(x=day,y=maxtemp, color="maxtemp"))+
  geom_line(aes(x=day,y=avgtemp, color="avgtemp"))+
  scale_x_continuous(breaks=seq(1,31,1))+
  scale_y_continuous(breaks=seq(0,100,2))+
  ggtitle("January Temperature Levels")+
  xlab("Day")+
  ylab("Temperature")+
  geom_hline(yintercept=min(JanWeather1$mintemp), linetype="longdash", color="blue")+
  geom_hline(yintercept=32, linetype="longdash", color="red")

#--------------------------------------------------------------------#

#--------------------------------Analysis 6--------------------------#

#Study Temperature for July
JulyWeather1 = Weather %>% #taking data stored in Weather
  filter(month=="7")%>% #only January
  group_by(day)%>%
  summarise(
    mintemp = min(temp, na.rm=TRUE),
    avgtemp = mean(temp, na.rm=TRUE),
    maxtemp = max(temp,na.rm=TRUE)
  )
View (JulyWeather1)

ggplot(JulyWeather1, aes(x=day,y=mintemp, color="mintemp"))+
  geom_line()+
  geom_line(aes(x=day,y=maxtemp, color="maxtemp"))+
  geom_line(aes(x=day,y=avgtemp, color="avgtemp"))+
  scale_x_continuous(breaks=seq(1,31,1))+
  scale_y_continuous(breaks=seq(0,100,2))+
  ggtitle("July Temperature Levels")+
  xlab("Day")+
  ylab("Temperature")+
  geom_hline(yintercept=max(JulyWeather1$maxtemp), linetype="longdash", color="red")+
  geom_hline(yintercept=95, linetype="longdash", color="orange")
#horizontal lines to indicate starting of danger levels

#--------------------------------Analysis 7---------------------------#
#Average Humidity based on 12 months
#Humidity based on 12 months showing min and max humidity

AverageMonthlyHum = Weather%>%
                    group_by(month)%>%
                    summarise(
                       highhumid = max(humid, na.rm=TRUE), 
                       lowhumid = min(humid, na.rm=TRUE),
                       avghumid = mean(humid, na.rm=TRUE)
                              )

HumidType = case_when(AverageMonthlyHum$avghumid <= 44 ~ "Low",
                         AverageMonthlyHum$avghumid <= 65 ~ "Ideal", 
                         AverageMonthlyHum$avghumid > 65 ~ "High")

AverageMonthlyHum = mutate(AverageMonthlyHum, HumidType)
View(AverageMonthlyHum)

ggplot(AverageMonthlyHum, aes(x = month,y =highhumid , color = "highhumid"))+ 
  geom_line() + 
  geom_line(aes(x = month,y =avghumid , color = "avghumid"))+
  geom_line(aes(x = month,y =lowhumid , color = "lowhumid"))+
  geom_hline(yintercept = 44, linetype = "longdash", color="red") + 
  geom_hline(yintercept = 65, linetype = "longdash", color="blue") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_y_continuous(breaks=seq(0,100,5)) +
  ggtitle("Average Humidity of all 12 Months in 2013") +
  xlab("Month") +
  ylab("Humidity")

#------------------------------------Analysis 8-------------------------------#
#Cross Wind in JFK Airport (Planes using Runway 31L)
RcrosswindJFK31L = Weather %>%
  filter(wind_gust > 40)%>%
  filter(origin == "JFK")%>% 
  filter(wind_dir >=0 & wind_dir <= 90)%>%
  select(month,day,hour,wind_gust,wind_dir)

LcrosswindJFK31L = Weather %>%
  filter(wind_gust > 40)%>%
  filter(origin == "JFK") %>%
  filter(wind_dir >= 180 & wind_dir <=270)%>%
  select(month,day,hour,wind_gust,wind_dir)

View(LcrosswindJFK31L)
View(RcrosswindJFK31L)

#https://thepointsguy.com/news/how-windy-does-it-have-to-be-before-planes-cant-take-off/#:~:text=Aircraft%20do%20have%20an%20additional,should%20not%20exceed%2045%20knots
#https://www.weathershack.com/static/ed-rain-measurement.html#:~:text=Rainfall%20amount%20is%20described%20as,that%20is%20one%20inch%20deep
#For Planes like Boeing 737 max wind_gust = 35 knots/40mph on a dry days
#-------------------------------------------------------------------------------------#


#---------------------------------Analysis 9----------------------------------#
#Studying Wind Speeds in all month of 2013 
MaximumDWS= Weather %>%
               group_by(origin,month,day) %>%
               summarise(
                   WindSpeed = max(wind_speed ,na.rm = TRUE)
                        )
View(MaximumDWS)

Moderatewindspeed= Weather %>%
                   group_by(origin,month,day) %>%
                   filter(wind_speed >=30 & wind_speed<=39)%>%
                   select(origin,month,day,wind_speed)

View(Moderatewindspeed)

Dangerzone= 40 #"A High Threat to Life and Property from High Wind."
Moderatezone=39 #"A Moderate Threat to Life and Property from High Wind."
                #wind advisory would be issued by National Weather Service 
Lowzone=25 #"A Low Threat to Life and Property from High Wind." 

ggplot(MaximumDWS, aes(x=day,y=WindSpeed, col=origin))+ 
  geom_point() +
  geom_line() +
  facet_wrap(~month)+ 
  geom_hline(yintercept = Dangerzone, linetype = "longdash",color = "red")+
  geom_hline(yintercept = Moderatezone, linetype = "longdash",color = "orange")+
  geom_hline(yintercept = Lowzone, linetype = "longdash",color = "green")+
  scale_x_continuous(breaks=seq(1,31,3))+
  ggtitle("Monthly Windspeed in JFK and LGA 2013")+
  xlab("Day")+
  ylab("Wind Speed")


#Wind advisory is when a wind warning is issued by National Weather Service
#Conditions: wind_speed > 40 and wind_gust >58
#https://www.usatoday.com/story/weather/talking-weather/2015/05/06/wind-warning-vs-wind-advisory/26966743/

#-----------------------------------Analysis 10--------------------------------#
#Study the effects of precipitation and wind gust
LCJFK = Weather%>%
  select(origin,month,day,hour,wind_gust,precip,wind_dir)%>%
  filter(origin=="JFK")%>%
  filter(wind_gust>17)%>%
  filter(precip>0.1)%>%
  filter(wind_dir>=180 & wind_dir <=270)
View(LCJFK)

RCJFK = Weather%>%
  select(origin,month,day,hour,wind_gust,precip,wind_dir)%>%
  filter(origin=="JFK")%>%
  filter(wind_gust>17)%>%
  filter(precip>0.1)%>%
  filter(wind_dir >=0 & wind_dir <=90)
View(RCJFK)

#https://thepointsguy.com/news/how-windy-does-it-have-to-be-before-planes-cant-take-off/#:~:text=Aircraft%20do%20have%20an%20additional,should%20not%20exceed%2045%20knots
#https://www.weathershack.com/static/ed-rain-measurement.html#:~:text=Rainfall%20amount%20is%20described%20as,that%20is%20one%20inch%20deep
# windgust = 15knots/17mph On wet days 
# precip > 0.1 means light precipitation , can be drizzle or light snow
#------------------------------------------------------------------------------#

#-----------------------------------Analysis 11--------------------------------#
#Precipitation Hours 
#Count the total precipitation hours in a month based on JFK and LGA

PrecipitationCategory = case_when(Weather2.0$precip == 0.0 ~ "None",
                                  Weather2.0$precip <= 0.1 ~ "Light", 
                                  Weather2.0$precip <= 0.3 ~ "Moderate",
                                  Weather2.0$precip >= 0.3 ~ "Heavy")

#If the precipitation reading from table Weather2.0 is = 0 means no precipitation
#if it is lesser than 0.1 then light precipitation is happening
#ranges above 0.1 to 0.3 would indicate moderate precipitation while anything more
#indicates heavy precipitation
Weather2.0 = cbind(Weather2.0, PrecipitationCategory)
#Overwrite previous data from Weather2.0 table with added column called 
#PrecipitationCategory
                                                    
View(Weather2.0)# To view the new data 

JFKAveragePrecip = Weather2.0%>%#get data from Weather2.0 and place it into JFKAveragePrecip
  filter(origin=="JFK")%>%#get data with origin JFK 
  filter(month=="1")%>%#narrow down data to only January
  group_by(day,precip,PrecipitationCategory)%>%#groups data into day,precip and 
  select(day,precip,PrecipitationCategory)#PrecipitationCategory, #choose to display
View(JFKAveragePrecip) #only data from the day, precip and PrecipitationCategory column
                     #View() show me the data in the specified table 
LGAAveragePrecip = Weather2.0%>%#get data from Weather2.0 and place in LGAAveragePrecip
  filter(origin=="LGA")%>%#only show data related to JFK Airport
  filter(month=="1")%>%#only show data from January 
  group_by(day,precip,PrecipitationCategory)%>%#groups data into day, precip and 
  select(day,precip,PrecipitationCategory)#PrecipitationCategory #choose to display
View(LGAAveragePrecip)#only day,precip and PrecipitationCategory with select() 
                    #View(LGAAveragePrecip) allow me to see data in LGAAveragePrecip table 
JFKAveragePrecipGraph = ggplot(JFKAveragePrecip, #plot a graph getting data from JFKAveragePrecip
                         aes(x = PrecipitationCategory, #sets argument x as..  
                             fill = PrecipitationCategory)) + #PrecipitationCategory
                             geom_bar(bins=20) + #fill give different colors to different types of..
                             #precipitation category#bins changes graph sensitivity
                             scale_y_continuous(breaks=seq(0,744,50))+#plots a bar graph
                             ylab("Hours")
                            #scale_x_continuous sets y axis to start at 0 and end at 744 
                            #because the maximum hours in a month is 744 hours
LGAAveragePrecipGraph = ggplot(LGAAveragePrecip,#plots a graph with data from LGAAveragePrecip
                             aes(x = PrecipitationCategory,
                             fill = PrecipitationCategory)) +
                             geom_bar(bins=20) + 
                             scale_y_continuous(breaks=seq(0,744,50))+
                             ylab("Hours")

HoursPrecipGraph = ggarrange(JFKAveragePrecipGraph,#ggarrange helps combines 2 graphs 
                             LGAAveragePrecipGraph,#into one page 
                             labels = c("JFK Hours with Precipitation",#helps label the graphs
                                        "LGA Hours with Precipitation"))#according to the names
       
HoursPrecipGraph

#---------------------------------Analysis 12--------------------------#
#dew point and frequencies during 2013

Dew1= Weather %>%#store specific data from Weather data frame into sub table Dew1
  group_by(month,day,hour,dewp,temp,humid) %>%#groups data by the variables specified
  filter(origin=="JFK")%>%#in the bracket, get data where origin is JFK
  select(month,dewp,temp,humid)#only show selected data to be displayed in Dew1
View(Dew1)#Displays all data stored in Dew1

ggplot(Dew1, aes(x = dewp, y=as.factor(month), fill=factor(month)))+#gets month and dewp
              geom_density_ridges(scale = 5, alpha = 0.6)+#from table Dew1 
              #creates a density graph filling the region below it. 
              #with a scale of 5, the higher the more sensitive
              #alpha sets the transparency of the fill
              scale_x_continuous (breaks=seq(-10,100,5))+#sets x-axis to start 
                                                         #from -10 and end at 100
              ylab("Month")+#change the label of y axis
              xlab("Dewpoint (°F)")+#change the label of x axis
              ggtitle("Dewpoint Level and Frequency throughout 2013")
              #change the title of the graph
              
#---------------------------------Analysis 13----------------------------------#
#Dew point VS Humidity on a Specific Day
Dew2= Weather%>%
  group_by(month,day,hour,dewp,temp,humid)%>%
  filter(origin=="JFK")%>%
  filter(month=="1")%>%
  filter(day=="11")%>%
  filter(hour>=0 & hour<=23)%>%
  select(month,day,hour,dewp,temp,humid)
View(Dew2)

ggplot(Dew2, aes(x = hour, y=dewp, color=humid)) +
  geom_line()+
  scale_y_continuous (breaks=seq(-10,100,5))+
  scale_x_continuous (breaks=seq(0,23,1))+
  ylab("Dewpoint(°F)")+
  xlab("Hours")+
  ggtitle("Dewpoint & Humidity Vs Hours of 11th of January")


#-------------------------------Analysis 14------------------------------------#
#Cross Wind in JFK Airport (Planes using Runway 22R)
RcrosswindJFK22R = Weather %>%
  filter(wind_gust > 40)%>%
  filter(origin == "JFK")%>% 
  filter(wind_dir >=265 & wind_dir <= 355)%>%
  select(month,day,hour,wind_gust,wind_dir)

LcrosswindJFK22R = Weather %>%
  filter(wind_gust > 40)%>%
  filter(origin == "JFK") %>%
  filter(wind_dir >= 85 & wind_dir <=175)%>%
  select(month,day,hour,wind_gust,wind_dir)

View(LcrosswindJFK22R)
View(RcrosswindJFK22R)
