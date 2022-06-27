# Credit: @AK1o9

# US Weather Report Data Management & Analysis


  #Installing extra packages:
  #N/A

  #Loading packages:
  install.packages('ggthemes')
  install.packages('extrafont')
  install.packages('tidyr')
  library(dplyr)
  library(ggplot2)
  library(plotrix)
  library(ggthemes)
  library(extrafont)
  library(tidyr)
  
  font_import() #Run only once (For the first time)
  
  
  #Importing .csv data file:
  forecast <- read.csv("C:\\Users\\Ahmed\\Documents\\Uni\\B.Sc Degree\\PFDA\\weather.csv",header=TRUE)
  forecast
  
  class(forecast) #data is in a data.frame()
  
  #Column names/headers of data:
  colnames(forecast)
  
  #No. of rows & columns:
  nrow(forecast)  #366 rows
  ncol(forecast)  #22 columns
  
  #Data preview:
  head(forecast,5)
  tail(forecast,3)

#Question 1: How many days of summer, or warm enough days, has there been the last year?
Q1 = function(){
  #Analysis 1-1: Find the summer season & display amount of days within.
  summary(forecast$Sunshine)  #Obtain Sum's MEAN(7.909) from summary
  filter(forecast,Sunshine>7.909)#Sunny days
  maxtemp.avg= mean(forecast$MaxTemp)
  filter(forecast,MaxTemp>maxtemp.avg)#Hot days
  summer=filter(forecast,Sunshine>7.909,MaxTemp>maxtemp.avg)
  summer
  message("\n>>In total, there are ", nrow(summer)," days of summer!\n")  #Days of summer
}
Q1()

#Question 2: When should we carry umbrellas with us?
Q2 = function(){
  #Analysis 2-1: Find out and show the days its expected to rain.
  rainfall.avg=mean(forecast$Rainfall)
  rainfall.days = filter(forecast,Rainfall>rainfall.avg,RainToday=='Yes',Evaporation>mean(Evaporation),RISK_MM>mean(RISK_MM))
  rainfall.days = select(rainfall.days,Rainfall,RainToday,Evaporation,RISK_MM)
  rainfall.days
  #Analysis 2-2: Find out the the chances of it raining & not raining each day
  values = c(nrow(rainfall.days),(nrow(forecast)-nrow(rainfall.days)))
  pie(values, labels=c("Raining","Not raining"), radius=1,main="Likeliness of it raining each day")
  
} 
Q2()

#Question 3: Throughout last year, which wind direction was the rainiest?
Q3 = function(){
  
  #Analysis 3-1: Identify the wind direction with the highest potential amount of rainfall
  rainfall.WindGustDir.rainiest = tapply(forecast$Rainfall,INDEX=forecast$WindGustDir,FUN=mean) #Likelihood of rainfall grouped by Wind Direction
  rainfall.WindGustDir.rainiest.Dir = max(names(rainfall.WindGustDir.rainiest)) # Result in abbreviation
  rainfall.WindGustDir.rainiest.Dir.name = switch(rainfall.WindGustDir.rainiest.Dir,'N'='North','W'='West','E'='East','S'='South',
                                                  'NE'='North-east','NW'='North-west','SE'='South-east','SW'='Soutch-west',
                                                  'ENE'='East & North-east','ESE'='East & South-east','NNE'='North & North-east',
                                                  'NNW'='North & North-west','SSE'='South & South-east','SSW'='South & South-west',
                                                  'WNW'='West & North-west','WSW'='West & South-west')
  message("\n>>The wind direction with the strongest winds would be: ",rainfall.WindGustDir.rainiest.Dir.name,"\n")  # Result clarified
}
Q3()

#Question 4: What are all the different possibilities of what the weather would be like tomorrow?
Q4 = function(){
  #Analysis 4-1: Show a graph depicting  the likelihood of every weather condition
  weather = select(forecast, RainTomorrow, RISK_MM, Humidity9am, Humidity3pm, WindSpeed9am, WindSpeed3pm,Cloud9am, Cloud3pm)
  #%>% mutate(weather = ifelse(RainTomorrow == "YES","Rain","Not Rain"))
  #nrow(weather)
  
  rainy = subset(weather, RainTomorrow=='Yes') #Days likely to rain
  non_rainy = subset(weather, RainTomorrow=="No") #Days unlikely to rain.
  
  #Analysis 4-1-1: Cloudy days
  cloudy = filter(non_rainy, (Cloud9am+Cloud3pm)>mean(Cloud9am+Cloud3pm)) #cloud 9 & 3 > mean & RainTomorrow = no
  cloudy.days = nrow(cloudy)
  
  #Analysis 4-1-2: Windy days*
  non_rainy.Cleaned.9amWindSpeed = filter(non_rainy, WindSpeed9am>=0) #WindSpeed9am is data cleaned (Removed 'NA' records)
  windy = filter(non_rainy.Cleaned.9amWindSpeed, (WindSpeed9am + WindSpeed3pm)>mean(WindSpeed9am + WindSpeed3pm)) #Wind 9 & 3 > mean & RainTom = now
  windy.days = nrow(windy)
  
  #Analysis 4-1-3: Rainy days
  rainy.days.records =  filter(rainy, RISK_MM>mean(RISK_MM))#RainTom = yes & Risk_MM > mean
  rainy.days = nrow(rainy.days.records)
  
  #Analysis 4-1-4: Sunny days
  sunny = filter(non_rainy, (Humidity9am + Humidity3pm)>mean(Humidity9am + Humidity3pm)) #RainTom = no & Humid 3 & 9 > mean
  sunny.days = nrow(sunny)
  
  values=c(cloudy.days,windy.days,rainy.days,sunny.days)
  weather.names = c("Cloudy","Windy","Rainy","Sunny")
  pie(values,labels=weather.names,main="What will tomorrow's weather be like?",col=c('white','light blue','dark blue','yellow'))
}  #1 (4 subs)
Q4()

#Question 5: What were the hottest and coldest temperatures during the year?
Q5 = function(){
  #Analysis 5-1: Display the hottest & coldest temperatures
  hottest = max(forecast$MaxTemp)
  coldest = min(forecast$MinTemp)
  message(">>The temperature recording are as follows:\n1.Recent hottest: ",hottest," Celsius\n2.Recent coldest: ",coldest," Celsius")
}
Q5()

#Question 6: What are the chances that it'll rain today?
Q6 = function(){
  #Analysis 6-1: Show the chances of it raining today (From previous data)
  today.raining=subset(forecast,RainToday=="Yes")
  today.not_raining=subset(forecast,RainToday=='No')
  today.rain_or_not = summarise(forecast,Rain=(nrow(today.raining))/(nrow(today.raining)+nrow(today.not_raining))*100,
                                No_rain=nrow(today.not_raining)/(nrow(today.raining)+nrow(today.not_raining))*100)
  print("Chances of it raining VS not raining: ")
  print(round(today.rain_or_not,2))
  values = c(nrow(today.raining),nrow(today.not_raining))
  pie3D(values,labels=c("Yes","No"),main="Will it rain today?",col=c('red','light green'))
}  
Q6()

#Question 7: How strong will the wind be in the afternoon relative to the morning? (Or vice-versa)
Q7 = function(){
  #Analysis 7-1: Find how windspeeds in the morning compare to those in the afternoon
  windspeeds = select(forecast, WindSpeed9am, WindSpeed3pm) %>% mutate(Stronger_WindSpeed = ifelse(WindSpeed9am>WindSpeed3pm,"Morning","Afternoon"))
  
    #Analysis 7-2: Find out the chances, in percentage, of when the wind will be stronger. (Morning,Afternoon or Both)
    values = c(nrow(filter(windspeeds,Stronger_WindSpeed=="Morning")),nrow(filter(windspeeds,Stronger_WindSpeed=="Afternoon")),
               nrow(filter(windspeeds,is.na(Stronger_WindSpeed))))
    
    message("\n>>Chances of stronger windspeeds: \n\n1.Morning: ",(values[1]/sum(values)*100)," %\n2.Afternoon: "
            ,(values[2]/sum(values)*100)," %\n3.Similar/mutual: ",(values[3]/sum(values)*100)," %\n")
  
  ggplot(windspeeds,aes(x=WindSpeed9am,y=WindSpeed3pm))+geom_point(color="sky blue")+facet_wrap(~WindSpeed3pm)
  
}  #2
Q7()

#Question 8: When is the best time to go camping?
Q8 = function(){
  #Analysis 8-1: Find out when it's warm
  no_heavy_rain = filter(forecast, Rainfall<mean(Rainfall)) #Eliminate days with Above average - Heavy rainfall.
  clear_weather = select(no_heavy_rain, Sunshine,Evaporation, Humidity9am,Humidity3pm,WindGustSpeed,Pressure9am,Pressure3pm)
  warm = mutate(clear_weather,warmth=ifelse(Humidity9am+Humidity3pm>(mean(Humidity9am+Humidity3pm)*1.25),'Bad','Good'))
  #Analysis 8-2: Find the off-peak season
  warm = filter(warm,Sunshine>=0,WindGustSpeed>=0)
  season.off_peak= filter(warm, Evaporation < mean(Evaporation)*1.4 && Evaporation > mean(Evaporation)*0.7,
                          Pressure9am+Pressure3pm<mean(Pressure9am+Pressure3pm)*1.3,
                          Sunshine > mean(Sunshine)) %>% subset(WindGustSpeed<mean(WindGustSpeed))
  #Analysis 8-3: Figure out the best common time in between days
  season.off_peak %>% subset(warmth=='Good')
}  #3
Q8()

#Question 9: What will the normal temperature be like today?
Q9 = function(){
  #Analysis 9-1: Find the normal expected temperature for the Morning & Afternoon
  forecast.AvgTemp = mutate(forecast,NormalTemp=((MinTemp+MaxTemp)/2)) %>% select(Temp9am,Temp3pm, NormalTemp)
  forecast.AvgTemp = summarise(forecast.AvgTemp,Morning=mean(Temp9am),Afternoon=mean(Temp3pm),Entire_Day = mean(NormalTemp))
  
  #forecast.AvgTemp = table(forecast.AvgTemp[1,],colnames(forecast.AvgTemp))
  values = c(forecast.AvgTemp[1,1],forecast.AvgTemp[1,2],forecast.AvgTemp[1,3])
  forecast.AvgTemp = data.frame(colnames(forecast.AvgTemp),values)
  colnames(forecast.AvgTemp) = c("Time","Average_Temperature")
  ggplot(forecast.AvgTemp,aes(x=Time,y=Average_Temperature))+geom_bar(stat="identity")
}  
Q9()

#Question 10: How dry will it be today?
Q10 = function(){
  #Analysis 10-1: Find the driest days
  dry_days = subset(forecast,Humidity9am>mean(Humidity9am),select=c(Humidity9am,Humidity3pm))#,Humidity3pm>mean(Humidity3pm))# %>% mutate(forecast, Dry = '')
  dry_days = subset(dry_days,Humidity3pm>mean(Humidity3pm)) %>% mutate(Most_Dry=ifelse(Humidity9am>Humidity3pm,Humidity9am,Humidity3pm))
  
    #Analysis 10-2: Calculate the average dryness per day.
    driest = summarise(dry_days,Average_Dryness = (mean(Humidity9am)+mean(Humidity3pm))/2)
    message("\n>>The average measurement units of dryness is: ",round(driest,digits=2),
            "\n\t->That is ",round((driest/max(dry_days$Most_Dry)*100),digits=2),"% of the driest season last year!\n")
  ggplot(dry_days, aes(x=Most_Dry))+geom_histogram(color="brown",aes(fill=..count..))+
    scale_fill_gradient("Count",low="yellow",high="red") # Gradient histogram depicting the driest days and their frequencies
  
}  #2
Q10()

#Question 11: Were there any signs of Global Warming from last year?
Q11 = function(){
  #Analysis 11-1: Find traces of Climate Change(for temperatures only; Not weather). (Higher Max & Min Temperatures)
  forecast.temp_diff = select(forecast,MinTemp,MaxTemp) %>% mutate(TempDifference=MaxTemp-MinTemp)
  chart_choice = (readline(prompt = ">Select:\t'Mx'-for Max Temp's\t'Mn'-for Min Temp's\t'Diff'-for Temp Differences\tChoice: "))
  if(chart_choice!="Mx" && chart_choice!="Mn" && chart_choice!="Diff"){
    message("Wrong input!\n<Please restart function>")
  }else if(chart_choice=="Mx"){
    plot(forecast.temp_diff$MaxTemp,xlab= "Day Number",ylab = "Max Temp Recordings", type="l")
  }else if(chart_choice=="Mn"){
    plot(forecast.temp_diff$MinTemp,xlab= "Day Number",ylab = "Min Temp Recordings", type="l")
  }else{
    chart_choice2= readline(prompt = "Type:\n'B'- For a boxplot graph\n'P' - For a point-plot graph\n'S' - For a scatterplot graph\n")
    if(chart_choice2=="B"){
      ggplot(forecast.temp_diff,aes(y=MinTemp,x=MaxTemp))+geom_boxplot()
    }else if(chart_choice2=="P"){
      plot(forecast.temp_diff$TempDifference,xlab= "Day Number",ylab = "Difference in Min. & Max. Temp's", type="p")
    }else if(chart_choice2=="S"){
      ggplot(forecast.temp_diff,aes(x=MinTemp,y=MaxTemp))+geom_point()+geom_smooth()
    }else{
      message("Wrong input!\n<Please restart function>")
    }
  }
}
Q11()

#Question 12: Just how windy would each wind direction be?
Q12 = function(){
  #Analysis 12-1: Find out how strong winds would be & group them by wind direction.
  forecast.winds=mutate(forecast,WindSpeed=((WindGustSpeed+WindSpeed9am+WindSpeed3pm)/3))%>%select(WindGustDir,WindSpeed)%>%arrange(WindGustDir)
  forecast.winds=na.omit(forecast.winds) #Clean N/A Values
  
  forecast.winds%>%
    ggplot(aes(x=WindSpeed,y=WindGustDir,color=WindGustDir)) + geom_line(size = 3) +
    labs(title = "Wind speed & stength per compass direction",
         subtitle = "Find out how windy it'll be and towards where!",
         x = "Wind Speed & Strength",
         y = "Wind Direction",
         color = "Wind Direction") + theme_fivethirtyeight()
}
Q12()

#Question 13: How windy and what direction will the wind face in the morning and/or afternoon?
Q13 = function(){
  #Anaylsis 13-1: Find the wind speeds and their respective directions.
  forecast.winds = sample_frac(forecast,1)%>%select(WindSpeed9am,WindSpeed3pm,WindDir9am,WindDir3pm)
  forecast.winds = na.omit(forecast.winds)
  forecast.winds = mutate(forecast.winds, WindSpeed = (WindSpeed9am+WindSpeed3pm)/2)
  
  WindDir.MorningToAfternoon = data.frame(forecast.winds$WindDir9am,forecast.winds$WindDir3pm)
  names(WindDir.MorningToAfternoon) = c("Morning","Afternoon")
  
  WindDir.MorningToAfternoon = unite(WindDir.MorningToAfternoon,WindDir,Morning,Afternoon,sep='-')
  forecast.winds = cbind(forecast.winds,WindDir.MorningToAfternoon)
  
  #Analysis 13-2: Make statistical charts and enable users to select whether they'd like to view windiness in the morning, afternoon or both.
  wind_choice = as.integer(readline(prompt = ">Please select either:\t1 - To view Morning wind statistics\t
                                    2 - To view Afternoon wind statistics\t3 - To view wind statistics at both times\nChoice: "))
  if(wind_choice==1){
    ggplot(forecast.winds,aes(x=WindSpeed9am,y=WindDir9am))+geom_bar(stat="identity",fill="orange")
  }else if(wind_choice==2){
    ggplot(forecast.winds,aes(x=WindSpeed3pm,y=WindDir3pm))+geom_bar(stat="identity",fill="dark red")
  }else if(wind_choice==3){
    forecast.winds%>%
      ggplot(aes(x=WindSpeed,y=WindDir,color=WindDir)) + geom_line(size = 2) +
      labs(title = "Wind speed per wind direction from Morning to Afternoon",
           subtitle = "Find out how windy different times will be!",
           x = "Wind Speed",
           y = "Wind Direction (Morning to Afternoon)",
           color = "Wind Direction (Morning to Afternoon)") + theme_excel() +
      theme(axis.title.y = element_text(), text = element_text(family="Arial",size=7) )
  }else{
    message("Wrong Input! <Please restart>")
  }
}  #2
Q13()

#Question 14: What were the average measures of rain pressure?
Q14 = function(){
  #Analysis case 14-1: Find out the average measure of rain pressure in the morning, afternoon and on average throughout the day.
  forecast.RainPressure = select(forecast,Pressure9am,Pressure3pm) %>% mutate(AveragePressure = (Pressure9am+Pressure3pm)/2)
  plot(forecast.RainPressure$AveragePressure,type="l",xlab="Rain Pressure (in Pascals)",ylab="Frequency",main="Rain Pressure",col="red",lty=1,lwd=2)
  lines(forecast.RainPressure$Pressure9am,type="p",xlab = "Rain Pressure (in Pascals)",ylab="Frequency",main="Rain Pressure",col="blue",lty=2)
  lines(forecast.RainPressure$Pressure3pm,type="p",xlab = "Rain Pressure (in Pascals)",ylab="Frequency",main="Rain Pressure",col="dark green",lty=2)
  legend("topleft", legend=c("Average Pressure", "Morning Pressure","Afternoon Pressure"),
         col=c("red", "blue","dark green"), lty=c(1,2,2), lwd = c(2,1,1), cex=0.8,
         title="Lines",box.lty = 0)
}  
Q14()


#Total analysis cases: 19
