library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
climate = read_csv("climate-data.csv")
#remove unwanted columns
climate$`Country Code` = NULL
climate$`Indicator Code` = NULL
#rename columns
names(climate)[names(climate)=="Country Name"] = "Country"
names(climate)[names(climate)=="Indicator Name"] = "Indicator"

#us data
us_data = climate[climate$Country == "United States",]
#extract data on World
world_data = climate[climate$Country == "World",]


#GREENHOUSE EMISSIONS----------------------------------------------------------------------------------
#US
us_ghe = us_data[us_data$Indicator == "Total greenhouse gas emissions (% change from 1990)",]
us_ghe = us_ghe[!is.na(us_ghe),]
us.tidy = gather(us_ghe, Year, Value, -Country, -Indicator, na.rm =T) #tidy text
us.tidy$Value = as.numeric(us.tidy$Value)
#World
world_ghe = world_data[world_data$Indicator == "Total greenhouse gas emissions (% change from 1990)",]
world_ghe = world_ghe[!is.na(world_ghe),]
world.tidy = gather(world_ghe, Year, Value, -Country, -Indicator, na.rm =T) #tidy text
world.tidy$Value = as.numeric(world.tidy$Value)

#ENERGY OVER TIME--------------------------------------------------------------------------------------
#US--------------
us.energy = us_data[grep("Electricity production from",us_data$Indicator),]
us.energy = us.energy[grep("(% of total)", us.energy$Indicator),]
us.energy.tidy = gather(us.energy,Year, Value, -Country, -Indicator, na.rm=T)
us.energy.tidy$Value = as.numeric(us.energy.tidy$Value)
#Combine renewables and hydroelectric
Renewable = us.energy.tidy[us.energy.tidy$Indicator=="Electricity production from renewable sources, excluding hydroelectric (% of total)",]$Value + 
  us.energy.tidy[us.energy.tidy$Indicator=="Electricity production from hydroelectric sources (% of total)",]$Value
temp = data.frame(Country=rep("United States", 56),Indicator=rep("Renewables", 56), Year = 1960:2015, Value=Renewable)
us.energy.tidy = rbind(us.energy.tidy,temp)
#remove unwanted rows
unwanted = c("Electricity production from renewable sources, excluding hydroelectric (% of total)","Electricity production from hydroelectric sources (% of total)")
us.energy.tidy = us.energy.tidy[us.energy.tidy$Indicator !=unwanted[[1]],]
us.energy.tidy = us.energy.tidy[us.energy.tidy$Indicator !=unwanted[[2]],]

#rename columns
#us.energy.tidy

#WORLD
world.energy = world_data[grep("Electricity production from",world_data$Indicator),]
world.energy.tidy = gather(world.energy,Year, Value, -Country, -Indicator, na.rm=T)
world.energy.tidy = world.energy.tidy[grep("(% of total)", world.energy.tidy$Indicator),]
world.energy.tidy$Value = as.numeric(world.energy.tidy$Value)
#combine renewable sources
Renewable = world.energy.tidy[world.energy.tidy$Indicator=="Electricity production from renewable sources, excluding hydroelectric (% of total)",]$Value + 
  world.energy.tidy[world.energy.tidy$Indicator=="Electricity production from hydroelectric sources (% of total)",]$Value

temp = data.frame(Country=rep("World", 44),Indicator=rep("Renewables", 44), Year = 1971:2014, Value=Renewable)
world.energy.tidy = rbind(world.energy.tidy,temp)
#remove unwanted rows
unwanted = c("Electricity production from renewable sources, excluding hydroelectric (% of total)","Electricity production from hydroelectric sources (% of total)")
world.energy.tidy = world.energy.tidy[world.energy.tidy$Indicator !=unwanted[[1]],]
world.energy.tidy = world.energy.tidy[world.energy.tidy$Indicator !=unwanted[[2]],]

ggplot(world.energy.tidy,aes(x=Year,y=Value, color=Indicator)) + geom_point()+theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle ("World Energy Trends") + ylab("(% of Total)")

#rename World
world.energy.tidy$Indicator = world.energy.tidy$Indicator %>% lapply(function(x){
  if(x=="Electricity production from oil sources (% of total)")
    return(x="Oil")
  if(x=="Electricity production from nuclear sources (% of total)")
    return(x="Nuclear")
  if(x=="Electricity production from natural gas sources (% of total)")
    return(x="Natural Gas")
  if(x=="Electricity production from coal sources (% of total)")
    return(x="Coal")
  if(x=="Renewables")
    return(x="Renewables")
})
#rename US
us.energy.tidy$Indicator = us.energy.tidy$Indicator %>% lapply(function(x){
  if(x=="Electricity production from oil sources (% of total)")
    return(x="Oil")
  if(x=="Electricity production from nuclear sources (% of total)")
    return(x="Nuclear")
  if(x=="Electricity production from natural gas sources (% of total)")
    return(x="Natural Gas")
  if(x=="Electricity production from coal sources (% of total)")
    return(x="Coal")
  if(x=="Renewables")
    return(x="Renewables")
})
View(us.energy.tidy)
class(us.energy.tidy$Indicator)
us.energy.tidy$Indicator = as.character(us.energy.tidy$Indicator)
class(world.energy.tidy$Indicator)
world.energy.tidy$Indicator = as.character(world.energy.tidy$Indicator)

ggplot(us.energy.tidy,aes(x=as.numeric(Year),y=Value, fill=factor(Indicator))) + geom_area() + theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +ggtitle("US Energy Trends") + ylab("(% of Total)")

ggplot(world.energy.tidy,aes(x=as.numeric(Year),y=Value, fill=factor(Indicator))) + geom_area() +theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle ("World Energy Trends") + ylab("(% of Total)")

#CO2 --------------------------------------------------------------------------------------------------
#US----------------------------
us.co2 = us_data[grep("CO2 emissions", us_data$Indicator),]
us.fuel = us.co2[grep("fuel consumption \\(\\%", us.co2$Indicator),]
us.fuel.ty = gather(us.fuel,Year,Value,-Country,-Indicator, na.rm=T)
us.fuel.ty$Value = as.numeric(us.fuel.ty$Value)
us.fuel.ty$Year = as.numeric(us.fuel.ty$Year)
#WORLD-------------------------
world.co2 = world_data[grep("CO2 emissions", world_data$Indicator),]
world.fuel = world.co2[grep("fuel consumption \\(\\%", world.co2$Indicator),]
world.fuel.ty = gather(world.fuel,Year,Value,-Country,-Indicator, na.rm=T)
world.fuel.ty$Value = as.numeric(world.fuel.ty$Value)
world.fuel.ty$Year = as.numeric(world.fuel.ty$Year)

#renaming indicators
us.fuel.ty$Indicator = sapply(us.fuel.ty$Indicator, function(x){
  if(x=="CO2 emissions from solid fuel consumption (% of total)")
    return("Solid")
  if(x=="CO2 emissions from liquid fuel consumption (% of total)")
    return("Liquid")
  if(x=="CO2 emissions from gaseous fuel consumption (% of total)")
    return("Gaseus")
})
class(us.fuel.ty$Indicator)
world.fuel.ty$Indicator = sapply(world.fuel.ty$Indicator, function(x){
  if(x=="CO2 emissions from solid fuel consumption (% of total)")
    return("Solid")
  if(x=="CO2 emissions from liquid fuel consumption (% of total)")
    return("Liquid")
  if(x=="CO2 emissions from gaseous fuel consumption (% of total)")
    return("Gaseus")
})





