# This scripe file was created to produce a graph of 
# Type of crime with biggest increase (2004-2015) 
# among different counties in Ireland
# 
 

#####################
# loading libraries
#####################

library(tidyverse)
library(ggplot2)
library(sf)

################
# sourcing data
################

# source:  https://data.gov.ie/dataset/crimes-at-garda-stations-level-2010-2016
crime <- read_csv("datasets/garda_stations.csv")
# source:  https://en.wikipedia.org/wiki/List_of_Irish_counties_by_population
population <- read_csv("datasets/population.csv")
# source: http://census.cso.ie/censusasp/saps/boundaries/ED_SA%20Disclaimer1%20-%20bkup20173107.htm
ireland_map <- st_read("shape-files/Census2011_Admin_Counties_generalised20m.shp")

###########################################################################################
# Creating a data set of change in crime from 2004 to 2015 by county for each type of crime
###########################################################################################

dat <-
  crime %>%
  gather(key="crime", value="number", -id,- Station, -Divisions, -x, -y) %>%
  mutate(temp_1 = str_replace(crime, "[*]", "")) %>%
  mutate(year = str_sub(temp_1, -4)) %>%
  mutate(temp_2 = str_sub(temp_1, end=-6))%>%
  mutate(crtype = recode(temp_2, 
                         `Attempts or threats to murder, assaults, harassments and related offences`="murder",
                         `Burglary and related offences`="burglary",  
                         `Controlled drug offences`="drug"        ,                          
                         `Damage to property and to the environment`="damage",
                         `Dangerous or negligent acts`="negligent"            ,                         
                         `Fraud, deception and related offences`="fraud"  ,
                         `Kidnapping and related offences`="kidnapping"  ,
                         `Offences against government, justice procedures and organisation of crime`="government" ,
                         `Public order and other social code offences`="social",
                         `Robbery, extortion and hijacking offences` ="robbery" ,
                         `Theft and related offences`="theft",
                         `Weapons and Explosives Offences`="weapons"))%>%
  mutate(county=recode(Divisions,
                       `Cavan/Monaghan Division`="Cavan/Monaghan",
                       `Clare Division`="Clare",
                       `Cork City Division`="Cork", 
                       `Cork North Division`="Cork",
                       `Cork West Division`="Cork",       
                       `D.M.R. Eastern Division`="Dublin", 
                       `D.M.R. North Central Division`="Dublin",      
                       `D.M.R. Northern Division`="Dublin", 
                       `D.M.R. South Central Division`="Dublin", 
                       `D.M.R. Southern Division`="Dublin",       
                       `D.M.R. Western Division`="Dublin",              
                       `Donegal Division`="Donegal", 
                       `Galway Division`="Galway",                
                       `Kerry Division`="Kerry",              
                       `Kildare Division`="Kildare", 
                       `Kilkenny/Carlow Division`="Kilkenny/Carlow",         
                       `Laois/Offaly Division`="Laois/Offaly",             
                       `Limerick Division`="Limerick", 
                       `Louth Division`="Louth",                 
                       `Mayo Division`="Mayo",                
                       `Meath Division`="Meath", 
                       `Roscommon/Longford Division`="Roscommon/Longford",        
                       `Sligo/Leitrim Division`="Sligo/Leitrim",            
                       `Tipperary Division`="Tipperary", 
                       `Waterford Division`="Waterford",                     
                       `Westmeath`="Westmeath",            
                       `Westmeath Division`="Westmeath", 
                       `Wexford Division`="Wexford",              
                       `Wicklow Division`="Wicklow"))%>%
  select(county, year, crtype, number) %>%
  filter(!(year=="2003" | year=="2016"))%>%
  group_by(county,year, crtype) %>%
  summarise(tot_number=sum(number)) %>%
  full_join(population, by=c("county")) %>%
  mutate(rel_number=(tot_number/population)*1000 ) %>%
  ungroup()%>%
  select(-tot_number, -population) %>%
  filter(year=="2004" | year=="2015") %>%
  unite("crtype_year",crtype, year)%>% 
  spread(key=crtype_year, rel_number) %>%
  mutate(burglary_change = (burglary_2015-burglary_2004),
         damage_change = (damage_2015-damage_2004),
         drug_change = (drug_2015-drug_2004),
         fraud_change = (fraud_2015-fraud_2004),
         government_change = (government_2015-government_2004),
         kidnapping_change = (kidnapping_2015-kidnapping_2004),
         murder_change = (murder_2015-murder_2004),
         negligent_change = (negligent_2015-negligent_2004),
         robbery_change = (robbery_2015-robbery_2004),
         social_change = (social_2015-social_2004),
         theft_change = (theft_2015-theft_2004),
         weapons_change = (weapons_2015-weapons_2004)
         )%>%
  select(county,burglary_change, damage_change, drug_change, fraud_change, government_change, 
         kidnapping_change, murder_change, negligent_change, robbery_change, social_change,
         theft_change,weapons_change )%>%
  gather("crime_type", "change",-county)%>%
  mutate(crime_type=str_sub(crime_type, end=-8))

##########################################################################
# creating a data frame with type of crime with max change for each county
##########################################################################

ldat <- split(dat,dat$county)  

extractMaxchange <- function(x){
  ind_max <- x$change == max(x$change)
  return(x[ind_max,])
}

fdat <- do.call("rbind",lapply(ldat, extractMaxchange) ) %>%
  rename(cname=county)

######################################
# merging crime data with shape file
######################################

ireland_map <- 
  ireland_map %>%
  mutate(cname = recode(COUNTYNAME,
                        `Limerick City`="Limerick",
                        `Limerick County`="Limerick",
                        `North Tipperary`="Tipperary",
                        `South Tipperary`="Tipperary",
                        `Waterford City`="Waterford"  ,         
                        `Waterford County`= "Waterford",         
                        `Galway City`= "Galway"         ,     
                        `Galway County`=  "Galway"       ,   
                        `Leitrim County`="Sligo/Leitrim"  ,          
                        `Mayo County`= "Mayo"              ,
                        `Roscommon County`= "Roscommon/Longford",         
                        `Sligo County`="Sligo/Leitrim"           ,  
                        `Cavan County`= "Cavan/Monaghan"        ,     
                        `Donegal County`= "Donegal"         ,  
                        `Monaghan County`="Cavan/Monaghan"   ,        
                        `Carlow County`="Kilkenny/Carlow"     ,      
                        `Dublin City`=  "Dublin"             ,
                        `South Dublin`= "Dublin"            ,
                        `Fingal`= "Dublin"                  , 
                        `D\xfan Laoghaire-Rathdown`="Dublin",
                        `Kildare County`= "Kildare"          ,
                        `Kilkenny County`= "Kilkenny/Carlow"  ,        
                        `Laois County`=  "Laois/Offaly"        ,    
                        `Longford County`="Roscommon/Longford"  ,        
                        `Louth County`= "Louth"             ,
                        `Meath County`= "Meath"             ,
                        `Offaly County`= "Laois/Offaly"      ,     
                        `Westmeath County`="Westmeath"        , 
                        `Wexford County`= "Wexford"           ,
                        `Wicklow County`= "Wicklow"           ,
                        `Clare County`="Clare"              ,
                        `Cork City`= "Cork"               ,
                        `Cork County`= "Cork"              ,
                        `Kerry County`="Kerry"             ))

ireland_map[20, c("cname")] <- "Dublin"

ireland <- full_join(ireland_map, fdat, by=c("cname"))

#####################################
# creating nice label for each county
#####################################

index_unique <- !duplicated(ireland_map$cname)


label_dat <- 
  ireland[index_unique ,] %>%
  mutate(crime_type=recode(crime_type,  
                           `burglary`="burglary",
                           `drug`="drug",      
                           `fraud`="fraud", 
                           `government`="off. ag. govt.",     
                           `murder`="murder",      
                           `theft`="theft"))%>%
  mutate(clabel=paste0(cname,"\n(",crime_type,")")) 

###################
# plotting the data 
###################

ggplot(ireland) +
  geom_sf(aes(fill = change))+
  geom_sf_label(data=label_dat,aes(label = clabel))+
  scale_fill_gradient2(name="Increase in crime\n per 1000 population",low = "blue", mid = "white",high = "red", space = "Lab" )+
  xlab("")+
  ylab("")+
  ggtitle("Type of crime with biggest increase (2004-2015) among different counties in Ireland")+
  theme(legend.position = c(0.85, 0.1),
        axis.text=element_blank(),
        axis.ticks=element_blank())

###################
# end of the script
###################





