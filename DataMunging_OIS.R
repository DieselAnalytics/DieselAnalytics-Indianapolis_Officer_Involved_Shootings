library(dplyr); library(readr); library(lubridate); library(stringr); library(tidyr); library(stringr)

setwd("D:/OneDrive - Diesel Analytics/Data/IndyPoliceData/Data")

#Calculates generation base on the age at the time of incident
get.generation <-function(age, date.value){
  
  born.year = year(ymd(date.value)) - age
  
  ifelse(born.year >= 1981 & born.year <= 1994, "generation y",
  ifelse(born.year >= 1960 & born.year <= 1980, "generation x", 
  ifelse(born.year < 1960, "baby boomer",
  "very young adult"))) -> gen
  
  return(gen)
}

#Converts the years of service to years of service range
get.years.of.service.range <-function(years.of.service){
  
  ifelse(years.of.service <= 5, "<=5",
  ifelse(years.of.service > 5 & years.of.service <= 10, "6-10", 
  ifelse(years.of.service > 10 & years.of.service <= 20, "11-20",
  ">20"))) -> gen
  
  return(gen)
}

#Imports the "Officer Involved Shooting" data then slices the columns needed for the analysis
ois <- read_csv("ois.csv")
ois <- ois[c('id', 'occurredDate', 'residentWeaponUsed','officerWeaponUsed','serviceType','residentCondition',
             'officerCondition','residentRace','residentSex','residentAge','officerRace','officerSex','officerAge',
             'officerYearsOfService','officerIdentifier')]

#Replaces the fields that contains empty strings with "Not Given"
ois$serviceType[ois$serviceType==""] <- "Not Given"
ois$residentRace[ois$residentRace==""] <- "Not Given"
ois$residentSex[ois$residentSex==""] <- "Not Given"
ois$residentCondition[ois$residentCondition==""] <- "Not Given"
ois$residentCondition[ois$residentCondition=="No injuries noted or visible"] <- "No visible injuries"
ois$officerRace <- paste(ois$officerRace, "Officers", sep=" ")
ois$officerRace[ois$officerRace==" Officers"] <- "Not Given"
ois$officerSex[ois$officerSex==""] <- "Not Given"
ois$officerCondition[ois$officerCondition==""] <- "Not Given"
ois$officerCondition[ois$officerCondition=="No injuries noted or visible"] <- "No visible injuries"
ois$residentWeaponUsed[ois$residentWeaponUsed ==""] <- "Not Given" 
ois$residentWeaponUsed[ois$residentWeaponUsed !="Not Given" & ois$residentWeaponUsed !="Unarmed"] <- "Armed"
ois$officerWeaponUsed[ois$officerWeaponUsed ==""] <- "Not Given" 
ois$officerWeaponUsed[ois$officerWeaponUsed !="Not Given"] <- "Armed"

#Change the data types of some of the fields and slices out the fields that I want to output to Power BI
ois <-
  ois %>%
  mutate(incident.id = id,
         residentAge = as.integer(ois$residentAge),
         officerAge = as.integer(ois$officerAge),
         occurredDate = as.Date(ois$occurredDate),
         officer.gen = paste(get.generation(as.integer(officerAge), as.Date(occurredDate)), "officer", sep = " "),
         resident.gen = get.generation(as.integer(residentAge), as.Date(occurredDate)),
         years.of.service.range = get.years.of.service.range(officerYearsOfService)
  ) %>%
  select(incident.id, occurredDate, serviceType, residentRace, residentSex, resident.gen, residentWeaponUsed, 
         residentCondition, officerIdentifier, officerRace, officerSex, officer.gen, officerWeaponUsed, 
         officerCondition, years.of.service.range)

ois <- data.frame(ois)

#Creates a table of the officer involved shootings by race. Import the demographic data and pivots it on the 
#CityOrDepartment field.Then adds fields to calculates city.percent, department.percent, and percentage.delta.
#Finally merge the "ois by race" table with the "demographic" table.

shootings.by.race <- unique(ois[,c("officerIdentifier", "officerRace")]) # there were some officers that was in the data set multiple times so this was done in order not to double count them
shootings.by.race <- data.frame(table(shootings.by.race$officerRace))
colnames(shootings.by.race) <- c("race","shootings")
shootings.by.race$race <- as.character(shootings.by.race$race)
shootings.by.race$race <- str_replace(shootings.by.race$race," Officers","")
shootings.by.race$race[shootings.by.race$race == "Not Given"] <- "Other"

demographics <- read_csv("demographics.csv")
demographics <-
  demographics %>%
  mutate(count = as.integer(count)) %>% 
  spread(cityOrDepartment, count) %>%
  mutate(city.percent = round(city/sum(city),3)
         ,department.percent = round(department/sum(department),3)
         ,percentage.delta = department.percent - city.percent 
  ) %>%
  select(race, department, city, department.percent, city.percent, percentage.delta)
  
demographics <- merge(demographics, shootings.by.race, all.x = TRUE)
demographics$shootings[is.na(demographics$shootings)] <- 0
demographics$shootings.percent <- round(demographics$shootings/sum(demographics$shootings),3)
demographics <- data.frame(demographics)
demographics$race <- ifelse(demographics$race=="Other","Unknown Race Officers", paste(demographics$race, "Officers", sep=" "))

#Creates date table that will be used by Power BI
min.date <- min(ois$occurredDate); max.date <- max(ois$occurredDate)
Dates <- seq(ymd(min.date), ymd(max.date), by="days") 

DateTable <- data.frame(Dates)

DateTable <- 
  DateTable %>%
  mutate("DateKey" = format(Dates, "%Y%m%d")
         ,"Year" = year(Dates)
         ,"Month Name" = format(Dates, "%b")
         ,"Month Key" = month(Dates)
         ,"Month Day" = mday(Dates)
         ,"Weekday Name" = as.character(wday(Dates, label = TRUE)) #if you don't do this Power BI will keep as integer
         ,"Weekend" = ifelse(wday(Dates) %in% c(1,7),TRUE,FALSE)
  )