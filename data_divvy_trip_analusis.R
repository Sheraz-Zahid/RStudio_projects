
install.packages("tidyverse")
install.packages("readr")
install.packages("skimr")
install.packages("data.table")
install.packages("dplyr")
install.packages("janitor")
library(tidyverse)
library(readr)
library(skimr)
library(data.table)
library(dplyr)
library(lubridate)
library(janitor)



#Function to load and transform date
re_name1 <- c("trip_id", "start_time", "end_time" ,"bikeid", "tripduration",
              "from_station_id","from_station_name","to_station_id", "to_station_name" ,"usertype",       
              "gender" ,"birthyear")    
re_name2 <- c("trip_id","ride_type", "start_time", "end_time" ,
              "from_station_name","start_station_id","to_station_name","end_station_id", "start_latiitude" ,"start_lng",       
              "end_latitude" ,"end_lng","usertype")
setwd("C:/Users/PC/Desktop/Case_study_data/Case_strudy_data/trip_data")
df <- 
  list.files(path = "C:/Users/PC/Desktop/Case_study_data/Case_strudy_data/trip_data", pattern = "*.csv") %>%
  map_df(~fread(.,
                colClasses = c(tripduration = "character",start_time = "character",
                               end_time = "character"),col.names = re_name1))
setwd("C:/Users/PC/Desktop/Case_study_data/Case_strudy_data/Case_study_data")
df2 <- 
  list.files(path = "C:/Users/PC/Desktop/Case_study_data/Case_strudy_data/Case_study_data", pattern = "*.csv") %>%
  map_df(~fread(.,na.strings = "NA",
                colClasses = c(tripduration = "character",start_time = "character",
                               end_time = "character",start_station_id = "character",end_station_id = "character"),col.names = re_name2))

# 1st data from with 12 variables

df$from_station_id <- as.character(df$from_station_id) 
df$to_station_id <- as.character(df$to_station_id)

# formating datetime to d/m/Y H:M
dataframe1$start_time <- mdy_hm(dataframe1$start_time)   #lubridate function mdy_hd
dataframe1$end_time <- mdy_hm(dataframe1$end_time)
dataframe1$tripduration <- as.integer(dataframe1$tripduration) 
dataframe1$from_station_id <- as.double(dataframe1$from_station_id) 
dataframe1$to_station_id <- as.double(dataframe1$to_station_id) 
# replacing the user type with the same name
replace(df$usertype,df$usertype == "subscriber", "member")
replace(df$usertype,df$usertype == "Customer", "causal")

#data cleaning
clean_names(dataframe1)
filtered_dataframe1 <- na.omit(dataframe1)
#saveRDS(filtered_dataframe1,"filtered_dataframe1")

# 2nd dataframe with 13 veriables
filtered_dataframe2 <- na.omit(dataframe2)

# select specific data
trimmed_data2 <- filtered_dataframe2 %>% 
  unite(difftime(filtered_dataframe2$end_time,filtered_dataframe2$start_time,units = "sec"),)

all_trips<- trimmed_data2 %>% 
  select(ride_type,usertype,week_days,tripduration) %>% 
  group_by(ride_type,usertype, week_days) %>% 
  summarise(number_of_ride = n(), average_duration_min = abs(mean(tripduration/60)))
