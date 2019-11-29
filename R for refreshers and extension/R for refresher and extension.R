#set working directory

#load libraries to use

library(tidyverse)
#or
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

#load in the dataset with read_csv()

employment <- read_csv("employment_statistics.csv")

#quick recap on tidyverse and pipes
#Pipes feed into each other. Easiest interpretation is to read them as 'then'
#Select variable, then, filter rows, then group, then....
# It reduces the number of environment objects created

employment_clean <- employment %>% #use employment data THEN
  select(-FeatureCode) %>%  #remove FeaureCode THEN
  filter(Measurement == 'Count') %>% #keep only the number ofpeople THEN
  select(-Units) #remove unnecessary column



######## Column modulation 

#we want to have the quaters and the years separate

employment_colummns <- employment_clean %>% 
  
  
  
  

#get summary statistics for male and female

employments_summary <- employment_clean %>% 
  group_by(Gender, `Working Pattern`) %>% 
  summarise(count = sum(Value))