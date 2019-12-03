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
  select(-c(Units, Measurement)) #remove unnecessary column



######## Column modulation 

#we want to have the quaters and the years separate
# separate(data, Variable, into= c('ne_varA' , 'new_varB' ), sep = 'separator')

employment_separate <- employment_clean %>% 
  separate(DateCode, c('Year', 'Quarter'), sep="-")

View(employment_separate)

#unite works is the exact opposite way  
# unite(data, new_variable, c(variable1, variable2), sep = 'separator')  


employment_united <- employment_separate %>% 
  unite(DateCode, c('Year', 'Quarter'), sep="-")


####### Finding patterns with grepl and grep

genders <- c('male', 'female', 'non-binary', 'transgender', 'other')

# we want to find at which positions we can find the word male - i.e. male and female

ind<- grep('male', genders, fixed = T)

gender_cat <- genders[ind]

#or we can check whethere the two words are in with grepl

grepl('male', genders, fixed= T)

genders[grepl('male', genders, fixed= T)] #gives you male and female immediately

#these two functions are sometimes very useeful when trying to filter instances 



######## Advanced Variable creation with mutate

#create a new variable based on a conditional statement 
# ifelse(conditon, case_if_true, case_if_false)

employments_mutate <- employment_separate %>% 
  mutate(numbered_gender = ifelse(Gender == 'Male', 1, ifelse(Gender == 'Female', 2, 0)))

# or when we wnat to have several different conditions and we do not want to write many ifelse's
employment_mutate_cases <- employments_mutate %>% 
  mutate(Carrots_eaten = case_when(married_percentage <= 60 ~ 40, married_percentage > 60 & Value<= 4000 ~ 3450, Value > 4000 ~ 6000))  # I have created a random carrots eating variable for illustration purposes
  


# or we can change more than one variable based on the same rule
employment_mutate_at <- employment_mutate_cases %>% 
  mutate_at(vars('Carrots_eaten', 'Value'),  sqrt ) 






# Advanced filtering  


#filter based on more than one matching condition using %in%

employment_females <- employment_mutate_cases %>% 
  filter(Gender %in% c('Male', 'Female')) # or put ! before Gender to get everything that is not Male or Female
#or filter(tolower(Gender) %in% gender_cat)

# we can specify which columns we want to filter at if we want to perform the same
# operation on more than one occasion

employment_filter_at <- employment_mutate_cases %>% 
  filter_at(vars('Carrots_eaten', 'Value'),  ~ . >2000 ) 
#We are keeping only those instances that are larger than 2000 both in carrots_eaten and Value


#Filter_if

# we can also do conditional filtering and mutations
# say you want to  only look at the ones 
employment_filter_if <- employment_mutate_cases %>% 
  filter_if(is.numeric,   all_vars( . < 6000)) #we only want to keep those instances that have a value smaller than 6000 in all numeric columns


#Filter_all
#sometimes you want to filter all of or variables at the same time 
employment_filter_all <- employment_mutate_cases %>% 
  mutate(Royalty = case_when(Gender == 'Female' ~ 'Q1', Gender == 'Male' ~ 'K1', Gender == 'All' ~ 'R1')) %>% 
  filter_all(any_vars(grepl(pattern ='Q1', ., fixed = TRUE))) #any row that is either first quarter or Q1 in royalty




############Joining tables ####


#we can join tables with the same column titles 

employment1<- employment[1:400,]
employment2 <- employment[600 :1200,]

employment_rbind <- rbind(employment1, employment2)



# or we can join based on same number of rows

emplyment_cbind <- cbind(employment1, employment2[1:400,])



### more useful is if we want to join tables we already have

# inner_join 



#get summary statistics for male and female

employments_summary <- employment_clean %>% 
  group_by(Gender, `Working Pattern`) %>% 
  summarise(count = sum(Value))