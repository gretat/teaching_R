#set working directory

#load libraries to use

library(tidyverse)
#or
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

#load in the dataset with read_csv()

employment <- read_csv("employment_statistics.csv") #scottish government employment data modified. Does not represent real data

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


####### Finding patterns with grepl and grep ######

genders <- c('male', 'female', 'non-binary', 'transgender', 'other')

# we want to find at which positions we can find the pattern male - i.e. male and female

ind<- grep('male', genders, fixed = T)

gender_cat <- genders[ind]

#or we can check whethere the two words are in with grepl

grepl('male', genders, fixed= T)

genders[grepl('male', genders, fixed= T)] #gives you male and female immediately

#these two functions are sometimes very useeful when trying to filter instances 



######## Advanced Variable creation with mutate #######

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






############## Advanced filtering  #######


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




############Joining tables #######


#we can join tables with the same column titles 

employment1<- employment[1:400,]
employment2 <- employment[600 :1200,]

employment_rbind <- rbind(employment1, employment2)



# or we can join based on same number of rows

emplyment_cbind <- cbind(employment1, employment2[1:400,])



### more useful is if we want to join tables we already have
####extra 2 joins: 

#create a lookup table just for ilustration purposes

#func_join(table1, table2, by = common_variables)
#inner_join -  keep only rows that match
#left_join - keep all from table 1, only matching rows in table2
# right_join - keep all from table 2, only matching rows in table 1
# anti_join - keep only rows in table 1 that do not have a match in table 2
# full_join - keep everything 


#we can

to_join <- tibble(married_perc = sort(unique(employment_mutate_cases$married_percentage)), rate_average = rep(c('bellow', 'above'), each = 2))

#join using 
joined_tables <- inner_join(employment_mutate_cases, to_join, by = c('married_percentage' = 'married_perc'))


########## Plotting opportunities #######


#lets make a basic plot

#get data for male and female only

employment_plot <- employment_mutate_cases %>% 
  filter(tolower(Gender) %in% gender_cat) %>% 
  group_by(Year, Gender, `Working Pattern`) %>% 
  summarise(Value = sum(Value),
            married_percentage = mean(married_percentage),
            carrots_eaten = sum(Carrots_eaten))%>% 
  ungroup() # in pipes, you need to always ungroup once you have finished with the groups


ggplot(employment_plot, aes(x = as.numeric(Year), y = Value/1000000, group = `Working Pattern`, colour = `Working Pattern`)) +
  geom_col(aes(y = carrots_eaten/100000, x = as.numeric(Year) , group = `Working Pattern`, fill = `Working Pattern`)) +
  scale_fill_manual(values = c("#0072B2", "#F69F00"))+ 
  geom_line() +  
  geom_point() +
  facet_grid(Gender~., labeller = as_labeller(c(Female = 'Women', Male = 'Men'))) +
  theme_bw() +
  labs(x = "Year 2004 - 2019", y = "Count in Millions") +
  theme(axis.title.x = element_text(size = 12, colour = 'magenta'),
        axis.title.y = element_text(size = 16, colour = 'blue'),
        legend.position = 'top')


####   Everyone’s favourite: Maps (static) ####

#Roughly following this blog post by John Mackintosh: https://www.johnmackintosh.com/2017-09-01-easy-maps-with-tmap/
library(tmap)
library(tmaptools)

# import the data bellow, or use the scot_data.RData file
scot <- read_shape("SG_SIMD_2016.shp", as.sf = T) #data: https://www2.gov.scot/Topics/Statistics/SIMD

highland <- (scot[scot$LAName=="Highland",])


#lets create a map that colours based on the Health column

#Similar to ggplot tmap works in layers

education <- tm_shape(highland) + # Layer 1. what is our shape
  tm_fill(col = "EduAttend", # Layer2 : fill or colour based on the Attending Education column
          palette = 'div', # choose a palette
        #  palette = 'seq',
          title = "Highland Council Area by School Pupil attendance") +# title of our leged
  tm_layout(legend.width = 1)
education 


#lets get Employment rank & dots for the crimeRank for Aberdeen and Glasgow

multipl <- tm_shape(scot[scot$LAName==c("Aberdeen City", "Glasgow City"),]) + #select only Glasgow & Aberdeen
  tm_fill(col  = "EmpRank",
              title = 'Employment Rank',
          palette = 'div') +
  tm_symbols(col = 'blue', 
             size = 'CrimeRank', 
             scale = .6,
             title.size = 'Crime Rank') +
  tm_facets(by = 'LAName', nrow = 2)

multipl


#lets plot small multiple maps. facetting option 2
multipl_v2<- tm_shape(highland) +
  tm_fill(col = c("EmpRank","HlthRank","EduRank",
                  "GAccRank","CrimeRank","Rank"),
          palette = 'div',
          title=c("Employment Rank","Health Rank","Education Rank",
                  "General Access Rank","Crime Rank", "Overall Rank"))
multipl_v2



