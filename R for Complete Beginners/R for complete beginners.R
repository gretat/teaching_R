# Set the working directory
# R needs to know whre to look for data
# You should keep scripts and data in the same folder
# Option 1: Menu Bar >> Session >> Set Working Directory >> To Source file location 
# Option 2: setwd("foder directory")


##### Loading packages ####

#We do this only once per script. Best done at the beginning. 

#library(NAME_OF_PACKAGE)
#today weare going to use 5 different packages

library(tidyverse) # this is a wraper that loads additional packages
#alternatively you can load them separately

library(tidyr) # designed to reshape data
library(dplyr) # helps with maipultng data
library(ggplot2) #helps us create beautiful plots
library(readr) # helps read variety of formats. Mostly used for csv and tsv formats
library(readxl) # helps us read in excel files


####loading files ####

#csv files: best functuonlity (package readr)- read_csv('filename.csv',...)

exports <- read_csv('exports_by_industry.csv')

# excel files: (package readxl) read_excel('filename.xlsx', sheet = 2,...)
#specify the sheet you want or it will take the 1st sheet by default

exports_2sheet <- read_excel('exports_by_industry_2sheets.xlsx', sheet = 'Sheet1')


##### Let's jump into tidyverse ####


#################### Select variables (i.e. columns) 

#select(data, variable_names_unquoted) 
  #Use backticks `` for variable names of more than one word

exports_1 <- select(exports, DateCode, Value, `Industry Sector (SIC 07)`, 
                    `Export Destination`)

View(exports_1)

#negative select: get rid of variables with the - sign. Keep in mind the c(). 

exports_1_minus <- select(exports, -c(FeatureCode, Measurement, Units))

View(exports_1_minus)

################## Keep or remove observations (rows): similar to an ifelse statemnt


#filter(data, variable_to_check = some_condition) filters IN (keeps what matches the condition)

#we want only 2017
exports1_2017 <- filter(exports_1, DateCode == 2017)

View(exports1_2017)

#we do not want the total rows, want to keep all other years

exports1_2017_nototal <- filter(exports1_2017, `Industry Sector (SIC 07)` != 'Total')

View(exports1_2017_nototal)

# some logical operators in R
# 
#  equal ==
#  not equal !=
#  larger than >
#  smaller than <



################## Creating new variables 

#mutate(data, new_variable_name = what_is_in_the_variable, new_variable_name2 = ...)

#Want to see the value in billions

exports1_billions <- mutate(exports1_2017_nototal, value_billions = Value/1000)

View(exports1_billions)

################# Grouping  and Summarising

#group_by(data, variable1, variable2,...) # choose how you want to form your groups


exports1_billions_destinations <- group_by(exports1_billions, `Export Destination`)

View(exports1_billions_destinations) # notice no actual change

glimpse(exports1_billions_destinations) # notice the groups in the summary


#summarise(data, summary_variable_name = some_function())

#we want to know how much we export per each destination, ignoring exporting industry

exports_destinations_summarised <- summarise(exports1_billions_destinations, 
                                          total_exp_destination = sum(value_billions))

# You can use any calculation but here are some useful functions
# min()
# max()
# mean()
# sd()
# median()
# n() - counts the members of the groups, if not grouped counts all instances



View(exports_destinations_summarised)



################# Reshape data 

# Sometimes the  operations we need to do require a different format, 
# for that we use gather() and spread()

# Our data is in long format, but we need it in a wide format. For example we want 
# each export destination to be a separate column

#spread(data, key = variable_with_names, value = variabe_with_values, ...)

exports1_wide <- spread(exports1_2017_nototal, key = `Export Destination`, value = Value)
# we used the data with only one value varible. Use the dataset with the value both in millions
# and billions to see what happens

View(exports1_wide)


# gather works in the reverse way. It makes your data into long format.

# gather(data, key = "name_for_var_with_labels", value = 'name_for_var_with_values', c(variable1, variable2,...))

exports1_long <- gather(exports1_wide, key = 'export_destination', value = 'Value', c(3:7))#or c(EU:Total) or list all


View(exports1_long)




############################# Let's Plot ###############################

# Here we use the ggplot package due to its ease of layering and variety

# Layer1: what data are we going to use
# Layer2: how do we want it to look

# ggplot(data, aes(x,y, ...)) + geom_something(...)

# histogram style

ggplot(exports1_long, aes(x = export_destination, y = Value)) + geom_col()

# adding extra layers

ggplot(exports1_long, aes(x = `Industry Sector (SIC 07)`, y = Value)) + 
  geom_col() + #each industry gets a column
  geom_point(aes(group = export_destination, colour = export_destination)) + #each destination is a point
  coord_flip() #flip to one side


#lets plot a line graph

#select only the total values, not individual industries WHY?
exports_line <- filter(exports_1, `Industry Sector (SIC 07)` == 'Total')


ggplot(exports_line, aes(DateCode, Value)) +
  geom_line(aes(color = `Export Destination`))



#Time to play

# TASK: 
# Create a summary table for one industry for International and Rest of UK only. 
# Then plot a line graph for all years


#Start with the original dataset exports and see how 
# far can you get along








#### Extra  - pipes

#Pipes feed into each other. Easiest interpretation is to read them as 'then'

#Select variable, then, filter rows, then group, then....

# It reduces the number of environment objects created


exports_pipes <- exports %>% #use exports data THEN
  
  select(DateCode, Value, `Industry Sector (SIC 07)`, 
         `Export Destination`) %>%  #select  DateCode, Value, `Industry Sector (SIC 07)` & `Export Destination` THEN
  
  filter(DateCode == 2017) %>% #keep only  year 2017 THEN
  
  mutate(value_billions = Value/1000) #rcreate a new column, which gives the 


####extra 2 joins: 

#create a lookup table just for ilustration purposes

#func_join(table1, table2, by = common_variables)
#inner_join -  keep only rows that match
#left_join - keep all from table 1, only matching rows in table2
# right_join - keep all from table 2, only matching rows in table 1
# anti_join - keep only rows in table 1 that do not have a match in table 2
# full_join - keep everything 


to_join <- tibble(SIC = unique(exports$`Industry Sector (SIC 07)`), industries = rep(c('A', 'B' , 'C', 'D', 'E'), times = c(4,3,11,2,9)))

#join using 
joined_tables <- inner_join(exports_pipes, to_join, by = c('Industry Sector (SIC 07)' = 'SIC'))
