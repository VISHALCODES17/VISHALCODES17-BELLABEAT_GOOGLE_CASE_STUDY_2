# Installing the required packages

install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")


install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("kableExtra")
install.packages("knitr")
install.packages("rmdformats")

library("janitor")
library("lubridate")
library("skimr")
library("kableExtra")
library("knitr")
library("rmdformats")


library(tidyverse)
library(tidyr)
library("ggplot2")
library(dplyr)

# PREPARE PHASE: Loading the dataset files into Rstudio workspace and ensuring credibility and organization of this data.

dailyActivity_overview<-read.csv("dailyActivity_merged.csv")
dailyCalories_overview<-read.csv("dailyCalories_merged.csv")
dailyIntensities_overview<-read.csv("dailyIntensities_merged.csv")
dailySteps_overview<-read.csv("dailySteps_merged.csv")
dailySleep_overview<-read.csv("sleepDay_merged.csv" , header =TRUE)
weightLog_overview<-read.csv("weightLogInfo_merged.csv")

# PROCESS PHASE: The below lines search for distinct records in a file by "ID" column.

n_distinct(dailyActivity_overview$Id)
n_distinct(dailyCalories_overview$Id)
n_distinct(dailyIntensities_overview$Id)
n_distinct(dailySteps_overview$Id)
n_distinct(dailySleep_overview$Id)
n_distinct(weightLog_overview$Id)

# Below are the data cleaning steps, first clean_names function is used which consists of names which are unique having only "_" character,numbers and letters.

dailyActivity_overview<-clean_names(dailyActivity_overview)
dailyCalories_overview<-clean_names(dailyCalories_overview)
dailyIntensities_overview<-clean_names(dailyIntensities_overview)
dailySteps_overview<-clean_names(dailySteps_overview)
dailySleep_overview<-clean_names(dailySleep_overview)
weightLog_overview<-clean_names(weightLog_overview)

# In ths data clening step we look for "NA" values in each data frame.

sum(is.na(dailyActivity_overview))
sum(is.na(dailyCalories_overview))
sum(is.na(dailyIntensities_overview))
sum(is.na(dailySteps_overview))
sum(is.na(dailySleep_overview))
sum(is.na(weightLog_overview))

# As date in every data file is in wrong format , it needs to be corrected with proper syntax in each file ,this is also data cleaning.

dailyActivity_overview$activity_date<-mdy(dailyActivity_overview$activity_date)
dailyCalories_overview$activity_day<-mdy(dailyCalories_overview$activity_day)
dailyIntensities_overview$activity_day<-mdy(dailyIntensities_overview$activity_day)
dailySteps_overview$activity_day<-mdy(dailySteps_overview$activity_day)

# Separating the date and time into different columns as part of data cleaning.

dailySleep_overview<- dailySleep_overview %>% 
  separate(sleep_day, into = c("sleep_date", "sleep_time"), sep = c(" "))

dailySleep_overview$sleep_date<-mdy(dailySleep_overview$sleep_date)

# Checking for duplicate records to ensure data redundancy is not there as part of data cleaning.

sum(duplicated(dailyActivity_overview))
sum(duplicated(dailyCalories_overview))
sum(duplicated(dailyIntensities_overview))
sum(duplicated(dailySteps_overview))
sum(duplicated(dailySleep_overview))
sum(duplicated(weightLog_overview))

# Found the dailySleep_overview data frame has 3 duplicate records. Using unique() function to remove redundant data.

dailySleep_overview <- dailySleep_overview %>% unique()

# ANALYZE PHASE:  Using skimr package library functio to get detailed summary of each data types and columns in a data frame separately.

skim_without_charts(dailyActivity_overview)
skim_without_charts(dailyCalories_overview)
skim_without_charts(dailyIntensities_overview)
skim_without_charts(dailySteps_overview)
skim_without_charts(dailySleep_overview)
skim_without_charts(weightLog_overview)

# Creating new column into exsting data frame for analyzing weight of individuals falling into which category as regulated by WHO. 
# Using mutate function to create new column.

weightLog_overview<-weightLog_overview %>%
  mutate(weight_category=case_when(
    .$bmi < 18.5 ~ "UNDERWEIGHT",
    .$bmi >=18.5 & .$bmi < 24.9 ~ "NORMAL",
    .$bmi >=25   & .$bmi < 29.9 ~ "OVERWEIGHT",
    .$bmi > 30 ~ "OBESITY"
  ))

# Creating another data frame g1 having all data of dailyActivity_overview data frame along with one extra column created by mutate function. 
# Here it will be used to mark people into categories of most to least active individuals by counting totalSteps walked which is tracked by bellabeta tracker device.

g1<-dailyActivity_overview %>%
  mutate(category_splitby_totalSteps=case_when(
    .$total_steps >=13000 ~ "VERY ACTIVE",
    .$total_steps>=10000 & .$total_steps<13000 ~ "ACTIVE",
    .$total_steps>=8000  & .$total_steps< 10000 ~ "FAIRLY ACTIVE",
    .$total_steps>=5000 & .$total_steps<8000 ~ "LEAST ACTIVE",
    .$total_steps<5000 ~ "SEDENTARY"))

# Creating another data frame g2 having all data of dailyActivity_overview data frame along with one extra column created by mutate function. 
# Here it will be used to mark people into categories of most to least active individuals by calories they burnt which is tracked by bellabeta tracker device.

g2<-dailyActivity_overview %>%
  mutate(category_splitby_totalCaloriesburnt=case_when(
    .$calories >= 0  & .$calories <=1800 ~ "SEDENTARY",
    .$calories >1800 & .$calories <=2500 ~ "LEAST ACTIVE",
    .$calories >2500 & .$calories <=3300 ~ "LIGHTLY ACTIVE",
    .$calories >3300 & .$calories <= 4000~ "FAIRLY ACTIVE",
    .$calories>4000 ~ "VERY ACTIVE"))

# Just to ensure that the distance measured by tracker is same as total distance travelled so that it can be verified that the tracker devices are digitally error free. 

dailyActivity_overview <- dailyActivity_overview %>%
  mutate(unmatch=case_when(
    .$total_distance != .$tracker_distance ~ "N",
    .$total_distance==  .$tracker_distance ~ "Y"
  ))

# Adding new column in dailySleep dta frame to analyze whether people are having any sleeping disorders. The below code matches the total time people are awake after getting into bed for sleeping.
# The more they are wake, the longer they remain awake the severe may be the disorder. As per WHO people should fall asleep after 20 mins, of gettting into bed.
# They may be awake because of chronic insomnia, hypertension,or other medical disorers needing doctor's intervention.

dailySleep_overview<- dailySleep_overview %>%
  mutate(freetime_inbed=case_when(
    .$total_time_in_bed - .$total_minutes_asleep <=20 ~ "NORMAL SLEEP",
    .$total_time_in_bed - .$total_minutes_asleep >20 & .$total_time_in_bed - .$total_minutes_asleep <=40 ~ "WORRIED",
    .$total_time_in_bed - .$total_minutes_asleep >40 & .$total_time_in_bed - .$total_minutes_asleep <=100 ~ "TENSION",
    .$total_time_in_bed - .$total_minutes_asleep >100 ~ "NEED DOCTOR"
  ))

# SHARE PHASE: TOTAL STEPS Vs. ACTIVITY INSIGHT 

# Here people are categorized based on their activity level i.e. total number of steps travelled by them. They are classified as ACTIVE ,FIRLY ACTIVE ,SEDENTARY etc. based on it.

ggplot(data=g1)+geom_bar(mapping=aes(x=category_splitby_totalSteps, fill=category_splitby_totalSteps))+labs(title=" Total Steps Vs. Activity Insight")

# Here people are categorized based on their activity level i.e. total calories burnt by them. They are classified as ACTIVE ,FIRLY ACTIVE ,SEDENTARY etc. based on it.

ggplot(data=g2)+geom_bar(mapping=aes(x=category_splitby_totalCaloriesburnt, fill=category_splitby_totalCaloriesburnt))+labs(Title="Total distance Vs. Calories Insight")

# Here people are categorized based on their current BMI st up by WHO. They are classified as Overweight, obesity etc. based on it. 

ggplot(data=weightLog_overview)+geom_bar(mapping=aes(x=weight_category, fill=weight_category))+labs(Title="Weight Category Of Customers")

# Here we want to match that the distance measured by tracker is same as total distance travelled so that it can be verified that the tracker devices are digitally error free.
# It is important as if teh customers are using some non bellabeat devices showing accurate distances then bellabeat reputation may go down, so by matching they will try to ensure
# that devices are error free.

ggplot(data=dailyActivity_overview)+geom_bar(mapping=aes(x=unmatch, fill=unmatch))+labs(Title="Total distance Vs. Tracker distance")

# The more they are awake,the severe may be the disorder. As per WHO people should fall asleep after 20 mins, of gettting into bed.
# They may be awake because of chronic insomnia, hypertension,or other medical disorers needing doctor's intervention. This insght will let them know how many people need medical assistance/

ggplot(data=dailySleep_overview)+geom_bar(mapping=aes(x=freetime_inbed, fill=freetime_inbed))+ labs(Title="Sleep Disorder Signs")

# Now i want to combine the columns from different data frames into single dat frame. So, i will merge the data frames to create a new one having all columns from both tables.
# This is useful as data is in separate data files and i need to plot them on same graph. Plotting total distnace from g1 data frame and BMI from weightlog data frame after combining
#them in g3.

g3<-merge(g1,weightLog_overview,by='id')

ggplot(data=g3)+geom_line(mapping = aes(x=total_distance,y=bmi))+labs(Title="Total distance Vs. BMI")

# Here also cross data frame columns are needed for visualization of total minutes a person is asleep and the total time they remin in bed so that it can be noted what is the relationship
#between these two variables and what could be its possible factors.

g4<-merge(g3,dailySleep_overview ,by='id')

ggplot(data=g4)+geom_line(mapping = aes(x=total_minutes_asleep,y=total_time_in_bed))+labs(Title="Time in Bed Vs. Actual Sleep")

# ACT PHASE:

# Bella-beat can advertise its customers to buy their tracker devices to maintain healthy records.
# Bella-beat company should stop using accelerometers( IF USING) leading to unpopularity and false results among customers for its products as the distances measured by their devices do not atch with total distnaces for some customers.
# Enlighten people about sleep disorders and advertising about its products to customers to keep track of such illness.
# Enlighten its technical team about the devices not being digitally accurate to be corrected as soon as possible.


