
# Purpose: we clean and prepare the UCLA survey data so that we can do the post-stratification
# Author: Dingyi Yu
# Data: 31 October 2020
# Contact: eddie.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# Need to have download the UCLA survey data ns20200625.dta and save it to the input folder 


library(tidyverse)


#get current directory and paste the path of survey file 
wd <- getwd()
wd <- paste(wd,"/inputs/ns20200625.dta",sep='')

# load raw survey data
raw_survey <- read_dta(wd)
raw_survey <- labelled::to_factor(raw_survey)


# selected useful variable from survey data
# 6479 observations
raw_survey <- 
  raw_survey %>% 
  select(vote_2020,
         vote_intention,     
         age,
         gender,
         state,
         employment,
         education,
         household_income,
         race_ethnicity)

# select only people who intend to vote or probably go to vote and preferred candidate is either Trump or Biden
survey_data <- raw_survey %>% filter((vote_intention =="Yes, I will vote" | vote_intention =="Not sure") & vote_2020 != "I would not vote" & (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))

# drop NA
# reduced to 4607 observations
survey_data<-na.omit(survey_data)



# Convert the numeric age variables to categorical agegroup variable in both datasets

survey_data<-survey_data %>% mutate(agegroup = case_when(age <= 25 ~ '25 and younger',
                                                         age >25  & age <= 35 ~ '25 to 35',
                                                         age >35  & age <= 45 ~ '35 to 45',
                                                         age >45  & age <= 55 ~ '45 to 55',
                                                         age >55  & age <= 65 ~ '55 to 65',
                                                         age >=65 ~ 'above 65'
)) 


# Convert the race_ethnicity variable to a new race variable in survey data
# To match the format of race variable in census data

survey_data<-survey_data %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity == "Asian (Asian Indian)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Asian (Vietnamese)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Asian (Other)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Asian (Korean)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Asian (Filipino)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Samoan)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Guamanian)" ~ "Other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Other)" ~ "Other asian or pacific islander",
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
  )) 





# Convert the employment variable to a employment_status variable in survey data
# To match the format of employment_status variable in census data

survey_data<-survey_data %>% 
  mutate(employment_status = case_when(employment=="Full-time employed"~"Employed",
                                       employment=="Unemployed or temporarily on layoff"~"Unemployed",
                                       employment=="Retired"~"Not in labor force",
                                       employment=="Student"~"Not in labor force",
                                       employment=="Homemaker"~"Unemployed",
                                       employment=="Part-time employed"~"Employed",
                                       employment=="Permanently disabled"~"Not in labor force",
                                       employment=="Self-employed"~"Employed",
                                       employment=="Other:"~"Not in labor force"
  )) 



# Added new edu variable inside survey data to standardize the format of education level 

survey_data<-survey_data %>% 
  mutate(edu = case_when(education=="High school graduate"~"High school graduate",
                         education=="Associate Degree"~"Associate Degree",
                         education=="College Degree (such as B.A., B.S.)"~"College Degree (such as B.A., B.S.)",
                         education=="Middle School - Grades 4 - 8"~"Middle School - Grades 4 - 8",
                         education=="3rd Grade or less"~"3rd Grade or less",
                         education=="Completed some high school"~"Completed some high school",
                         education=="Masters degree"~"Masters degree",
                         education=="Completed some college, but no degree"~"Completed some college, but no degree",
                         education=="Completed some graduate, but no degree"~"College Degree (such as B.A., B.S.)",
                         education=="Other post high school vocational training"~"High school graduate",
                         education=="Doctorate degree"~"Doctorate degree"
  )) 


# update survey_data to the converted version of each variable
# note we conserve vote_2020 for further analysis

survey_data <- 
  survey_data %>% 
  select(vote_2020,
         age,     
         agegroup,
         gender,
         state,
         race,
         edu,
         household_income,
         employment_status)

# combine gender and race together to create cell variable
survey_data$cell<-paste(survey_data$gender,survey_data$race)

# convert all categorical variables of survey data to factors 
survey_factors<-c("vote_2020","agegroup","gender","state","household_income" ,"race","employment_status","edu" ,"cell")
survey_data[survey_factors] <- lapply(survey_data[survey_factors], factor) 

# predicting the probability for voting Biden using Trump as reference
survey_data$vote_2020 <- relevel(survey_data$vote_2020, ref = "Donald Trump")
survey_data[survey_factors] <- lapply(survey_data[survey_factors], factor) 

# save the cleaned survey data
wd <- getwd()
wd <- paste(wd,"/outputs/data/survey_data.csv",sep='')
write_csv(survey_data, wd)




