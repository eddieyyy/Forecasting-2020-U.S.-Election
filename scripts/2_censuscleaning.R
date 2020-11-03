# Purpose: we clean and prepare the ACS data so that we can do the post-stratification
# Author: Dingyi Yu
# Data: 31 October 2020
# Contact: eddie.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# Need to have download the ACS data usa_00003.dta and save it to the input folder 


library(tidyverse)


#get current directory and paste the path of survey file 
wd <- getwd()
wd <- paste(wd,"/inputs/usa_00003.dta",sep='')

# load raw census data
raw_census <- read_dta(wd)
raw_census <- labelled::to_factor(raw_census)


# selected useful variables from census data
#3214539 observations
raw_census <- 
  raw_census %>% 
  select(perwt,    
         citizen,
         age,
         sex, 
         stateicp,
         empstat,
         educd,
         hhincome,
         race
  )

# select adults and citizen only and drop not reported household income
raw_census$age<-as.numeric(raw_census$age)
census_data<-raw_census %>% filter(age>=18 & citizen!="not a citizen" & hhincome!=9999999)

# drop  NA
# reduced to 230575 observations 
census_data<-na.omit(census_data)



# Convert the numeric age variables to categorical agegroup variable in both datasets

census_data<-census_data %>% mutate(agegroup = case_when(age <= 25 ~ '25 and younger',
                                                         age >25  & age <= 35 ~ '25 to 35',
                                                         age >35  & age <= 45 ~ '35 to 45',
                                                         age >45  & age <= 55 ~ '45 to 55',
                                                         age >55  & age <= 65 ~ '55 to 65',
                                                         age >=65 ~ 'above 65'
)) 

# Covert sex variable's format to match with survey dataset
# Change the name of sex variable to gender to match with survey dataset

census_data$sex<-ifelse(census_data$sex=="male","Male","Female")
census_data<-rename(census_data,gender=sex)



# Create a new state variable to covert the full name of stateicp variable in census data to abbreviation 
# Match the format in survey data

census_data<-census_data %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="district of columbia"~"DC",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY"
  )) 


# Convert the numeric hhincome variable to a group of house_hold income
# Match the format of in survey data

census_data<-census_data %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 



# Match the format of race variable in survey data

census_data<-rename(census_data,race_ethnicity=race)
census_data<-census_data %>% 
  mutate(race = case_when(race_ethnicity=="white"~"White",
                          race_ethnicity=="black/african american/negro"~"Black, or African American",
                          race_ethnicity=="chinese"~"Chinese",
                          race_ethnicity=="japanese"~"Japanese",
                          race_ethnicity=="american indian or alaska native"~"American Indian or Alaska Native",
                          race_ethnicity=="other asian or pacific islander"~"Other asian or pacific islander",
                          race_ethnicity=="two major races"~"Other race",
                          race_ethnicity=="other race, nec"~"Other race",
                          race_ethnicity=="three or more major races"~"Other race",
  )) 


# Convert the empstat variable to a employment_status race variable in census data
# To match the format of employment_status variable in census data

census_data<-census_data %>% 
  mutate(employment_status = case_when(empstat=="employed"~"Employed",
                                       empstat=="unemployed"~"Unemployed",
                                       empstat=="not in labor force"~"Not in labor force"
  )) 





# Added new edu variable inside census data to standardize the format of education level 


census_data<-census_data %>% 
  mutate(edu = case_when(educd=="regular high school diploma"~"High school graduate",
                         educd=="associate's degree, type not specified"~"Associate Degree",
                         educd=="ged or alternative credential"~"High school graduate",
                         educd=="bachelor's degree"~"College Degree (such as B.A., B.S.)",
                         educd=="grade 7"~"Middle School - Grades 4 - 8",
                         educd=="no schooling completed"~"3rd Grade or less",
                         educd=="grade 10"~"Completed some high school",
                         educd=="master's degree"~"Masters degree",
                         educd=="12th grade, no diploma"~"Completed some high school",
                         educd=="1 or more years of college credit, no degree"~"Completed some college, but no degree",
                         educd=="some college, but less than 1 year"~"Completed some college, but no degree",
                         educd=="grade 11"~"Completed some high school",
                         educd=="grade 1"~"3rd Grade or less",
                         educd=="grade 2"~"3rd Grade or less",
                         educd=="grade 3"~"3rd Grade or less",
                         educd=="grade 4"~"Middle School - Grades 4 - 8",
                         educd=="grade 5"~"Middle School - Grades 4 - 8",
                         educd=="grade 6"~"Middle School - Grades 4 - 8",
                         educd=="grade 8"~"Middle School - Grades 4 - 8",
                         educd=="grade 9"~"Completed some high school",
                         educd=="nursery school, preschool"~"3rd Grade or less",
                         educd=="professional degree beyond a bachelor's degree"~"College Degree (such as B.A., B.S.)",
                         educd=="doctoral degree"~"Doctorate degree",
                         educd=="kindergarten"~"3rd Grade or less"
  )) 



# update census data to the converted version of each variable
# note we conserve perwt for further analysis

census_data <- 
  census_data %>% 
  select(perwt,
         age,     
         agegroup,
         gender,
         state,
         race,
         edu,
         household_income,
         employment_status)

# combine gender and race together to create cell variable
census_data$cell<-paste(census_data$gender,census_data$race)

# convert all categorical variables of census data to factors 
census_factors<-c("agegroup","gender","state","household_income" ,"race","employment_status","edu","cell")
census_data[census_factors] <- lapply(census_data[census_factors], factor)

# save the cleaned census data
wd <- getwd()
wd <- paste(wd,"/outputs/data/census_data.csv",sep='')
write_csv(census_data, wd)




