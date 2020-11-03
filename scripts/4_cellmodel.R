# Purpose: store model2 here to save time 
# Author: Dingyi Yu
# Data: 31 October 2020
# Contact: eddie.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# Need to have prepared survey_data.csv and save it to the input folder 


library(tidyverse)

wd <- getwd()
wd <- paste(wd,"/outputs/data/survey_data.csv",sep='')
wd
survey_data <- read_csv(wd)
# convert all categorical variables of survey data to factors 
survey_factors<-c("vote_2020","agegroup","gender","state","household_income" ,"race","employment_status","edu" ,"cell")
survey_data[survey_factors] <- lapply(survey_data[survey_factors], factor) 

# set state as the cell variable and varies the coefficient of the household income as states change,
# build the multilevel regression model
model2 <- glmer(vote_2020~(1+agegroup|cell)+gender+employment_status+race+edu,
                data = survey_data, 
                family=binomial)
summary(model2)

# save the model1
wd <- getwd()
wd <- paste(wd,"/outputs/model/model2.rds",sep='')

saveRDS(model2,file=wd)