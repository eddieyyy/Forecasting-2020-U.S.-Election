# Overview

This repo contains code and data for forecasting the US 2020 presidential election. It was created by NAME. The purpose is to create a report that summarises the results of a statistical model that we built. Some data is unable to be shared publicly. We detail how to get that below. The sections of this repo are: inputs, outputs, scripts.

Inputs contain data that are unchanged from their original. We use two datasets: 

- [Survey data - The survey data is acquired firstly by getting access to the Democracy Fund + UCLA Nationscape'Full Data Set'. Then we choose to use "ns20200625" as our survey data. Required URL link is https://www.voterstudygroup.org/publication/nationscape-data-set.]

- [ACS data - The post-stratification data is acquired using American Community Surveys (ACS). Firstly, we create an account on IPUMS (https://usa.ipums.org/usa/index.shtml). For this analysis, 2018 1-year ACS has been chosen and the variables we have chosen are AGE, SEX, RACE, CITIZEN, EDUC, HHINC and STATEICP. Finally download the relevative data and transfrom it into format of dta.]

Outputs contain data that are modified from the input data, the report and supporting material.

- The first Multilevel logistic regression model with only states as "cell", which is not qualified enough.
- The second Multilevel logistic regression model with gender and race as "cells".
- Following reports based on the second model is included in the Rmarkdown file.

Scripts contain R scripts that take inputs and outputs and produce outputs. These are:

- 01_surveycleaning.R
- 02_censuscleaning.R
- 03_statemodel.R
- 04_cellmodel.R




