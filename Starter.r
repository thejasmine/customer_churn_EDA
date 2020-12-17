library(dplyr)
library(ggplot2)

#read the data
telco.df <- read.csv("Telco-Customer-Churn.csv", stringsAsFactors = TRUE)


#understand the data structure
#View(telco.df)
summary(telco.df)
glimpse(telco.df)
str(telco.df)

#check if data contain null -> FALSE
is.null(telco.df)

#data cleaning and preprocessing
#convert SeniorCitizen column from int to facter
telco.df$SeniorCitizen <- as.factor(ifelse(telco.df$SeniorCitizen==1, 'Yes', 'No'))
telco.df$SeniorCitizen
