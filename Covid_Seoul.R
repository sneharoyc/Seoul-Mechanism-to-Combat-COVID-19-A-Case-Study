rm(list = ls())

#############################LIBRARIES###########################################

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(randomForest)
library(moments)

############################READ DATASET############################################
# Dataset 1
covid19<-read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
# Dataset 2
recovered<-read_csv("recovered.csv")

#############################Data Cleaning############################################

# Dataset 1

covid<-covid19%>%
  select(-c("iso_code","continent","reproduction_rate","new_vaccinations_smoothed_per_million","people_fully_vaccinated_per_hundred","people_vaccinated_per_hundred","total_cases","total_deaths","total_cases_per_million","people_fully_vaccinated","population_density","new_tests_smoothed_per_thousand","new_tests_smoothed","total_deaths_per_million","new_cases_per_million","new_cases_smoothed_per_million","life_expectancy","human_development_index","hospital_beds_per_thousand","gdp_per_capita","aged_70_older","aged_65_older","total_vaccinations","hosp_patients_per_million","total_tests_per_thousand","new_people_vaccinated_smoothed_per_hundred","people_vaccinated","new_vaccinations","new_vaccinations_smoothed","total_vaccinations_per_hundred","new_cases_smoothed","new_deaths_smoothed","new_people_vaccinated_smoothed","male_smokers","total_boosters_per_hundred","cardiovasc_death_rate","total_boosters","icu_patients_per_million","icu_patients","hosp_patients","weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions","weekly_hosp_admissions_per_million","total_tests","tests_units","positive_rate","median_age","population","extreme_poverty","handwashing_facilities","excess_mortality_cumulative_absolute","excess_mortality_cumulative","excess_mortality","excess_mortality_cumulative_per_million","new_tests","new_tests_per_thousand","female_smokers","diabetes_prevalence","new_deaths_smoothed_per_million","new_deaths_per_million"))

# Dataset 2

r=recovered%>%
           pivot_longer(-c('Province/State', 'Country/Region', Lat , Long),
                             names_to = "date" ,
                             values_to = "recovered")%>%
         select(-c(Lat,Long))%>%
         rename(country='Country/Region')%>%
         mutate(date=mdy(date))%>%
         group_by(country, date)%>%
         summarise(recovered=sum(recovered))%>%
         ungroup()%>%filter(country == "Korea, South")


recovered_cases<-r%>%
  select(-c("country"))

#################################### Data for South Korea ####################################

SK_data<-filter(covid,location=="South Korea") 

##################################### Data for South Korea (22 Jan,2020- 31 Dec,2021)##########

SK1<-SK_data[-c(711:724),]
recovered_cases<-recovered_cases[-c(711:724),]

#################################### Merge data sets #########################################

SK<-merge(SK1,recovered_cases)
#View(SK)

##################################### Pandemic Overview of South Korea####################################

# Descriptive Statistics

S1<-na.omit(SK$new_cases)
S2<-na.omit(SK$new_deaths)
summary(S1)
summary(S2)

# Standard Deviation
sd1<-sd(S1)
sd2<-sd(S2)

# Coefficient of Variation
CV1<-(sd1/896)*100
CV2<-(sd2/8.26)*100
CV1
CV2

################################Exploratory Data Analysis#################################

#South Korea COVID-19 pandemic trends from 21 January 2020 to 31 December 2021.

ggplot() +
     geom_line(data=SK, aes(x=date, y=new_cases,color="new_cases"),na.rm=T)+
      geom_line(data=SK, aes(x=date, y=new_deaths,color="new_deaths"),na.rm=T)+
      scale_x_date(date_breaks="8 weeks",date_labels="%d %b %y")+labs(x="Date",y="New cases and New deaths(log10)",title="Evolution of Covid-19 cases and deaths",caption="Source:Our World In Data")+ scale_y_continuous(trans="log10")+
geom_vline(xintercept=SK$date[33],linetype=3,color="black", size=2)+ggeasy::easy_center_title()+
  geom_text(aes(x=SK$date[110], label="crisis alert to the highest level", y=7000), colour="black", angle=0.5, vjust = 1, text=element_text(size=12))
 

#Testing Strategy of South Korea over the time period 28 January 2020 to 29 October2021

ggplot()+
  geom_line(data=SK,mapping=aes(x=date,y=tests_per_case),na.rm=T)+labs(x="Date",y="Tests per confirmed case",title="Testing Strategy:South Korea")+
scale_x_date(date_breaks="8 weeks",date_labels="%d %b %y")+
 geom_vline(xintercept=SK$date[31],linetype=3,color="red", size=2)+ggeasy::easy_center_title()


#Change in the Stringency measures over the time period 21 January 2020 to 31 December 2021
 
ggplot()+
  geom_line(data=SK,mapping=aes(x=date,y=stringency_index),na.rm=T)+labs(x="Date",y="Stringency index",title="COVID-19 Stringency index",caption="Source:Our World In Data")+
scale_x_date(date_breaks="8 weeks",date_labels="%d %b %y")


#South Korea COVID-19 recovery trends from 21 January 2020 to 31 December 2021

sum(SK$recovered)
ggplot()+
geom_line(data=SK, aes(x=date, y=recovered))+
scale_x_date(date_breaks="8 weeks",date_labels="%d %b %y")+
ggeasy::easy_center_title()+
labs(x="Date",y="Recovered cases",title="Recovered Covid-19 cases: South Korea",caption="Source:John Hopkins Coronavirus Resource Centre")
 


################## Modelling the impact of government responses and mortality cases on new COVID19 cases###############


SK<-SK%>%
  select(-c("recovered"))
data1=na.omit(SK)

##Dependent variable:New cases of Covid-19
##Independent variable1:Stringency Index
##Independent variable2:Tests per case 

data<-data1%>%
  select(c("new_cases","tests_per_case","stringency_index"))

################Multiple Regression#####################################

#################Validity of model assumption############################

# Normality assumption 

#original new cases
s=data$new_cases 

#transformation new cases
t=sqrt(s)

#Histogram showing the distribution of the dependent variable

par(mfrow=c(1,2))
hist(s,main="Histogram of new cases",xlab="New cases", ylab="Frequency")
hist(t,main="Histogram of new cases(transformed)",xlab="New cases", ylab="Frequency")

# Skewness
skewness(s)
skewness(t)

#Regression Models
reg1<-lm(s~data$tests_per_case+data$stringency_index)
summary(reg1)

reg2<-lm(t~data$tests_per_case+data$stringency_index)
summary(reg2)

#Diagonistic plots

# Linearity, Homoscedasticity and no outlier assumption
plot(reg2,1)

# Normality of Residuals
plot(reg2,2)


# Multicollinearity

cormat=round(cor(data),2)
melted_cormat=melt(cormat)
ggplot(melted_cormat,aes(x=Var1,y=Var2,fill=value))+
geom_tile()+labs(title="CORRELATION HEATMAP")+ggeasy::easy_center_title()


##########################RANDOM FOREST#####################################

SK1<-SK%>%select("new_cases","stringency_index", "tests_per_case")


# Create random forest for regression
reg <- randomForest(SK1$new_cases~ ., data = SK1 ,mtry =2,
                         importance = TRUE, na.action = na.omit)
# Print regression model
print(reg)







