library(dplyr)
library(tidyverse)
library(openintro)

select(census_data, 
       where(is.factor))
census_data <- openintro::census
head(census_data)
smoking_data=openintro::smoking
head(smoking_data)
select(smoking_data, 
       gender,age,smoke)
select(smoking_data, 
       where(is.numeric))
arrange(smoking_data,-age)
smoking_data_selected <- select(smoking_data,
                              -ethnicity,-nationality)
select(smoking_data,where(is.factor))
smoking_data_selected=select(smoking_data,-ethnicity,-nationality)
rename(smoking_data,education=highest_qualification)
smoking_data <- mutate(smoking_data,sohan=amt_weekends+amt_weekdays)
filter(smoking_data,gender=="Male")
filter(smoking_data,smoke =="No"& age=="35")
filter(smoking_data,marital_status!="Divorced",nationality!="English")
filter(smoking_data,smoke=="Yes"& age<20)
filter(smoking_data,region=="London"|region=="Wales")
smoking_data%>%
          select(gross_income,amt_weekends)%>%
          group_by(gross_income)%>%
          summarise(tota_cigar = sum(amt_weekends, na.rm =T))
smoking_data%>%
          select(smoke,age)%>%
          filter(smoke=="Yes"&age<30)
smoking_data%>%
          select(highest_qualification,amt_weekdays)%>%
          rename(siksha=highest_qualification,cigs=amt_weekdays)%>%
          group_by(siksha)%>%
          summarise(total_cigss=sum(cigs,na.rm=T))
smoking_data%>%
          group_by(age)%>%
          summarise(meanof_female_males=mean(age,na.rm=T))
smoking_data%>%
          mutate(
                    age_category=case_when(
                              age>=15&age<=25 ~ "15-25",
                              age>=26&age<=40 ~ "26-40",
                              age>=40&age<=59 ~ "40-59",
                              age>59 ~ "59+"
                    )
          )
smoking_data%>%
          mutate(
                    gender_category= case_when(
                              gender=="Female"~1,
                              gender=="Male" ~0
                    )
          )
smoking_data%>%
          mutate(
                    smoke_category=case_when(
                              amt_weekends>30 ~"High",
                              amt_weekends>=11&amt_weekends<=30~"Medium",
                              amt_weekends>=0&amt_weekends<=10~"Low"
                    )
          )
                    )
                    
ggplot(data = yrbss_samp)+ #specifying data
          geom_histogram(
                    aes(x = height),
                    fill= "red"
          ) 
ggplot(data = yrbss_samp)+
          geom_density(
                    aes(x= age)
          )
ggplot(data = yrbss_samp)+
          geom_density(
                    aes(x= weight)
          )
ggplot(data = yrbss_samp)+
          geom_bar(
                    aes(x = gender,
                        fill=gender)
                   )
                    
ggplot(data = yrbss_samp)+ #specifying data
          geom_density(
                    aes(x = hispanic)
          )
ggplot(data = yrbss_samp)+
          geom_point(
                    aes(x = height,y = weight)
          )
ggplot(data = yrbss_samp)+
          geom_bar(
                    aes(x = gender
                        )
          )
          
          
