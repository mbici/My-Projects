#Loading the Data
library(readr)
Project_data <- read_csv("C:/Users/USER/Desktop/Final Year Project/Data.csv")
View(Project_data)
View(Has_Insurance)

#Cleaning the Data
library(dplyr)

#Selecting Necessary Feature Columns
model_data<- Project_data %>%
  select(County,A18,A21,A22,A24,A9,B1B2,U3,S1,B1A)%>%
  mutate_if(is.numeric,as.factor)


model_data1<- Project_data%>%
  select(A19,B3I)

View(model_data1)
colnames(model_data1)<-c("Age","Monthly_Income")
colnames(model_data)<-c("County","Sex","Education","Marital_Status",
                        "Chronic_Disease","Cluster","Emergency_Fund","Religion",
                        "Phone_Ownership","Financial_Priority")

model.data = data.frame(model_data,model_data1)
View(model.data)

Project_data$C1_42
responses_data<- Project_data %>%
  select(C1_42,C1_43)

#Selecting Response Variables
colnames(responses_data)<- c("NHIF","Other")
responses_data$NHIF[responses_data$NHIF > 1]<-0
responses_data$Other[responses_data$Other >1]<-0
response=responses_data %>%
  mutate(Has_Insurance = NHIF + Other)
response$Has_Insurance[response$Has_Insurance >= 1] <- 1
Has_Insurance <- as.factor(response$Has_Insurance)

# Drop Age respondents below 18

model_data <- data.frame(model.data,Has_Insurance)
model_data <- model_data%>%
  filter(Age > 18)

#Count missing Values
sum(is.na(model_data))
library(tidyr)
library(tidyverse)
library(dplyr)
model_data <- model_data %>%
  drop_na()
View(model_data)
write_csv(model_data,"C:/Users/USER/Documents/R Project/model_data.csv")

#Contingency Tables and Chi Square Test
county_table<-table(model_data$Has_Insurance,model_data$County)
county_table
chisq.test(county_table)


sex_table <- table(model_data$Has_Insurance,model_data$Sex)
sex_table
chisq.test(sex_table)

Edu_table <- table(model_data$Has_Insurance,model_data$Education)
Edu_table
chisq.test(Edu_table)


Marital_table <- table(model_data$Has_Insurance,model_data$Marital_Status)
Marital_table
chisq.test(Marital_table)



Chronic_table <- table(model_data$Has_Insurance,model_data$Chronic_Disease)
Chronic_table
chisq.test(Chronic_table)



Cluster_table <- table(model_data$Has_Insurance,model_data$Cluster)
Cluster_table
chisq.test(Cluster_table)


Emergency_table <- table(model_data$Has_Insurance,model_data$Emergency_Fund)
Emergency_table
chisq.test(Emergency_table)


Religon_table <- table(model_data$Has_Insurance,model_data$Religion)
Religon_table
chisq.test(Religon_table)


Phone_table <- table(model_data$Has_Insurance,model_data$Phone_Ownership)
Phone_table
chisq.test(Phone_table)


Financial_table <- table(model_data$Has_Insurance,model_data$Financial_Priority)
Financial_table
chisq.test(Financial_table)

#Exploring Numeric Variables
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data=model_data,aes(x= Has_Insurance, y = Age,fill = Has_Insurance))+
  geom_boxplot()+
  ggtitle("BoxPlot of Age vs Insurance")
ggplot(data=model_data,aes(x= Has_Insurance, y = Monthly_Income,
                           fill = Has_Insurance))+
  geom_boxplot()+
  scale_y_log10()+
  ggtitle("BoxPlot of Monthly Income vs Insurance")
#Checking for proportion of outliers

lower_outliers<-quantile(model_data$Age,p=0.25) - 1.5* IQR(model_data$Age)
upper_outliers <- quantile(model_data$Age, p =0.75) + 1.5 * IQR(model_data$Age)
sum(model_data$Age > upper_outliers)/length(model_data$Age)

lower_outliers_1<-quantile(
  model_data$Monthly_Income,p=0.25) - 1.5* IQR(model_data$Monthly_Income)
upper_outliers_1 <- quantile(
  model_data$Monthly_Income,p=0.75) + 1.5* IQR(model_data$Monthly_Income)
sum(model_data$Monthly_Income> upper_outliers_1)/length(model_data$Monthly_Income)
#Scaling the Numeric Age and Income features
#Define a scaling function
scaler <- function(x){
  (x-min(x))/(max(x)- min(x))
}


model_data_scaled <- model_data %>%
  mutate( Age = scaler(Age), Monthly_Income = scaler(Monthly_Income))
View(model_data_scaled)


#Proportion of the Response Distribution
library(dplyr)
model_data_scaled %>%
  group_by(Has_Insurance)%>%
  summarise(cnt = n()) %>%
  mutate(prop = cnt/sum(cnt))
#Plotting Response Distribution
library(ggplot2)
ggplot(data = model_data,aes(x = Has_Insurance,fill =Has_Insurance))+
  geom_histogram(stat ="count")+
  labs(title = "Count of the Response Variable Categories")
library(dplyr)
model_data<-model_data%>%
  filter(Age != 98 & Age!=99)
model_data<-model_data%>%
  filter(Monthly_Income != 98 & Monthly_Income!=99)
library(tidyverse)
write_csv(model_data, file = "C:/Users/USER/Documents/model.csv")
model_data_scaled <- model_data %>%
  mutate( Age = scaler(Age), Monthly_Income = scaler(Monthly_Income))
View(model_data_scaled)

model_data%>%
  filter(Has_Insurance ==1)%>%
  summarise(Age = max(Age))

model_data%>%
  filter(Has_Insurance ==0)%>%
  summarise(Monthly_Income = min(Monthly_Income))
cor(model_data$Age,model_data$Monthly_Income)
#Plotting The Features Distributions
#Histogram of Age
ggplot(model_data,aes(x= Age))+
  geom_histogram(aes(y = ..density..),color =1,fill = "white")+
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  labs(title = "Histogram of Age")
#Histogram of Monthly income

ggplot(model_data,aes(x= Monthly_Income))+
  geom_histogram(aes(y = ..density..),color =1,fill = "white",binwidth=5000)+
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  xlim(10000,400000)

write_csv(model_data, file = "C:/Users/USER/Documents/model.csv")
#Sex
sex_dist<-table(model_data$Sex,model_data$Has_Insurance)
colnames(sex_dist)=c("No_Insurance","Has_Insurance")
rownames(sex_dist) <- c('Male',"Female")
addmargins(sex_dist)
prop.table(sex_dist)
sex_dist[1]
#Education
educ_table<- table(model_data$Has_Insurance,model_data$Education)
educ_table
rownames(educ_table)=c("No_Insurance","Has_Insurance")
colnames(educ_table) = c('No_education',"Some_Prim","Prim_comp")

