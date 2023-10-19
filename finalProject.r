#Import Library
library(tidyverse)

#Import Data
dataBillionaire<-read.csv("D:\\Lab\\Sadasa - R Basic\\BillionairesStatisticsDataset.csv",header=TRUE)
#Selecting Data
#Select Column
data<-dataBillionaire%>%
  select(4,5,6,3,2)
#Select Top 10 Country
top10<-data%>%     #Top 10 Country
  count(country)%>%
  arrange(desc(n))
view(top10)        #View Top 10 Country
data<-data%>%      #Filter Top 10 Country
  filter(country %in% c("United States",
                        "China",
                        "India",
                        "Germany",
                        "United Kingdom",
                        "Russia",
                        "Switzerland",
                        "Hong Kong",
                        "Italy",
                        "Singapore"))
#The Data
view(data)
dim(data)
str(data)


#Preprocessing Data
#Detect missing values
colSums(is.na(data))
#Detect outlier on age
data%>%
  ggplot(aes(age))+
  geom_histogram(binwidth = 2)


#Manipulating Data
#Transform age to double
data<-transform(data, age=as.double(age))  
#Replace missing value in age with mean
data<-data%>%         
  mutate(age = replace_na(age, mean(age, na.rm = TRUE)))
colSums(is.na(data))
#Remove Outlier age<25
data<-data%>%
  filter(age>25)
data%>%
  ggplot(aes(age))+
  geom_histogram(binwidth = 2)


#Visualization Data
#Bar chart - Number of Billionare Based On Country
data%>%
  count(country)%>%
  ggplot(aes(x = reorder(country, -n), y = n))+
  geom_bar(stat = "identity", fill = "#0a0a42")+
  labs(title = "Number of Billionare Based On Country",
    x = "Country", y = "Number of Billionare") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0))

#Bar chart - Mean Wealth Based on Industry
mean_data <- data %>%       #Create mean_data
  group_by(category) %>%
  summarise(mean_worth = mean(finalWorth))
mean_data <- mean_data %>%  #Sorting mean_data
  arrange(desc(mean_worth))
mean_data%>%                #Visualize
  ggplot(aes(x = reorder(category, mean_worth), y = mean_worth))+
  geom_bar(stat = "identity", fill = "#0a0a42")+
  labs(title = "Mean Wealth Based on Industry",
    x = "Industry Category", y = "Mean Wealth") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0))

#Tile - Number Billionare Distribution based on Category and Country
choosen%>%
  count(category, country)%>%
  ggplot(aes(x=category, y=country))+
  geom_tile(aes(fill=n))+
  labs(title = "Number Billionare Distribution based on Industry Category and Country",
    x = "Industry Category", y = "Country") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0))
