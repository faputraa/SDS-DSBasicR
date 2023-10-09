#Pertemuan 2

#install package
install.packages(c("tidyverse","tidyr","readr","dplyr","missForest","Hmisc","readxl"))
#load packages
library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(Hmisc)
library(missForest)
library(readxl)

#import CSV
df<-read.csv("iris.csv", header = FALSE,
             col.names = c("sepal_length", "sepal_width", "petal_length", "petal_width", "species"))
head(df,5)

#import excel
df_exc<- read_excel("product_price.xlsx", sheet = "Sheet1")
head(df_exc,5)

#Preprocessing

#check dimension
dim(df)

#check variable names
names(df)

#check structure and missing value
str(df)

#missing value
colSums(is.na(df))
df_miss<-prodNA(df,noNA=0.2)
df_miss%>%
    head()
colSums(is.na(df_miss))

#select_if from dplyr
df_num<- df_miss%>%
  select_if(is.numeric)
df_num%>%
  head()

#impute from misc library, mean
df_mean<- Hmisc::impute(df,cols=df_num, fun=mean)
head(df_mean,5)
                         
#impute using median
df_med<- Hmisc::impute(df, cols=df_num, fun=function(x) ifelse(is.na(x), median(x, na.rm=TRUE),x))
head(df_med,5)

#verbose using missForest
miss_na<- missForest(df_num, verbose = TRUE)
head(miss_na,5)
head(round(miss_na$ximp, digits=2))

#Wrangling Data
#choosing column, filtering data, summary, transformation
#load R dataset
data(mtcars)
df_mtcars<-mtcars
head(df_mtcars)

#giving name
data1<-df_mtcars%>%
  tibble::rownames_to_column(var = "car_types")
data1%>%
  head()

#select
some_column<-df_mtcars%>%
  dplyr::select(mpg, hp, drat)
some_column%>%
  head()

some_column1<-df_mtcars%>%
  dplyr::select(1:4,6)
some_column1%>%
  head()

#info dataset
?mtcars
?msleep

#mutate: creating new column, transmute
df1<-msleep
#mutate
mutate_df<-df1%>%
  dplyr::select(name, sleep_total)%>%
  mutate(total_sleep=sleep_total*60)
mutate_df%>%
  head()
mutate_df%>%
  arrange(desc(total_sleep))%>%
  head(10)

#transmute
trans_df<-df1%>%
  dplyr::select(name, sleep_total)%>%
  transmute(total_sleep=sleep_total*60)
trans_df%>%
  arrange(desc(total_sleep))%>%
  head(10)

#mutate function with case_when
categ<-df1%>%
  dplyr::select(name, sleep_total)%>%
  mutate(disc_col=case_when(
    sleep_total>13 ~ "Very Long",
    sleep_total>10 ~ "Long",
    sleep_total>7 ~ "Limited",
    TRUE ~ "Short"
  )) %>%
  mutate(disc_col=factor(disc_col, levels=c("Very Long","Long","Limited","Short")))
head(categ)

#save data
write.csv(categ, "new_data.csv")

df_mtcars<-df_mtcars%>%
  tibble::rownames_to_column(var = "car_types")
df_mtcars%>%
  head()

#Separate car types to name, type
separate_cartypes<-df_mtcars%>%
  dplyr::select(car_types)%>%
  tidyr::separate(car_types, into=c("name","type"), sep=" ", extra="merge", fill="right")
separate_cartypes%>%
  head()

#Unite, combining the data
unite_df<-separate_cartypes%>%
  dplyr::select(name,type)%>%
  tidyr::unite(col_combine, name, type, sep =" ")
unite_df%>%
  head()

#Filter function from dplyr library
df_filter<- df_mtcars%>%
  dplyr::select(car_types,disp)%>%
  dplyr::filter(disp>100)
df_filter%>%
  arrange(desc(disp))%>%
  head()

#Between function to get middle value
df_between<-df_mtcars%>%
  dplyr::select(car_types,hp)%>%
  dplyr::filter(between(hp,100,200))
df_between%>%
  arrange(desc(hp))%>%
  head()

#Summary data, msleep data
df1
#group_by function
df_group<-df1%>%
  dplyr::group_by(genus)%>%
  dplyr::count(order, sort=TRUE)
df_group%>%
  head()
#Summarize function
df_sum<-df1%>%
  dplyr::group_by(order)%>%
  summarise(n=n(),average=mean(sleep_total), maximum=max(sleep_total),minimun=min(sleep_total))
df_sum%>%
  head()

group_da<- df1%>%
  dplyr::group_by(order,genus)%>%
  summarise(
    tot_sleep=sum(sleep_total),
    meanawake=mean(awake),
    max_brainwt=max(brainwt)
  )
head(group_da,5)
