#Pertemuan 4 - EDA
library(tidyverse)
view(diamonds)
?diamonds

#variasi of cut
diamonds%>%
  ggplot(aes(cut))+
  geom_bar()
diamonds%>%
  count(cut)

#distribution of carat
diamonds%>%
  ggplot(aes(carat))+
  geom_histogram(binwidth = 0.5) #binwidth is for bar width to expand variation
diamonds%>%
  count(cut_width(carat, 0.5))

#smaller data
smaller<-diamonds%>%
  filter(carat<3)
smaller%>%
  ggplot(aes(carat))+
  geom_histogram(binwidth = 0.01)
smaller%>%
  ggplot(aes(carat, color=cut))+
  geom_freqpoly()

#histogram for y variable
diamonds%>%
  ggplot(aes(x=y))+
  geom_histogram(binwidth = 0.5)+
  coord_cartesian(ylim = c(0,50))

library(dplyr)
#filter data
unusual<-diamonds%>%
  filter(y<3 | y>20)%>%
  select(price,x,y,z)
unusual

#if we want to take out the outlier
diamonds2<-diamonds%>%
  filter(between(y,3,20))

diamonds3<-diamonds%>%
  mutate(y=ifelse(x<3|y>20,NA,y))
diamonds3

#plotting the new data using geom_point
diamonds3%>%
  ggplot(aes(x=x,y=y))+
  #geom_point()
  geom_point(na.rm = TRUE)

#covariation
diamonds%>%
  ggplot(aes(price))+
  #geom_freqpoly()
  geom_freqpoly(aes(colour = cut), binwidth=500)

#using density in y variable
diamonds%>%
  ggplot(aes(x=price, y=..density..))+
  geom_freqpoly(aes(colour = cut), binwidth=500)

#distribution using boxplot function
diamonds%>%
  ggplot(aes(cut, price))+
  geom_boxplot()

head(mpg,10)
mpg%>%
  ggplot(aes(class, hwy))+
  geom_boxplot()
mpg%>%
  ggplot(aes(x=reorder(class,hwy, FUN = median), y=hwy))+
  geom_boxplot()+
  coord_flip()

#correlation between 2 categoricals variable
diamonds%>%
  ggplot(aes(cut, color))+
  geom_count()

diamonds%>%
  count(cut,color)

#visualize using geom title
diamonds%>%
  count(cut, color)%>%
  ggplot(aes(x=color, y=cut))+
  geom_tile(aes(fill=n))

#using carat and price variable
diamonds%>%
  ggplot(aes(carat, price))+
  geom_point()

#geom bin2
diamonds%>%
  ggplot(aes(carat,price))+
  geom_bin2d()

#geom hex
install.packages("hexbin")
  library(hexbin)
  diamonds%>%
    ggplot(aes(carat, price))+
    geom_hex()

#boxplot
smaller%>%
  ggplot(aes(carat, price))+
  geom_boxplot(aes(group=cut_width(carat,0.1)))

#boxplot using cut number
smaller%>%
  ggplot(aes(carat, price))+
  geom_boxplot(aes(group=cut_number(carat,20)))

#pattern and model
faithful%>%
  ggplot(aes(eruptions, waiting))+
  geom_point()

#model
#linear model, x=price, y=carat
library(modelr)
mod<-lm(log(price)~log(carat), data=diamonds)
mod
residu<-resid(mod)
residu

diamonds2<-diamonds%>%
  add_residuals(mod)%>%
  mutate(residual=exp(resid))
view(diamonds2)

#plotting
diamonds2%>%
  ggplot(aes(x=carat, y=resid))+
  geom_point()

#boxplot cut and resid
diamonds2%>%
  ggplot(aes(cut, resid))+
  geom_boxplot()
