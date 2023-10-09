#Pertemuan 3
#load tidyverse
install.packages("gghighlight")
library(tidyverse)

view(mpg)
?mpg
str(mpg)

#visualization for manufacture
mpg%>%
  ggplot(aes(manufacturer))+
  geom_bar(aes(fill=manufacturer))+
  labs(x="manufacturer", y="number of cars",
      title="Number of Car by manufacturer")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="manufacturer")

#arrange
mpg%>%
  count(manufacturer)%>%
  arrange(n)%>%
  ggplot(aes(x=reorder(manufacturer, -n), y=n))+
  geom_bar(aes(fill=manufacturer), stat="identity")+
  labs(x="manufacturer", y="number of cars",
       title="Number of Car by manufacturer")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name="manufacturer")

#comparison graph
mpg%>%
  ggplot(aes(class))+
  geom_bar(aes(fill=drv), position = position_stack(reverse = TRUE))+
  coord_flip()+
  theme(legend.position = "right")+
  labs(title = "Number of DRV by Class", x="class",y="drv")

#geom point
mpg%>%
  ggplot(aes(displ,hwy, color=class))+
  geom_point()+
  geom_smooth(aes(x=displ, y=hwy))+
  facet_wrap(~drv)
  labs(title = "Impact of drive type  of efficiency",x="displ",y="hwy")

unique(mpg$drv)
  
mpg_fill<-mpg%>%
  filter(mpg$class=="suv")

mpg_fill%>%
  ggplot(aes(displ,hwy))+
  geom_point()



#distribution variable category psavert & uempmed
view(economics_long)
economics_long%>%
  filter(variable%in%
           c("psavert","uempmed"))%>%
  ggplot(aes(x=date,y=value,col=variable))+
  geom_line()

#composition
view(Titanic)
class(Titanic)
Titanic<-as.data.frame(Titanic)
str(Titanic)

#count passenger based on class
count_class<-aggregate(Freq~Class, data = Titanic, FUN = sum)
count_class

#using mutate, counting the proportion
count_class<-count_class%>%
  mutate(prop=round(prop.table(Freq)*100,2))%>%
  arrange(desc(Class))%>%
  mutate(lab.ypos=cumsum(prop)-0.5)

#making pie chart
library(gghighlight)
count_class%>%
  ggplot(aes(x="",y=prop,fill=Class))+
  ggtitle("Proportion of Passengers")+
  geom_bar(width = 1, stat = "identity", color="black")+
  coord_polar("y", start = 0)+
  geom_text(aes(y=lab.ypos, label=prop), color="black")+
  scale_fill_manual(values = c("red","yellow","green","grey"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_void()

#Relationship plot
view(mpg)

#filled with class
mpg%>%
  ggplot(aes(displ,hwy,fill=class))+
  geom_point()
#Titanic
Titanic%>%
  ggplot(aes(Class,Freq,fill=Survived))+
  geom_bar(stat="identity", position = "dodge")+
  theme_bw()+
  theme(legend.position = "right")+
  ggtitle("Passengers Who Survived")

#save plot
?ggsave
ggsave("Titanic.jpg")


#Next EDA
