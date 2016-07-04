library(stringr)
library(dplyr)
library(class)
library(gmodels)
library(ggplot2)
train <- read.csv("D:/amit/R/AnalyticsVidya/BigMart3/Train_UWu5bXk.csv", stringsAsFactors=FALSE)

summary(train)



sapply(train,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features

train$Item_Fat_Content=factor(train$Item_Fat_Content)
str(train$Item_Fat_Content)
levels(train$Item_Fat_Content) #Same levels with different wordings exist. Will have to be merged
train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
unique(train$Item_Fat_Content)
train[train$Item_Fat_Content=="low fat",]$Item_Fat_Content ="Low Fat"
train[train$Item_Fat_Content=="LF",]$Item_Fat_Content ="Low Fat"
train[train$Item_Fat_Content=="reg",]$Item_Fat_Content ="Regular"
unique(train$Item_Fat_Content)
train$Item_Fat_Content=factor(train$Item_Fat_Content)

summary(train$Item_Identifier)
length(unique(train$Item_Identifier))

unique(train$Item_Type)
ggplot(train) + geom_bar(aes(x=Item_Type, stat="identity") , fill="orange") +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust = 0.5))

unique(train$Outlet_Identifier)
#COnvert to factor
train$Outlet_Identifier=factor(train$Outlet_Identifier)
table(train$Outlet_Identifier)

train$Outlet_Establishment_Year=as.factor(train$Outlet_Establishment_Year)

unique(train$Outlet_Location_Type)
table(train$Outlet_Location_Type)
unique(train$Outlet_Type)
table(train$Outlet_Type)
#Is there any relationship between outlet size and outlet location type
table(train$Outlet_Size,train$Outlet_Location_Type) 
#Is there any relationship between outlet size and outlet type 
table(train$Outlet_Size,train$Outlet_Type) 

ggplot(train)+geom_bar(aes(x=Outlet_Location_Type,fill=Outlet_Type, stat="identity")) +facet_wrap("Outlet_Size")


#Missing Value treatments
#Outlet Size

summary(train$Item_Weight)
head(train[is.na(train$Item_Weight),])

mean_weight_df <- train %>%
  group_by(Item_Identifier) %>%
  summarize(avg_item_weight=mean(Item_Weight,na.rm=TRUE)) 


train <- inner_join(train,mean_weight_df)
for (i in 1 : nrow(train))
{
  if (is.na(train[i,"Item_Weight"]))
      train[i,"Item_Weight"]=train[i,"avg_item_weight"]
}
train$avg_item_weight <- NULL

#Now Outlet Size
table(train$Outlet_Identifier,train$Outlet_Size) #Size not available for outlets 10,17 and 45
table(train$Outlet_Location_Type,train$Outlet_Size)


ggplot(train) +geom_boxplot(aes(x=Outlet_Type, y= Item_Outlet_Sales)) + facet_wrap("Outlet_Size") 

#Lets compare the sales
sales <- train %>%
  select(Outlet_Identifier,Item_Outlet_Sales,Outlet_Type) %>%
  group_by(Outlet_Identifier) %>%
  summarize(total_outlet_sales=sum(Item_Outlet_Sales)) %>%
  arrange(total_outlet_sales)

table(train$Outlet_Type,train$Outlet_Size)

maxsize <- train %>%
  group_by(Outlet_Type) %>%
  tally()
