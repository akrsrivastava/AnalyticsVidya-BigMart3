library(stringr)
library(dplyr)
library(class)
library(gmodels)
library(ggplot2)
train <- read.csv("D:/amit/R/AnalyticsVidya/BigMart3/Train_UWu5bXk.csv", stringsAsFactors=FALSE)
summary(train)

train$Item_Fat_Content=factor(train$Item_Fat_Content)
str(train$Item_Fat_Content)

sapply(train,function(x){sum(ifelse(str_trim(x)=="",1,0))})

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
  