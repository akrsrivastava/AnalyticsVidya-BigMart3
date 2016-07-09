library(stringr)
library(dplyr)
library(class)
library(gmodels)
library(ggplot2)
train <- read.csv("D:/amit/R/AnalyticsVidya/BigMart3/Train_UWu5bXk.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/R/AnalyticsVidya/BigMart3/Test_u94Q5KV.csv", stringsAsFactors=FALSE)
train$IsTrain <- TRUE
test$IsTrain <- FALSE
test$Item_Outlet_Sales <- NA

combi <- bind_rows(train,test)



summary(combi)



sapply(combi,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features

combi$Item_Fat_Content=factor(combi$Item_Fat_Content)
str(combi$Item_Fat_Content)
levels(combi$Item_Fat_Content) #Same levels with different wordings exist. Will have to be merged
combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)
unique(combi$Item_Fat_Content)
combi[combi$Item_Fat_Content=="low fat",]$Item_Fat_Content ="Low Fat"
combi[combi$Item_Fat_Content=="LF",]$Item_Fat_Content ="Low Fat"
combi[combi$Item_Fat_Content=="reg",]$Item_Fat_Content ="Regular"
unique(combi$Item_Fat_Content)
combi$Item_Fat_Content=factor(combi$Item_Fat_Content)

summary(combi$Item_Identifier)
length(unique(combi$Item_Identifier))

unique(combi$Item_Type)
ggplot(combi) + geom_bar(aes(x=Item_Type, stat="identity") , fill="orange") +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust = 0.5))


unique(combi$Outlet_Identifier)
#COnvert to factor
combi$Outlet_Identifier=factor(combi$Outlet_Identifier)
table(combi$Outlet_Identifier)

combi$Outlet_Establishment_Year=as.factor(combi$Outlet_Establishment_Year)

unique(combi$Outlet_Location_Type)
table(combi$Outlet_Location_Type)
unique(combi$Outlet_Type)
table(combi$Outlet_Type)
#Is there any relationship between outlet size and outlet location type
table(combi$Outlet_Size,combi$Outlet_Location_Type) 
#Is there any relationship between outlet size and outlet type 
table(combi$Outlet_Size,combi$Outlet_Type) 

ggplot(combi)+geom_bar(aes(x=Outlet_Location_Type,fill=Outlet_Type, stat="identity")) +facet_wrap("Outlet_Size")


#Missing Value treatments
#Item Weight

summary(combi$Item_Weight)
head(combi[is.na(combi$Item_Weight),])

mean_weight_df <- combi %>%
  group_by(Item_Identifier) %>%
  summarize(avg_item_weight=mean(Item_Weight,na.rm=TRUE)) 

###TRy this using match
combi <- inner_join(combi,mean_weight_df)
for (i in 1 : nrow(combi))
{
  if (is.na(combi[i,"Item_Weight"]))
      combi[i,"Item_Weight"]=combi[i,"avg_item_weight"]
}
combi$avg_item_weight <- NULL

#Now Outlet Size # This section took me two days !!!
table(combi$Outlet_Identifier,combi$Outlet_Size) #Outlets 10, 17 and 45 have blank outlet sizes
table(combi$Outlet_Identifier,combi$Outlet_Type)
table(combi$Outlet_Type,combi$Outlet_Size)
df <- as.data.frame(table(Type=combi$Outlet_Type,Size=combi$Outlet_Size))

Outlet_size_mode <- df %>%
  filter(Size!="") %>%
  select(Type,Size,Freq) %>%
  group_by(Type) %>%
  filter(Freq==max(Freq)) %>%
  as.data.frame
#THis dataframe gives the mode Size of each outlet type

combi$Outlet_Size <- ifelse(combi$Outlet_Size=="",
                            as.character(Outlet_size_mode[match(combi$Outlet_Type,Outlet_size_mode$Type),"Size"]),
                            combi$Outlet_Size)
#Remove the Zeros in visibility with the mean visibility 
mean_visibility <- combi %>%
                    group_by(Item_Identifier) %>%
                    summarize(avgvis=mean(Item_Visibility,na.rm=TRUE)) %>%
                    select(Item_Identifier,avgvis) %>%
                    as.data.frame
combi$Item_Visibility <- ifelse(combi$Item_Visibility==0,
                                as.numeric(mean_visibility[match(combi$Item_Identifier,mean_visibility$Item_Identifier),"avgvis"]),
                                combi$Item_Visibility
                                )

#COmbine the 16 item type as per first two chars in Item Identifier
combi$New_Item_Type <-   substr(combi$Item_Identifier,1,2)
combi$Item_Type <- NULL

#Convert char features to factor
combi$Item_Identifier=factor(combi$Item_Identifier)
#combi$Item_Type=factor(combi$Item_Type)
combi$Outlet_Size=factor(combi$Outlet_Size)
combi$Outlet_Location_Type=factor(combi$Outlet_Location_Type)
combi$Outlet_Type=factor(combi$Outlet_Type)

#Convert outlet establishment year to age of outlet
combi$AgeOfOutlet <- 2013-as.numeric(combi$Outlet_Establishment_Year)
combi$Outlet_Establishment_Year <- NULL

#Divide back into train and test
train <- combi[combi$IsTrain==TRUE,]
test <- combi[combi$IsTrain!=TRUE,]
train$IsTrain <- NULL
test$IsTrain <- NULL
summary(train) #No NAs
summary(test) #NO NAs
sapply(train,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features
sapply(test,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features

#Now Divide train into tow datsets to prepare and test regression model
train_fortest=train[7001:8523,]
train_fortrain=train[1:7000,]
#train_fortest$Item_Identifier <- NULL
#train_fortrain$Item_Identifier <- NULL
# 
linearmodel1 <- lm(data=train_fortrain,Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content+Item_Visibility+
                   Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+New_Item_Type)


#linearmodel1 <- lm(data=train_fortrain,Item_Outlet_Sales ~ Item_MRP+Outlet_Size+Outlet_Type)
Item_Outlet_Sales_Predict <- predict(linearmodel1,train_fortest)
#CrossTable(Item_Outlet_Sales_Predict,train_fortest$Item_Outlet_Sales)
summary(linearmodel1)
summary(Item_Outlet_Sales_Predict)
plot(linearmodel1)

#Now applying model to actual test data
Item_Outlet_Sales_Predict <- predict(linearmodel1,test)




submission_df <- data.frame (Item_Identifier= test$Item_Identifier,
                       Outlet_Identifier=test$Outlet_Identifier,
                       Item_Outlet_Sales = Item_Outlet_Sales_Predict)
                       
#write.csv(submission_df,file="D:/amit/R/AnalyticsVidya/BigMart3/submission.csv",row.names=FALSE)
