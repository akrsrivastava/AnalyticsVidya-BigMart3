library(stringr)
library(dplyr)
library(class)
library(gmodels)
library(ggplot2)
library(caret)
library(randomForest)
library(GGally)
library(gridExtra)
library(memisc) #mtable
library(gmodels)
library(rpart)
library(car)
library(gvlma)
library(doParallel)

train <- read.csv("D:/amit/Data Science/AnalyticsVidya/BigMart3/Train_UWu5bXk.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/Data Science/AnalyticsVidya/BigMart3/Test_u94Q5KV.csv", stringsAsFactors=FALSE)
train$IsTrain <- TRUE
test$IsTrain <- FALSE
test$Item_Outlet_Sales <- NA

combi <- bind_rows(train,test)

#Check blanks
sapply(combi,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features
#4016 outlet size blank

#Check NAs
sapply(combi,function(x){sum(ifelse(is.na(x),1,0))}) #Check no. of NAs in the features
#2439 Item_weight is NA

summary(combi)
scatterplotMatrix(combi)
#Item Identifier

length(unique(combi$Item_Identifier)) #1559 unique item identifiers
#First two characters of Item Identifier seem interesting
length(unique(substr(combi$Item_Identifier,1,2))) #Only 3 combinations
unique(substr(combi$Item_Identifier,1,2))
table(substr(combi$Item_Identifier,1,2))
length(unique(substr(combi$Item_Identifier,1,3))) #Only 71 combinations
#Is there any relation ship between the first two characters and Item TYpe
unique(combi$Item_Type)
unique(substr(combi[combi$Item_Type=='Dairy',]$Item_Identifier,1,2))
#All Dairy Items have Item Identiers starting with FD or DR

unique(substr(combi[combi$Item_Type=='Soft Drinks',]$Item_Identifier,1,2))
#All Dairy Items have Item Identiers starting with DR

#Create another ItemId Code with just the firsttwo chars
combi$Item_Identifier_Code= factor(substr(combi$Item_Identifier,1,2))

#Checking which Food Type belongs to which Item Code
ItemGroups_df <- combi%>%
                  group_by(Item_Identifier_Code) %>%
                  #select(Item_Identifier_Code,Item_Type) %>%
                  arrange(Item_Identifier_Code) %>%
                  as.data.frame
table(ItemGroups_df$Item_Identifier_Code,ItemGroups_df$Item_Type)
                  

#Item Fat Content
unique(combi$Item_Fat_Content) #Same levels with different wordings exist. Will have to be merged
combi[combi$Item_Fat_Content=="low fat",]$Item_Fat_Content ="Low Fat"
combi[combi$Item_Fat_Content=="LF",]$Item_Fat_Content ="Low Fat"
combi[combi$Item_Fat_Content=="reg",]$Item_Fat_Content ="Regular"
unique(combi$Item_Fat_Content)
combi$Item_Fat_Content=factor(combi$Item_Fat_Content)


#Item Type
#Convert to factor
combi$Item_Type=factor(combi$Item_Type)
unique(combi$Item_Type) #16 different Item Types. Should we combine them ? Or will we loose info?
ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=Item_Type,y=Item_Outlet_Sales), stat="summary" ,fun.y=median)
#Not much info perceivable

ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=substr(Item_Identifier,1,2),y=Item_Outlet_Sales), stat="summary" ,fun.y=median) 
#Median sale DR<FD<NC We are getting better info if we just consider the first two characters of Item ID
table(substr(combi$Item_Identifier,1,2))

#Item Weight
#This has 2439 NAs
#Do all item identifier have the same weight?
head(combi[combi$Item_Identifier=='DRC01',"Item_Weight"],100)
diffwts <- combi %>%
  filter(!is.na(Item_Weight)) %>%
  group_by(Item_Identifier) %>%
  summarize(nbrwts=length(unique(Item_Weight)), #nbrwts= nbr of diff weights
            wts=mean(Item_Weight))%>%
            as.data.frame 
unique(diffwts$wts)
#Yes All Item identifiers have same weights. wts will store the weight for that identifier.
#So  we can impute NA weights 
combi$Item_Weight <- ifelse(is.na(combi$Item_Weight),
                                (diffwts[match(combi$Item_Identifier,diffwts$Item_Identifier),"wts"]),
                                combi$Item_Weight 
)

summary(combi$Item_Weight)
#No more NAs
rm(diffwts) #Not required anymore


#Outlet Size
#4016 outlet size are blank
outsize <- combi %>%
       # filter(str_trim(Outlet_Size)!='') %>%
        group_by(Outlet_Identifier) %>%
        summarize(nbrsizes=length(unique(Outlet_Size)), 
                  sz=max(Outlet_Size)) %>%
        as.data.frame 
#Yes all outlet identifiers have the same Outlet_Size. But outlets 10, 17 and 45 never have an associated size
#So cant use Outlet Id to find Size
#Note on sz=max(Outlet_size) : We are constrained to use summary functions inside Summarize
#Since there is only one outlet size per outlet identifier max(Outlet_size) will return the outlet size. 

#Is there any relation between Outlet Size, Outlet Location and Outlet TYpe
#To do some investigations lets update all blank Outlet Size to XXX
combi$Outlet_Size <- ifelse(combi$Outlet_Size =="" | is.na(combi$Outlet_Size),'XXX',combi$Outlet_Size)
#Check again for blanks
nrow(combi[combi$Outlet_Size=='',])

with(combi, table(Outlet_Location_Type, Outlet_Size))
with(combi, table(Outlet_Type, Outlet_Size))
ggplot(combi) + geom_point(aes(x=Outlet_Size,y=Outlet_Location_Type,color=Outlet_Type),size=3)
#All tier 2 outlets are Small Size. So we can mark missing tier 2 outlets as small size
#Tier 3 outlets can be High or medium. Lets see the freq distribution
table(combi$Outlet_Size,combi$Outlet_Location_Type)
#3105 Tier 3 are Medium Size, 1553 are High Size. So no certainity
#Any other way to find out?
ggplot(combi) +geom_boxplot(aes(x=Outlet_Size,y=Item_Outlet_Sales))
#The Sales of XXX match those of the Small Outlet Size. So I believe it is better to update XXX to Small




#Now we can impute the Outlet Size
combi$Outlet_Size <- ifelse(str_trim(combi$Outlet_Size)=='XXX',"Small",  combi$Outlet_Size)
#Check again for blanks
nrow(combi[combi$Outlet_Size=='XXX',])
#Convert to factor
combi$Outlet_Size=factor(combi$Outlet_Size)

#Convert outlet establishment year to age of outlet
combi$AgeOfOutlet <- 2013-as.numeric(combi$Outlet_Establishment_Year)
combi$Outlet_Establishment_Year <- NULL
#Is there a relationship between age of outlet and sale
ggplot(subset(combi, IsTrain==TRUE)) +geom_line(aes(x=AgeOfOutlet,y=Item_Outlet_Sales), stat="summary", fun.y=mean)+
  scale_x_continuous(breaks=seq(0,30,1)) 

#Item Visibility
#Certain item visibilities are zero which makes no sense
ggplot(combi) + geom_histogram((aes(x=Item_Visibility))) #Item Visibility is right tailed
#We will impute using median of the visibility for the corrosponding Item Identifier

medianvisibility_df <- combi %>%
                        filter(Item_Visibility!=0) %>%
                        group_by(Item_Identifier) %>%
                        summarize(medvis=median(Item_Visibility)) %>%
                        as.data.frame


combi$Item_Visibility <- ifelse(combi$Item_Visibility==0,
                                medianvisibility_df[match(combi$Item_Identifier,medianvisibility_df$Item_Identifier),"medvis"],
                                combi$Item_Visibility)


#Outlet Identifier
#Convert to factor
combi$Outlet_Identifier = factor(combi$Outlet_Identifier)

ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=Outlet_Identifier,y=Item_Outlet_Sales), stat="summary", fun.y=sum)

#Outlet Location Type
#Convert to factor
combi$Outlet_Location_Type=factor(combi$Outlet_Location_Type)

#Item MRP
ggplot(combi) + geom_density(aes(x=Item_MRP),color = "blue")
#Outlet Type
combi$Outlet_Type=factor(combi$Outlet_Type)

#Checking relationship of Item Outlet Sales with differnet variables


#Multiple scatterplots
#ggpairs(data=subset(combi, IsTrain==TRUE),columns=c("Item_Weight","AgeOfOutlet","Item_Visibility","Item_Identifier_Code","Item_MRP","Item_Type","Item_Outlet_Sales"),axisLabels = 'internal')
#Only significant corelation is between Sales and MRP

#Trying MRP*Weight
combi$VOl_Sale = combi$Item_MRP*combi$Item_Weight
#ggpairs(data=subset(combi, IsTrain==TRUE),columns=c("Item_MRP","Item_Weight","VOl_Sale","Item_Outlet_Sales"),axisLabels = 'internal')
#No, Corelation with Vol Sales is lower than Item Mrp
combi$VOl_Sale <- NULL

#Item_Outlet_Sales vs MRP
ggplot(subset(combi, IsTrain==TRUE)) + geom_point(aes(x=Item_MRP, y = Item_Outlet_Sales),alpha=1/2 ,color="orange") +
  geom_smooth(method="lm")

#Item_Outlet_Sales vs AgeOf Outlet
ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=AgeOfOutlet,y=Item_Outlet_Sales),stat="summary",fun.y=mean)
#No discernible pattern

ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=Item_Type,y=Item_Outlet_Sales) , stat="summary", fun.y=mean) +
  facet_wrap("Outlet_Size")

ggplot(subset(combi, IsTrain==TRUE)) + geom_bar(aes(x=Outlet_Size,y=Item_Outlet_Sales),stat="summary",fun.y=mean)


#Create dummy Vars
# dmy <- dummyVars("~.",combi,fullRank = TRUE)
# combi <- as.data.frame(predict(dmy,combi))
# corelation_df <- as.data.frame(cor(combi))
#names(corelation_df) = c("Var1","Var2")
#corelation_df[which(corelation_df[,]>0.5)]
#Scale all numeric columns
combi <- preProcess(combi) %>%
          predict(combi) %>%
          as.data.frame

# column_index <- sapply(combi, is.numeric)
# combi[column_index] <- lapply(combi[column_index], scale)

#Model building
#Now Divide train into tow datsets to prepare and test regression model
# train_fortest=train[7001:8523,]
# train_fortrain=train[1:7000,]
set.seed(1)
train=combi[combi$IsTrain==TRUE,]
test=combi[combi$IsTrain==FALSE,]
train$IsTrain <- NULL
test$IsTrain <- NULL
inTrain <- createDataPartition(train$Item_Outlet_Sales,p=0.8,list=FALSE)
train_fortrain = train[inTrain,]
train_fortest=train[-inTrain,]

linearmodel <- lm(data=train_fortrain,Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
                    Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
                    Outlet_Type + Item_Identifier_Code + AgeOfOutlet)
Item_Outlet_Sales_Predict <- predict(linearmodel,test)

RMSE(pred= Item_Outlet_Sales_Predict, obs=test$Item_Outlet_Sales)
varimp_df <- data.frame(varImp(linearmodel))


linearmodel_caret <- train(data=train_fortrain,Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
                             Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
                             Outlet_Type + Item_Identifier_Code + AgeOfOutlet, method = "lm")


durbinWatsonTest(linearmodel) #To test Independence of Prdictors
#p-value=0.682 which means the predictors are independent This test is applicable for Time Series data


subsets <- c(1:5, 10)
set.seed(10)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
sales <- train_fortrain$Item_Outlet_Sales
predictors <- subset(train_fortrain, select=-c(Item_Outlet_Sales))

# do it in parallel
cl <- makeCluster(detectCores()); registerDoParallel(cl)
lmProfile <- rfe(x = predictors,
                 y = sales,
                 sizes = subsets,
                 rfeControl = ctrl)
stopCluster(cl); registerDoSEQ();




#Random Forest
# m1 <- randomForest(Item_Outlet_Sales ~ Item_MRP, data = train)
# m2 <- update(m1, ~ . + Item_Type)
# m3 <- update(m2, ~ . + Outlet_Location_Type)
# m4 <- update(m3, ~ . + Outlet_Type)
# m5 <- update(m4, ~ . + Outlet_Identifier)
# m6 <- update(m5, ~ . + Item_Identifier_Code)
# #m7 <- update(m6, ~ . + as.factor(train$Item_Identifier))
# mtable(m1, m2, m3, m4, m5,m6)
# m <- randomForest(data = train_fortrain, Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
#                     Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
#                     Outlet_Type + Item_Identifier_Code + AgeOfOutlet)  
# 
# summary(m)
# # m
# Item_Outlet_Sales_Predict <- predict(m,train_fortrain)
# df.rfImportance <- data.frame(variable = names(m$importance[,1]), importance = m$importance[,1])
# 
# df.rfImportance <- df.rfImportance[ order(-df.rfImportance[,2]),]
# #selectedVars <- sort(m$importance[,2],decreasing=TRUE)
# selectedVars <- df.rfImportance[1:9,1]
# selectedVars <- as.vector(selectedVars)
# selectedVars <- paste(selectedVars,collapse = "+")
# frml <- as.formula(paste("Item_Outlet_Sales ~ ", b, sep=" "))
# m_opt <- randomForest(data = train_fortrain, frml)   
# lm_opt <- lm(data = train_fortrain, frml)
# summary(lm_opt)
# 

submission_df <- data.frame (Item_Identifier= test$Item_Identifier,
                             Outlet_Identifier=test$Outlet_Identifier,
                             Item_Outlet_Sales = Item_Outlet_Sales_Predict) 
write.csv(submission_df,file="D:/amit/Data Science/AnalyticsVidya/BigMart3/submission_new.csv",row.names=FALSE)


#rpart Decision Tree #The score for this is less than the score of random forest
Item_Outlet_Sales_Actuals <- train$Item_Outlet_Sales
train$Item_Outlet_Sales <- NULL
rpartmodel <- rpart(data = train, Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
                    Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
                    Outlet_Type + Item_Identifier_Code + AgeOfOutlet)
Item_Outlet_Sales_Predict <- predict(rpartmodel,test)
