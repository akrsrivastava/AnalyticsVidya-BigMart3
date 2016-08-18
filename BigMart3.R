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
library(dummies)
library(Boruta)

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

#Feature Selection with Boruta
train_forBoruta <- dummy.data.frame(combi[combi$IsTrain==TRUE,], names = c("Item_Fat_Content","Item_Type",
                                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                                   "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
#Takes two hours
#boruta.train <- Boruta(Item_Outlet_Sales~.-c(Item_Identifier,IsTrain), data = train_forBoruta, doTrace = 2)


#Model building
#Now Divide train into tow datsets to prepare and test regression model
# train_fortest=train[7001:8523,]
# train_fortrain=train[1:7000,]
set.seed(1)

train=combi[combi$IsTrain==TRUE,]
test=combi[combi$IsTrain==FALSE,]

inTrain <- createDataPartition(train$Item_Outlet_Sales,p=0.8,list=FALSE)
train_fortrain = train[inTrain,]
train_fortest=train[-inTrain,]

train_fortrain$Item_Identifier <- NULL
train_fortest$Item_Identifier <- NULL
train_fortrain$IsTrain <- NULL
train_fortest$IsTrain <- NULL

linearmodel <- lm(data=train_fortrain,Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
                    Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
                    Outlet_Type + Item_Identifier_Code + AgeOfOutlet)
#Predict Test Data Set
Item_Outlet_Sales_Predict <- predict(linearmodel,train_fortest)

RMSE(pred= Item_Outlet_Sales_Predict, obs=train_fortest$Item_Outlet_Sales) #1159.762
varimp_df <- data.frame(varImp(linearmodel))


#PCA
#Create dummy Vars
# dmy <- dummyVars("~.",combi[,-1],fullRank = TRUE)
# combi <- as.data.frame(predict(dmy,combi))
pca.train_fortrain <- train_fortrain
pca.train_fortest <- train_fortest


pca.train_fortrain$Item_Outlet_Sales <- NULL
pca.train_fortest$Item_Outlet_Sales <- NULL


pca.train_fortrain <- dummy.data.frame(pca.train_fortrain, names = c("Item_Fat_Content","Item_Type",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))

str(pca.train_fortrain)

prin_comp <- prcomp(pca.train_fortrain, scale. = T)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[1:5,1:4]
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
 std_dev <- prin_comp$sdev

#compute variance
 pr_var <- std_dev^2

#check variance of first 10 components
 pr_var[1:10]

 prop_varex <- pr_var/sum(pr_var)
 
 plot(prop_varex, xlab = "Principal Component",
      ylab = "Proportion of Variance Explained",
      type = "b")
prin_comp$x

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train_fortrain$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]


#pca.linearmodel <- lm(data=train.data,Item_Outlet_Sales ~ .)
#pca.randomforest <- randomForest(data=train.data,Item_Outlet_Sales ~ .)

pca.train_fortest <- dummy.data.frame(pca.train_fortest, names = c("Item_Fat_Content","Item_Type",
                                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                                     "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
test.data <- predict(prin_comp, newdata = pca.train_fortest)
test.data <- as.data.frame(test.data)

test.data <- test.data[,1:30]
#Item_Outlet_Sales_Predict<- predict(pca.linearmodel,  test.data)
#Item_Outlet_Sales_Predict<- predict(pca.randomforest,  test.data)

RMSE(pred= Item_Outlet_Sales_Predict, obs=train_fortest$Item_Outlet_Sales) #1159.809(lm) 1163.789(Random Forest. Its more than RMSE of lm !!!)
#RMSE without PCA was 1159.762(lm).1146 (Random Forest) . 
#Lets check the actual test data
test$Item_Outlet_Sales <- NULL
pca.test <- dummy.data.frame(test, names = c("Item_Fat_Content","Item_Type",
                                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                                   "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

test.data <- test.data[,1:30]
#Item_Outlet_Sales_Predict<- predict(pca.linearmodel,  test.data)
Item_Outlet_Sales_Predict<- predict(pca.randomforest,  test.data)


# linearmodel_caret <- train(data=train_fortrain,Item_Outlet_Sales ~ Item_Weight +  Item_Fat_Content + Item_Visibility +
#                              Item_Type + Item_MRP + Outlet_Identifier + Outlet_Size + Outlet_Location_Type +
#                              Outlet_Type + Item_Identifier_Code + AgeOfOutlet, method = "lm")
# 
# 
# durbinWatsonTest(linearmodel) #To test Independence of Prdictors
# #p-value=0.682 which means the predictors are independent This test is applicable for Time Series data
# 
# 
# subsets <- c(1:5, 10)
# set.seed(10)
# 
# ctrl <- rfeControl(functions = lmFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# sales <- train_fortrain$Item_Outlet_Sales
# predictors <- subset(train_fortrain, select=-c(Item_Outlet_Sales))
# 
# # do it in parallel
# cl <- makeCluster(detectCores()); registerDoParallel(cl)
# lmProfile <- rfe(x = predictors,
#                  y = sales,
#                  sizes = subsets,
#                  rfeControl = ctrl)
# stopCluster(cl); registerDoSEQ();


###XGBoost
xgb.train_fortrain <- train_fortrain
xgb.train_fortest <- train_fortest

xgb.train_fortrain$Item_Identifier <- NULL
xgb.train_fortrain$IsTrain <- NULL
xgb.train_fortrain_ItemOutletSales <- xgb.train_fortrain$Item_Outlet_Sales
xgb.train_fortrain$Item_Outlet_Sales <- NULL

xgb.train_fortest$Item_Identifier <- NULL
xgb.train_fortest$IsTrain <- NULL
xgb.train_fortest_ItemOutletSales <- xgb.train_fortest$Item_Outlet_Sales
#xgb.train_fortest$Item_Outlet_Sales <- NULL



xgb.train_fortrain <- dummy.data.frame(xgb.train_fortrain, names = c("Item_Fat_Content","Item_Type",
                                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                                     "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
#xgb.train_fortrain <- xgb.DMatrix(data.matrix (xgb.train_fortrain),label =xgb.train_fortrain_ItemOutletSales)

#Cross Validation
# xgb.crossValidation <- xgb.cv(data=xgb.train_fortrain,nrounds=1000,objective="reg:linear" ,
#                               nfold=10, eta= 0.01, early.stop.round = 4,maximize=FALSE)
# xgb_model <- xgboost(data=xgb.train_fortrain,nrounds=330,objective="reg:linear",eta=0.01 )

#Tuning XGBoost using Caret

# pack the training control parameters
xgb.cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 5, 
                        #summaryFunction = twoClassSummary,
                        #classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.01,0.05,0.1),
                        max_depth = c(2,4,6,8,10,14),
                        gamma=1,
                        colsample_bytree=1,
                        min_child_weight=1
)

xgb_train_1 = train(
  x=as.matrix(xgb.train_fortrain),
  y=xgb.train_fortrain_ItemOutletSales,
  #data= xgb.train_fortrain,
  trControl = xgb.cv.ctrl,
  tuneGrid = xgb.grid,
  method = "xgbTree",
  nthread=4,
  metric="RMSE",
  #early_stopping_rounds = 4,
  maximize=FALSE
)
#Prediction
xgb.train_fortest <- dummy.data.frame(xgb.train_fortest, names = c("Item_Fat_Content","Item_Type",
                                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                                     "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
xgb.train_fortest <- xgb.DMatrix(data.matrix (xgb.train_fortest),label =xgb.train_fortest$Item_Outlet_Sales)

#pred <- predict(xgb_model,xgb.train_fortest)
pred <- predict(xgb_train_1$finalModel,xgb.train_fortest) #Using model generated from caret CV

RMSE(pred,train_fortest$Item_Outlet_Sales) #1134 Slightly less than the RMSE of lm()
#1122 using model generated from Caret CV

#Predicting the actual test data
xgb.predict.test <- test
xgb.predict.test$Item_Identifier <- NULL
xgb.predict.test$IsTrain <- NULL
xgb.predict.test$Item_Outlet_Sales <- NULL
xgb.predict.test <- dummy.data.frame(xgb.predict.test, names = c("Item_Fat_Content","Item_Type",
                                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                                   "Outlet_Location_Type","Outlet_Type","Outlet_Identifier","Item_Identifier_Code"))
xgb.predict.test <- xgb.DMatrix(data.matrix (xgb.predict.test))
#Item_Outlet_Sales_Predict <- predict(xgb_model,xgb.predict.test)
Item_Outlet_Sales_Predict <- predict(xgb_train_1$finalModel,xgb.predict.test) #Using model generated from caret CV
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



