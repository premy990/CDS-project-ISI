library(xgboost)
library(Matrix)
library(MatrixModels)
library(data.table)



test= read.csv("C:/Users/DELL/Desktop/Tanzania/test set values.csv")



test$status_group <- 0
#View(test)
dim(test)

train<-read.csv("C:/Users/DELL/Desktop/Tanzania/training set values.csv")
label<- read.csv("C:/Users/DELL/Desktop/Tanzania/training set labels.csv")

label <- subset(label, select = status_group )


train<-cbind(train,label)


train$status_group<-0

train$tst <- 0
test$tst <- 1


data<- rbind(train,test)


data$date_recorded<-as.Date(data$date_recorded)
#str(data)

data$region_code<-factor(data$region_code)
data$district_code<-factor(data$district_code)


min_year<-1960
data$construction_year<-data$construction_year-min_year
summary(data)
data$construction_year[data$construction_year<0]= median(data$construction_year[data$construction_year>0])


data$gps_height[data$gps_height==0]=median(data$gps_height[data$gps_height>0])

data$amount_tsh[data$amount_tsh==0]<-median(data$amount_tsh[data$amount_tsh>0]


data$num_private<-NULL


data$recorded_by<-NULL


data$wpt_name<-NULL


data$extraction_type_group<-NULL
data$extraction_type<-NULL


data$payment_type<-NULL


data$water_quality<-NULL

#data$basin<-NULL
data$scheme_management<-NULL


data$district_code<-NULL
data$region<-NULL
data$region_code<-NULL
data$subvillage<-NULL
data$ward<- NULL


data$waterpoint_type_group<-NULL


data$quantity_group<-NULL


data$installer<-NULL


data_train <- data[data$tst==0,]
data_test <- data[data$tst==1,]


data_test.noID<-subset(data_test, select = -id)

data_train<-subset(data_train, select = c(-id,-status_group))

data_test.noID <- as.matrix(as.data.frame(lapply(data_test.noID, as.numeric)))
data_train <- as.matrix(as.data.frame(lapply(data_train, as.numeric)))
label<-as.numeric(label$status_group)


train.DMatrix <- xgb.DMatrix(data = data_train,label = label, missing = NA


i=2


solution.table<-data.frame(id=data_test[,"id"])
for (i in 2:12){
  #Set seed so that the results are reproducible
  set.seed(i)
  
  
  xgb.tab = xgb.cv(data = train.DMatrix, objective = "multi:softmax", booster = "gbtree",
                   nrounds = 500, nfold = 4, early.stop.round = 10, num_class = 4, maximize = FALSE,
                   evaluation = "merror", eta = .2, max_depth = 12, colsample_bytree = .4)
  
  
  min.error.idx = which.min(xgb.tab[, test.merror.mean])
  
  
  model <- xgboost(data = train.DMatrix, objective = "multi:softmax", booster = "gbtree",
                   eval_metric = "merror", nrounds = min.error.idx, 
                   num_class = 4,eta = .2, max_depth = 14, colsample_bytree = .4)
  
  
  
  predict <- predict(model,data_test.noID)
  
  
  predict[predict==1]<-"functional"
  predict[predict==2]<-"functional needs repair"
  predict[predict==3]<-"non functional"
  
  
  table(predict)
  
  
  
  solution.table[,i]<-predict
}


solution.table.count<-apply(solution.table,MARGIN=1,table)


predict.combined<-vector()

x=1
for (x in 1:nrow(data_test)){
  predict.combined[x]<-names(which.max(solution.table.count[[x]]))}


table(predict.combined)


solution<- data.frame(id=data_test[,"id"], status_group=predict.combined)


head(solution)


write.csv(solution, file = "Water_solution - xgboost 45.csv", row.names = FALSE)



importance <- xgb.importance(feature_names = colnames(data_train), model =model)
importance
xgb.plot.importance(importance_matrix = importance)
