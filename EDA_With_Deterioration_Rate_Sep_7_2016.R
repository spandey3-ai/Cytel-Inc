install.packages("caret")
install.packages("e1071")
install.packages("xgboost")
install.packages("randomForest")
# Data= read.csv("Merged_For_Modeling_with_Det_Rate_greater_than_minus_point_5.csv")
Data=read.csv(file.choose(),header=T)
#Data = read.csv("Data_FOR_MODELING_24_JUN_with_det_rate_and_perc.csv")
dim(Data)
dim(Data)[2]-2
Data = subset(Data, Data$DETERIORATION_RATE < -0.01)
dim(Data)
head(Data)

par(mfrow=c(2,2))
plot(Data$FREIGHT_SPEED_M,Data$DETERIORATION_RATE,xlab='Freight Speed', ylab='Deterioration Rate',col='blue')
#plot(Data$CURVE_DELTA_SCORE_M,Data$DETERIORATION_RATE,xlab='Freight Speed', ylab='Deterioration Rate',col='blue')
plot(Data$CURVE_DELTA_M,Data$DETERIORATION_RATE,xlab='Curvature', ylab='Deterioration Rate',col='blue')
plot(Data$MGT_M,Data$DETERIORATION_RATE,xlab='Tonnage', ylab='Deterioration Rate',col='blue')
plot(Data$Last_10_years, Data$DETERIORATION_RATE,xlab='Ties Replaced in Last 10 years', ylab='Deterioration Rate',col='blue')
plot(Data$Last_20_10_years,Data$DETERIORATION_RATE,xlab='Ties Replaced in Last 10 -20 years', ylab='Deterioration Rate',col='blue')
plot(Data$Last_30_20_years,Data$DETERIORATION_RATE,xlab='Ties Replaced in Last 20-30 years', ylab='Deterioration Rate',col='blue')
plot(Data$HAZMAT_X,Data$DETERIORATION_RATE,xlab='Hazardous Material', ylab='Deterioration Rate',col='blue')
plot(Data$DETERIORATION_ZONE_X,Data$DETERIORATION_RATE,xlab='Deterioration Zone', ylab='Deterioration Rate',col='blue')

MAPE = function(a,b){
  return(mean(abs((b-a)/a)))
}
# Data_New  =subset(Data,Data$DETERIORATION_RATE > -0.5)
# plot(Data_New$FREIGHT_SPEED_M,Data_New$DETERIORATION_RATE)
# plot(Data_New$CURVE_DELTA_SCORE_M,Data_New$DETERIORATION_RATE)
# plot(Data_New$CURVE_DELTA_M,Data_New$DETERIORATION_RATE)
# plot(Data_New$MGT_M,Data_New$DETERIORATION_RATE)
# plot(Data_New$Last_10_years, Data_New$DETERIORATION_RATE)
# plot(Data_New$Last_20_10_years,Data_New$DETERIORATION_RATE)
# plot(Data_New$Last_30_20_years,Data_New$DETERIORATION_RATE)
# plot(Data_New$Weighted_Age,Data_New$DETERIORATION_RATE)



# plot(Datai$Weighted_Age, Datai$Det_Perc)
# plot(Data$TRACK_TYPE_C,Data$Det_Perc)
# plot(Data$HAZMAT_X,Data$Det_Perc)
# plot(Data$DETERIORATION_ZONE_X,Data$Det_Perc)
# plot(Data$Bad_Ties_EX,Data$Det_Perc)
# plot(Data$Good_Ties_EX,Data$Det_Perc)
# plot(Data$Total_Ties_EX,Data$Det_Perc)
# 

FinalTest = subset(Data, Data$YEAR_I==2016)
Train=subset(Data, Data$YEAR_I!=2016)
dim(FinalTest)

# Train=(Data[Data$YEAR_I %in% c(2013,2014),])
dim(Train)
colnames(Train)

num_cols1= c(
  "FREIGHT_SPEED_M"                                   
  ,"CURVE_DELTA_SCORE_M"
  ,"CURVE_DELTA_M"                                 
  ,"MGT_M",
  "Last_10_years",                
  "Last_20_10_years",
  "Last_30_20_years",
  "Weighted_Age"
)
cat_cols1= c(
  "TRACK_TYPE_C"
  ,"HAZMAT_X"                       
  ,"DETERIORATION_ZONE_X"
)

# pred_cols=c("Det_Perc")
pred_cols=c("DETERIORATION_RATE")

#install.packages('stringi')
library(caret)


dummies=dummyVars(~TRACK_TYPE_C+HAZMAT_X+DETERIORATION_ZONE_X, data=Train[cat_cols1])
dummies
Train_cat=as.data.frame(predict(dummies,newdata=Train[cat_cols1]))
head(Train_cat)
FinalTest_cat=as.data.frame(predict(dummies,newdata=FinalTest[cat_cols1]))

# Train

Train_Num = Train[num_cols1]
FinalTest_Num_ = FinalTest[num_cols1]
head(Train_Num)

procValues <- preProcess(Train_Num, method = c("center", "scale"))
scale_train = predict(procValues,Train_Num)
scale_test = predict(procValues,FinalTest_Num_)


TrainX= cbind(scale_train,Train_cat)
FinalTestX = cbind(scale_test,FinalTest_cat)


Train_Predicted = Train[pred_cols]
FinalTest_Predicted = FinalTest[pred_cols]


Train_Total =cbind(Train_Predicted, TrainX)
FinalTest_Total =cbind(FinalTest_Predicted, FinalTestX)

head(Train_Total)
head(FinalTest_Total)
# write.csv(Train_Total,"Train_Total.csv")
# write.csv(FinalTest_Total,"FinalTest_Total.csv")


smp_size <- floor(0.75 * nrow(Train_Total))
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(Train_Total)), size = smp_size)

train <- Train_Total[train_ind, ]
test <- Train_Total[-train_ind, ]



# y = train$Det_Perc
y= train$DETERIORATION_RATE
param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)
# Fit the model
library(xgboost)
xgb.fit = xgboost(param=param, data = as.matrix(train[,2:20]), label = y, nrounds=2000, eta = .01, max_depth = 7, lambda=0, alpha=1,min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8)
# ?xgboost

# predict_xgboost_train = predict(xgb.fit, as.matrix(train[,2:20]))
predict_xgboost_train = predict(xgb.fit, as.matrix(train[,2:20]))
plot(y,predict_xgboost_train,xlim=c(-0.5,0), ylim=c(-0.5,0), main="XGBoost - Plot of Train Data set")
MAPE(y,predict_xgboost_train)
# MAPE_xg_train <- mean(abs((train$DETERIORATION_RATE-predict_xgboost_train_det_rate)/train$DETERIORATION_RATE))


# predict_xgboost_test = predict(xgb.fit, as.matrix(test[,2:20]))
predict_xgboost_test_det_rate = predict(xgb.fit, as.matrix(test[,2:20]))
plot(test$DETERIORATION_RATE,predict_xgboost_test_det_rate,xlim=c(-0.5,0), ylim=c(-0.5,0), main="XGBoost - Plot of Test Data set")
MAPE(test$DETERIORATION_RATE,predict_xgboost_test_det_rate)
# MAPE_xg_test <- mean(abs((test$DETERIORATION_RATE-predict_xgboost_test_det_rate)/test$DETERIORATION_RATE))
# MAPE_xg_test

predict_xgboost_2016_det_rate = predict(xgb.fit, as.matrix(FinalTest_Total))
plot(FinalTest_Total$DETERIORATION_RATE,predict_xgboost_2016_det_rate,xlim=c(-0.5,0), ylim=c(-0.5,0), main="XGBoost -Deterioration Rate - Plot of 2016 Test Data set",xlab='Actual',ylab='Predicted',col='blue')
MAPE(FinalTest_Total$DETERIORATION_RATE,predict_xgboost_2016_det_rate)
# MAPE_xg_2016 <- mean(abs((FinalTest_Total$Det_Perc-predict_xgboost_2016)/FinalTest_Total$Det_Perc))
# MAPE_xg_2016


library(e1071)
# head(train)
sv = svm(DETERIORATION_RATE~.,
         kernel='radial',
         cost=1, gamma=1, epsilon=0.1,
         data=train, ntree=200)

summary(sv)

predict_svm_train = predict(sv,train)
plot(y,predict_svm_train,xlim=c(-0.5,0), ylim=c(-0.5,0), main="XGBoost - Plot of Train Data set")
MAPE(y,predict_svm_train)
# MAPE_svm_train <- mean(abs((train$Det_Perc-predict_svm_train)/train$Det_Perc))
# MAPE_svm_train

# test['DETERIORATION_ZONE_X.ZONE_3']=0


predict_svm_test = predict(sv, test)
plot(test$DETERIORATION_RATE,predict_svm_test,xlim=c(-0.5,0), ylim=c(-0.5,0), main="SV - Plot of Test Data set")
MAPE(test$DETERIORATION_RATE,predict_svm_test)
# MAPE_svm_test <- mean(abs((test$Det_Perc-predict_svm_test)/test$Det_Perc))
# MAPE_svm_test

predict_svm_2016_det_rate = predict(sv, FinalTest_Total)
plot(FinalTest_Total$DETERIORATION_RATE,predict_svm_2016_det_rate,xlim=c(-0.5,0), ylim=c(-0.5,0), main="SV - Deterioration Rate - Plot of 2016 Test Data set",xlab='Actual',ylab='Predicted',col='blue')
MAPE(predict_svm_2016_det_rate, FinalTest_Total$DETERIORATION_RATE)
# MAPE_svm_2016 <- mean(abs((FinalTest_Total$Det_Perc-predict_svm_2016)/FinalTest_Total$Det_Perc))
# MAPE_svm_2016

####################################################
library(randomForest)
# detach("package:RRF", unload=TRUE)
rf = randomForest(DETERIORATION_RATE~.,data=train, ntree=300,mtry=10)
varImpPlot(rf)
importance(rf)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

rfFit1 <- train(DETERIORATION_RATE~., data = train, 
                 method = "rf", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
rfFit1

rf =rfFit1
# rf2 = randomForest(Det_Perc~MGT_M+CURVE_DELTA_SCORE_M+FREIGHT_SPEED_M+TRACK_TYPE_C.SG+TRACK_TYPE_C.1+TRACK_TYPE_C.2+TRACK_TYPE_C.3+TRACK_TYPE_C.4+TRACK_TYPE_C.5+TRACK_TYPE_C.SD+DETERIORATION_ZONE_X.ZONE_5+DETERIORATION_ZONE_X.ZONE_4+DETERIORATION_ZONE_X.ZONE_3+DETERIORATION_ZONE_X.ZONE_2+HAZMAT_X.N+HAZMAT_X.Y, data=train, ntree=300,mtry=10))

predict_rf_train = predict(rf,train)
plot(y,predict_rf_train,xlim=c(-0.5,0), ylim=c(-0.5,0), main="XGBoost - Plot of Train Data set")
MAPE(y,predict_rf_train)
# MAPE_rf_train <- mean(abs((train$Det_Perc-predict_rf_train)/train$Det_Perc))
# MAPE_rf_train

# test['DETERIORATION_ZONE_X.ZONE_3']=0


predict_rf_test = predict(rf, test)
plot(test$DETERIORATION_RATE,predict_rf_test,xlim=c(-0.5,0), ylim=c(-0.5,0), main="RF - Plot of Test Data set")
MAPE(test$DETERIORATION_RATE, predict_rf_test)
# MAPE_rf_test <- mean(abs((test$Det_Perc-predict_rf_test)/test$Det_Perc))
# MAPE_rf_test

predict_rf_2016_det_rate = predict(rf, FinalTest_Total)
plot(FinalTest_Total$DETERIORATION_RATE,predict_rf_2016_det_rate,xlim=c(-0.5,0), ylim=c(-0.5,0), main="RF -Deterioration Rate - Plot of 2016 Test Data set",xlab='Actual',ylab='Predicted',col='blue')
?plot
MAPE(FinalTest_Total$DETERIORATION_RATE, predict_rf_2016_det_rate)
# MAPE_rf_2016 <- mean(abs((FinalTest_Total$Det_Perc-predict_rf_2016)/FinalTest_Total$Det_Perc))
# MAPE_rf_2016

head(test)
test = cbind(test, predict_xgboost_test_det_rate,predict_svm_test,predict_rf_test)
T2=(cbind(Data[rownames(test),],predict_xgboost_test_det_rate,predict_svm_test,predict_rf_test))
write.csv(T2,"T2.csv")
#################################

Final = cbind(FinalTest, predict_rf_2016_det_rate, predict_svm_2016_det_rate, predict_xgboost_2016_det_rate)
head(Final)

st= dim(Final)[2]-2
en =dim(Final)[2]

Final$AVG_det_rate = rowMeans(Final[,st:en])
plot(Final$DETERIORATION_RATE,Final$AVG_det_rate,xlim=c(-0.5,-0.1),ylim=c(-0.5,-0.1))
MAPE(Final$DETERIORATION_RATE,Final$AVG_det_rate)

# Final$Predicted_Good_Ties = (Final$Total_Ties_EX - (Final$AVG * Final$Total_Ties_EX))
# Final$Predicted_Good_Ties = (Final$Total_Ties_EX - (Final$predict_rf_2016 * Final$Total_Ties_EX))
# Final$Difference = Final$Good_Ties_EX - Final$Predicted_Good_Ties
# hist(Final$Difference)

write.csv(Final,"June_24_CLEANED_PREDICTIONS_with_Predictions_For_Det_RATE.csv")
colnames(train)
# 
# Formula = Det_Perc~MGT_M+Weighted_Age+CURVE_DELTA_SCORE_M+FREIGHT_SPEED_M+TRACK_TYPE_C.SG+DETERIORATION_ZONE_X.ZONE_5+DETERIORATION_ZONE_X.ZONE_4+DETERIORATION_ZONE_X.ZONE_3+DETERIORATION_ZONE_X.ZONE_2+TRACK_TYPE_C.1+TRACK_TYPE_C.2+TRACK_TYPE_C.3+TRACK_TYPE_C.4+TRACK_TYPE_C.5+TRACK_TYPE_C.S7
# LM1=lm(Formula,train)
# summary(LM1)
# 
# plot(train$FREIGHT_SPEED_M,train$Det_Perc)
# FP =subset(train, train$MGT_M <5)
# dim(FP)
# plot(FP$MGT_M,FP$Det_Perc)
# min(FP$MGT_M)

