library(caret)
library(randomForest)
RF_data=read.csv("/home/pritam/Desktop/R/rating_final2.csv", header = T, colClasses="factor")
str(RF_data)


split_data=sample(2, nrow(RF_data), replace = T, prob=c(0.7,03))
train_data=RF_data[split_data==1,]
test_data=RF_data[split_data==2,]

db_rf=randomForest(service_rating~food_rating+rating+delay,data=train_data)
print(db_rf)

db_pred=predict(db_rf,data=train_data)
db_pred_test=predict(db_rf,newdata=test_data)

confusionMatrix(db_pred, train_data$service_rating)
confusionMatrix(db_pred_test,test_data$service_rating)  


importance(db_rf)

varUsed(db_rf)

