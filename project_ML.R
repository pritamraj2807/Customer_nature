#Importing library
library(party)
install.packages("Hmisc")

#install.packages("corrplot")
library(corrplot)
library(Hmisc)
#Reading data
rating_datase=read.csv("/home/pritam/Desktop/R/rating_final2.csv")
str(rating_datase)

rating_datasett <- na.omit(rating_datase)
#Converting to NO and Yes values

rating_datasett$delay[rating_datasett$delay =="no"]<-as.numeric(0)
rating_datasett$delay[rating_datasett$delay =="yes"]<-as.numeric(1)
#rating_dataset$delay=factor(rating_dataset$delay)
str(rating_datasett)

rating_datasett <- as.data.frame(apply(rating_datasett, 2, as.numeric))
sapply(rating_datasett, class)

freq=table(rating_dataset$userID)
plot(freq)

mydata = rcorr(as.matrix(rating_dataset))
mydata

mydat = cor(as.matrix(rating_datasett))
mydat
#splitting data into training and testing
pd=sample(2,nrow(rating_dataset), replace = T, prob=c(0.7,0.3))
train_data=rating_dataset[pd==1,]
test_data=rating_dataset[pd==2,]



#decision tree
output_tree=ctree(service_rating~rating+food_rating+delay+refund_request, data=train_data)
output_tree
plot(output_tree)

#prediction using test data
predict(output_tree,  test_data, type = "prob")
test_pred=predict(output_tree, test_data)

#confusion matrix
tab=table(predict(output_tree), train_data$service_rating)
print (tab)

#Accuracy of training data
sum(diag(tab)/sum(tab))
#misclassification error
1-sum(diag(tab)/sum(tab))

#accuracy of testing data
test_pred=predict(output_tree, newdata=test_data)
tab=table(test_pred,test_data$service_rating)
tab
sum(diag(tab)/sum(tab))
1-sum(diag(tab)/sum(tab))



  
