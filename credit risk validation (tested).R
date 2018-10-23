
#_______________________________________LOGISTIC_______________________________
library(Amelia)
library(ROCR)

crp<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_10.2_Credit_Risk_Train_data (1).csv",na.strings=c(""," ","NA"))
View(crp)
summary(crp)
colSums(is.na(crp))
crp<-crp[,-1]
View(crp)

missmap(crp, main="Observed vs predicted")

crp$Credit_History=as.factor(crp$Credit_History)
View(crp)
summary(crp)


#missing values imputation
crp$Gender[is.na(crp$Gender)]<- 'Male'
crp$Married[is.na(crp$Married)]<- "Yes"
crp$Dependents[is.na(crp$Dependents)]<- 0
crp$Self_Employed[is.na(crp$Self_Employed)]<-"No"
crp$LoanAmount[is.na(crp$LoanAmount)]<-median(crp$LoanAmount,na.rm=T)
crp$Loan_Amount_Term[is.na(crp$Loan_Amount_Term)]<-median(crp$Loan_Amount_Term,na.rm=T)
crp$Credit_History[is.na(crp$Credit_History)]<- 1

colSums(is.na(crp))

crptrain <-crp;


#______________________________________________
library(ggplot2)
ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#FF6666") + 
  facet_grid(.~Gender) + ggtitle("Loan Status by Gender of Applicant")

ggplot(crp, aes(x=Loan_Status))+ geom_bar(fill="#FF2222") + 
  facet_grid(.~Married) + ggtitle("Loan Status by Marital Status of Applicant")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#86DBC4") +
  facet_grid(.~Dependents) + ggtitle("Loan Status by number of Dependents of Applicant")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#E3E323") +
  facet_grid(.~Education) + ggtitle("Loan Status by Education of Applicant")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#717d7e") +
  facet_grid(.~Self_Employed) + ggtitle("Loan Status by Employment status of Applicant")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#138d75") +
  facet_grid(.~Loan_Amount_Term) + ggtitle("Loan Status by terms of Loan")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#784212") +
  facet_grid(.~Credit_History) + ggtitle("Loan Status by Credit History of Applicant")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#273746") +
  facet_grid(.~Property_Area) + ggtitle("Loan Status by Property area")

ggplot(crp, aes(x=Loan_Status)) + geom_bar(fill="#273746") +
  facet_grid(.~ApplicantIncome) + ggtitle("Loan Status by Applicant Income")

ggplot(crp, aes(x=Loan_Status, y=ApplicantIncome)) + geom_boxplot() + ggtitle("Loan Status by Applicant Income")

ggplot(crp, aes(x=Loan_Status, y=CoapplicantIncome)) + geom_boxplot() + ggtitle("Loan Status by Coapplicant")

ggplot(crp, aes(x=Loan_Status, y=LoanAmount)) + geom_boxplot() + ggtitle("Loan Status by Loan Amount")

print(ggplot(data=crp[crp$ApplicantIncome<20000,],aes(ApplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))

print(ggplot(data=crp[crp$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))

library(plyr)
crpFE<-mutate(crp,TotalIncome=ApplicantIncome+CoapplicantIncome)
print(ggplot(data=crpFE,aes(TotalIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))

contrasts(crp$Gender)
contrasts(crp$Married)
contrasts(crp$Education)
contrasts(crp$Self_Employed)
contrasts(crp$Credit_History)

modelp1<-glm(Loan_Status~.,family=binomial(link="logit"),data=crp)
summary(modelp1)
#AIC:587.45
modelp2<-glm(Loan_Status~Married+Credit_History+Property_Area,family=binomial(link="logit"),data=crp)
summary(modelp2)
#AIC: 578.62
modelp3<-glm(Loan_Status~Married+Credit_History+Property_Area+ApplicantIncome,family=binomial(link="logit"),data=crp)
summary(modelp3)
#AIC:580.61

#testing
crp_v<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings=c(""," ","NA"))
summary(crp_v)
colSums(is.na(crp_v))
View(crp_v)
crp_v<-crp_v[,-1]
View(crp_v)


crp_v$Credit_History<-as.factor(crp_v$Credit_History)
crp_v$LoanAmount[is.na(crp_v$LoanAmount)] <- median(crp_v$LoanAmount,na.rm=T)
crp_v$Loan_Amount_Term[is.na(crp_v$Loan_Amount_Term)] <- median(crp_v$Loan_Amount_Term,na.rm=T)
crp_v$Credit_History[is.na(crp_v$Credit_History)]<-1
crp_v$Self_Employed[is.na(crp_v$Self_Employed)]<-"No"
crp_v$Dependents[is.na(crp_v$Dependents)]<-0
crp_v$Gender[is.na(crp_v$Gender)]<-"Male"

#validation of our model using validation set
# if type = response is not mentioned it will take log(odd(probability)), its for backtransforming it to categorical variable
fitted.resultsp1 <- predict(modelp2,newdata=crp_v[,-12],type='response')
#Thresholding
fitted.resultsp1 <- ifelse(fitted.resultsp1 > 0.5,1,0)

#plotting auc curve

p2 <- predict(modelp2, newdata=crp_v[,-12], type="response")
pr2 <- prediction(fitted.resultsp1, crp_v[,12])
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

aucp <- performance(pr2, measure = "auc")
aucp1 <- aucp@y.values[[1]]
aucp1
#0.940281

#predicting on test set

crp_test<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_10.3_Credit_Risk_Test_data.csv",na.strings=c(""," ","NA"))
colSums(is.na(crp_test))
summary(crp_test)
View(crp_test)

crp_test$Credit_History<-as.factor(crp_test$Credit_History)

crp_test$LoanAmount[is.na(crp_test$LoanAmount)] <- median(crp_test$LoanAmount,na.rm=T)
crp_test$Loan_Amount_Term[is.na(crp_test$Loan_Amount_Term)] <- median(crp_test$Loan_Amount_Term,na.rm=T)
crp_test$Credit_History[is.na(crp_test$Credit_History)]<-1
crp_test$Self_Employed[is.na(crp_test$Self_Employed)]<-"No"
crp_test$Dependents[is.na(crp_test$Dependents)]<-0
crp_test$Gender[is.na(crp_test$Gender)]<-"Male"
colSums(is.na(crp_test))


#predict for test set
fr_test <- predict(model1,newdata=crp_test,type='response')
fr_test <- ifelse(fr_test > 0.5,1,0)
plot(fr_test)
View(fr-test)

crp_test<-crp_test[,-13]
outcome_test<-fr_test
crp_test=data.frame(crp_test,outcome_test)
View(crp_test)


table(crp_test$Gender,crp_test$outcome_test)
table(crp_test$Married,crp_test$outcome_test)
table(crp_test$Dependents,crp_test$outcome_test)
table(crp_test$Education,crp_test$outcome_test)
table(crp_test$Self_Employed,crp_test$outcome_test)
table(crp_test$Credit_History,crp_test$outcome_test)
table(crp_test$Property_Area,crp_test$outcome_test)
tapply(crp_test$ApplicantIncome,crp_test$outcome_test,mean)
tapply(crp_test$CoapplicantIncome,crp_test$outcome_test,mean)
tapply(crp_test$LoanAmount,crp_test$outcome_test,mean)
tapply(crp_test$Loan_Amount_Term,crp_test$outcome_test,mean)

#___________________________SVM______________________________________


library(e1071)

#model building
classifier<-svm(formula =Loan_Status~.,data=crp,type = 'C-classification')
summary(classifier)
classifier1<-svm(formula =Loan_Status~.,data=crp,type = 'C-classification',gamma=2,cost=4)
summary(classifier1)


# SVM based based on grid scearch
tunesvmcrp=tune(svm,Loan_Status~.,
                data=crp,
                ranges = list(gamma=2^(-1:1),cost=2^(2:9)))
summary(tunesvmcrp)


classifier2<-svm(formula =Loan_Status~.,data=crp,type = 'C-classification',kernel="linear")
summary(classifier2)
classifier3<-svm(formula =Loan_Status~.,data=crp,type = 'C-classification',kernel="sigmoid")
summary(classifier3)
classifier4<-svm(formula =Loan_Status~.,data=crp,type = 'C-classification',kernel="polynomial")
summary(classifier4)


#validation data
#validation of our model using validation set
# if type = response is not mentioned it will take log(odd(probability)), its for backtransforming it to categorical variable
fitted.resultssvmcrp1 <- predict(classifier1,newdata=crp_v[,-12])
fitted.resultssvmcrp2 <- predict(classifier2,newdata=crp_v[,-12])
fitted.resultssvmcrp3 <- predict(classifier3,newdata=crp_v[,-12])
fitted.resultssvmcrp4 <- predict(classifier4,newdata=crp_v[,-12])


#Confusion matrix
svmcf1<-table(fitted.resultssvmcrp1 , crp_v[,12])
svmcf2<-table(fitted.resultssvmcrp2 , crp_v[,12])
svmcf3<-table(fitted.resultssvmcrp3 , crp_v[,12])
svmcf4<-table(fitted.resultssvmcrp4 , crp_v[,12])


#function for accuracy for logistic radial
acc<-function(svmcf1){
  Totp<-svmcf1[2,1]+svmcf1[2,2]
  TP<-svmcf1[2,2]
  c<-TP/Totp
  c
}
acc(svmcf1)
#0.849359


#function for accuracy for logistic linear
acc<-function(svmcf1){
  Totp<-svmcf2[2,1]+svmcf2[2,2]
  TP<-svmcf2[2,2]
  c<-TP/Totp
  c
}
acc(svmcf2)
#0.93831


#function for accuracy for logistic sigmoid
acc<-function(svmcf3){
  Totp<-svmcf3[2,1]+svmcf3[2,2]
  TP<-svmcf3[2,2]
  c<-TP/Totp
  c
}
acc(svmcf3)
#0.93421


#function for accuracy for logistic polynomial
acc<-function(svmcf4){
  Totp<-svmcf4[2,1]+svmcf4[2,2]
  TP<-svmcf4[2,2]
  c<-TP/Totp
  c
}
acc(svmcf4)
#0.79452

#plotting auc curve for linear
svmp <- predict(classifier2, newdata=crp_v[,-12])
svmp <- as.numeric(svmp)
crp_v$outcome <-as.numeric(crp_v$outcome)
pr2 <- prediction(svmp, crp_v[,12])
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

aucsvmp <- performance(pr2, measure = "auc")
aucsvmp1 <- aucsvmp@y.values[[1]]
aucsvmp1
#0.87489

#predicting
#predict for test set
svmfr_test <- predict(classifier2,newdata=crp_test)
fr_test <- ifelse(fr_test > 0.5,1,0)
plot(svmfr_test)
View(svmfr_test)

svmcrp_test<-crp_test[,-13]
outcomesvm_test<-svmfr_test
svmcrp_test=data.frame(crp_test,outcomesvm_test)
View(svmcrp_test)


#_______________________NAIVE BAYES_____________________________________
library(e1071)

#model building on train data
library(e1071)
crpnaivem1 <- naiveBayes(Loan_Status~., data=crp)
dim(crpnaivem1)
summary(crpnaivem1)


#validation data

CRPNaive_pred = predict(crpnaivem1, newdata = crp_v)
cmCRPNaive = table(CRPNaive_pred, crp_v$outcome)
library(caret)
cfNaiveCRP1<-confusionMatrix(CRPNaive_pred,crp_v$outcome)
cfNaiveCRP1
#0.921

#predicting
#predict for test set
naivefr_test <- predict(crpnaivem1,newdata=crp_test)
plot(naivefr_test)
View(naivefr-test)

crp_test<-crp_test[,-13]
naiveoutcome_test<-naivefr_test
crp_test=data.frame(crp_test,naiveoutcome_test)
View(crp_test)



#_________________________________________DECESION TREES____________________________________

library(rpart)
modelCRPDT1 <- rpart(formula = Loan_Status ~., data=crp)
plot(modelCRPDT1)
text(modelCRPDT1)


#validation


CRPDT_pred = predict(modelCRPDT1, newdata = crp_v, type = 'class')
class(CRPDT_pred)


# confusion matrix
cmCRPDT = table(CRPDT_pred, crp_v$outcome)
cmCRPDT

library(caret)
cfCRPDT1<-confusionMatrix(CRPDT_pred,crp_v$outcome)
cfCRPDT1
#0.9455


#predicting 
#predict for test set
DTCRPfr_test <- predict(modelCRPDT1,newdata=crp_test,type='class')
plot(DTCRPfr_test)
View(DTCRPfr_test)

crp_test<-crp_test[,-13]
DTCRPoutcome_test<-DTCRPfr_test
crp_test=data.frame(crp_test,DTCRPoutcome_test)
View(crp_test)


#_________________________________RANDOM FOREST_______________________________

#fitting random forest classification to the training set
library(randomForest)
randomforestCRP = randomForest(x = crp[-12],y = crp$Loan_Status, ntree = 50)

#predicting the test set results
randomCRP_pred = predict(randomforestCRP,newdata = crp_v[,-12])
#making the confucion matrix
cmCRPrandom = table(crp_v$outcome,randomCRP_pred)
acc(cm)

#predict for test set
randomCRPfr_test <- predict(randomforestCRP,newdata=crp_test)
plot(randomCRPfr_test)
View(randomCRPfr_test)

crp_test<-crp_test[,-13]
randomCRPoutcome_test<-randomCRPfr_test
crp_test=data.frame(crp_test,randomCRPoutcome_test)
View(crp_test)


#____________________________________KNN____________________________________

library(dplyr)
library(lubridate)
library(caret)
library(class)

crp <- na.omit(crp)
crp_v<- na.omit(crp_v)
crp$Loan_Status<-na.omit(crp$Loan_Status)



crp$Loan_Status=
accuracy <- 0
k <- 5:40


for(i in k){
  CRPkpredict <- knn(crp,crp_v,crp$Loan_Status,k=i)
  crpkcf<- table(CRPkpredict,crp_v$outcome)
  accuracy[i]<-acc(crpkcf)
}
CRPkpredict <- knn(crp,crp_v,crp$Loan_Status,k=5)

colSums(is.na(crp))
colSums(is.na(crp_v))
summary(crp)
crpa<- data.frame(accuracy,1:40)
crpa<- na.omit(crpa)
View(crpa)
crpa<- a[-1,]
max(a$accuracy)



#prediction
#predictors <- cbind(lag(stocks$Apple,default =210.73), lag(stocks$Google))
CRPKNNprediction <- knn(crp,crp_v,crp$Loan_Status,k=XX)


CRPKNNcf=table(CRPKNNprediction,crp_v$outcome)
acc<-function(CRPKNNcf){
  Totp<-CRPKNNcf[2,1]+CRPKNNcf[2,2]
  TP<-CRPKNNcf[2,2]
  c<-TP/Totp
  c
}
acc(CRPKNNcf)


library(lattice)
library(caret)

#repeats cross validation 3 times on train train
CRPKNNctr=trainControl(method="repeatedcv",repeats=3)
crp$Loan_Status<-as.factor(crp$Loan_Status)

CRPknnFit<-train(Loan_Status ~., data=crp,method="knn",trControl = CRPKNNctr, preProcess = c("center","scale"),tuneLength = XX)

#__________________NEURAL NETWORK_______________

#use scale() and convert the resulting matrix to a data frame
CRPscaled.data <- as.data.frame(crp[,1:11])

#check out results
print (head(CRPscaled.data,2))
View(crp)
colSums(is.na(CRPscaled.data))


#splitting
#convert Private column from YES/NO to 1/0
CRPloanstatus = as.numeric(crp$Loan_Status)-1
CRPdata = cbind(CRPloanstatus,CRPscaled.data)
View(CRPdata)

str(CRPdata)
library(caTools)
set.seed(101)


#create split (any column is fine)
split = sample.split(data$Private,SplitRatio = 0.70)

#split based off of split Boolean vector
train = subset(data,split == TRUE)
test= subset(data,split==FALSE)


#neural networks model building
CRPfeats <- names(CRPdata)

#Concatenate strings
CRPF <- paste(CRPfeats,collapse = '+')
CRPF <- paste('CRPloanstatus ~',CRPF)

#convert to formula
CRPF <- as.formula(CRPF)



str(crp)
library(neuralnet)
CRPnn <- neura
lnet(CRPF,CRPdata,hidden= 4,linear.output = FALSE,constant.weights = NULL)
plot(CRPnn)

#computePrediction off Test set
View(crp)
predicted.CRPnn.values <- compute(CRPnn,test[,2:!2])

# check not net.result
print(head(predicted.CRPnn.valuess$Loan_Status))


predicted.CRPnn.valuess$Loan_Status <- sapply(predicted.CRPnn.valuess$Loan_Status,round,digits=0)


#confusion matrix
CRPcf <- table(crp_v$outcome,predicted.CRPnn.valuess$Loan_Status)
acc(CRPcf)

#visualizing neural network
plot(CRPnn)






