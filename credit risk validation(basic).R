#We have used lm() to fit our regression models.
#lm is limited in particular it fits only models for continuous dependnet variables
#for categorical dependent variables we will use glm()

library(Amelia)
# Plots for missing values
library(ROCR)

#read data in R
cr<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_10.2_Credit_Risk_Train_data (1).csv")
View(cr)
#Identifying missing values
colSums(is.na(cr))
summary(cr)
#notice blanks were not changed to NA while reading. 
#so change the above code using c(""," ")
cr<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_10.2_Credit_Risk_Train_data (1).csv",na.strings=c(" ","","NA"))
#run the read.csv with na.strings
#data processing
View(cr)
head(cr)
colSums(is.na(cr))
#assign loanid as rownames and remove it from the dataset
#Using a particular column as row names
rownames(cr)<-cr[,1]
cr<-cr[,-1]
View(cr)

#Visual take on missing values: plot the dataset and highlight missing values
#Ameila
missmap(cr, main = "Missing values vs observed")
#check for the type of variables whihc has missing values
summary(cr)

#convert categorical variable into factor
cr$Credit_History=as.factor(cr$Credit_History)

#imputation
cr$LoanAmount[is.na(cr$LoanAmount)] <- median(cr$LoanAmount,na.rm=T)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)] <- median(cr$Loan_Amount_Term,na.rm=T)
cr$Credit_History[is.na(cr$Credit_History)]<-1
cr$Self_Employed[is.na(cr$Self_Employed)]<-'No'
cr$Dependents[is.na(cr$Dependents)]<-0
cr$Gender[is.na(cr$Gender)]<-'Male'
cr$Married[is.na(cr$Married)]<-'Yes'

colSums(is.na(cr))

boxplot(cr[,6:9])


#Missing value imputation
#mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#function for mode : this will give me the category with maximum frequency
mode<-cr$Credit_History[max(table(cr$Credit_History))]
mode
#imputation
cr$LoanAmount[is.na(cr$LoanAmount)] <- median(cr$LoanAmount,na.rm=T)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)] <- median(cr$Loan_Amount_Term,na.rm=T)
cr$Credit_History[is.na(cr$Credit_History)]<-mode(cr$Credit_History)
cr$Self_Employed[is.na(cr$Self_Employed)]<-mode(cr$Self_Employed)
cr$Dependents[is.na(cr$Dependents)]<-mode(cr$Dependents)
cr$Gender[is.na(cr$Gender)]<-mode(cr$Gender)
cr$Married[is.na(cr$Married)]<-mode(cr$Married)
#similarly impute for gender, married, dependents, self employed

#again check if there are any missing values in the data
colSums(is.na(cr)) 
#no missing values found




#For a better understanding of how R is going to deal with the categorical variables, we can use the contrasts() function
contrasts(cr$Gender)
contrasts(cr$Married)
contrasts(cr$Education)
contrasts(cr$Self_Employed)
contrasts(as.factor(cr$Credit_History))

#Splitting: which is already done. we are fitting the model on the train set only
#Model fitting
model1 <- glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Credit_History+Property_Area,family=binomial(link='logit'),data=cr)
summary(model1)
model2<-glm(Loan_Status~Married+Property_Area+Credit_History,family=binomial(link='logit'),data=cr)
summary(model2)

#male reduces the log odds by 2.75 while a unit increase in age reduces the log odds by 0.037.

#anova(model1,test='Chisq')
#assessing the predictive ability of the model
#read the test model


#cleaning for data issues
#function for mode : this will give me the category with maximum frequency
cr_v<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings=c(""," ","NA"))
#check for data issues with testa nd clean it for the same
colSums(is.na(cr_v))
View(cr_v)

#cr<-cr[,-1]

cr_v$Credit_History<-as.factor(cr_v$Credit_History)

cr_v$LoanAmount[is.na(cr_v$LoanAmount)] <- mean(cr_v$LoanAmount,na.rm=T)
cr_v$Loan_Amount_Term[is.na(cr_v$Loan_Amount_Term)] <- mean(cr_v$Loan_Amount_Term,na.rm=T)
cr_v$Credit_History[is.na(cr_v$Credit_History)]<-mode(cr_v$Credit_History)
cr_v$Self_Employed[is.na(cr_v$Self_Employed)]<-mode(cr_v$Self_Employed)
cr_v$Dependents[is.na(cr_v$Dependents)]<-mode(cr_v$Dependents)
cr_v$Gender[is.na(cr_v$Gender)]<-mode(cr_v$Gender)
#similarly impute for gender, married, dependents, self employed

#validation of our model using validation set
# if type = response is not mentioned it will take log(odd(probability)), its for backtransforming it to categorical variable
fitted.results1 <- predict(model1,newdata=cr_v[,-13],type='response')
#Thresholding
fitted.results1 <- ifelse(fitted.results1 > 0.5,1,0)


#Confusion matrix
cf1<-table(fitted.results1 , cr_v[,13])
#function for accuracy
acc<-function(cf1){
  Totp<-cf1[2,1]+cf1[2,2]
  TP<-cf1[2,2]
  c<-TP/Totp
  c
}
acc(cf1)

#ROC curve
#ROCR package
#As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
#a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.

#plotting auc curve
p <- predict(model1, newdata=cr_v[,-13], type="response")
pr <- prediction(p, cr_v[,13])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#obtaining area under ROC curve
auc <- performance(pr, measure = "auc")
auc1 <- auc@y.values[[1]]
auc1

cr_test<-read.csv("D:/imarticus/R/logistic regression/Logistic regression and SVM_Case study/R_Module_Day_10.3_Credit_Risk_Test_data.csv",na.strings=c(""," ","NA"))
colSums(is.na(cr_test))
View(cr_v)

cr_test$Credit_History<-as.factor(cr_test$Credit_History)

cr_test$LoanAmount[is.na(cr_v$LoanAmount)] <- mean(cr_v$LoanAmount,na.rm=T)
cr_test$Loan_Amount_Term[is.na(cr_v$Loan_Amount_Term)] <- mean(cr_v$Loan_Amount_Term,na.rm=T)
cr_test$Credit_History[is.na(cr_v$Credit_History)]<-mode(cr_v$Credit_History)
cr_test$Self_Employed[is.na(cr_v$Self_Employed)]<-mode(cr_v$Self_Employed)
cr_test$Dependents[is.na(cr_v$Dependents)]<-mode(cr_v$Dependents)
cr_v$Gender[is.na(cr_v$Gender)]<-mode(cr_v$Gender)

#predict for test set
fr_p <- predict(model1,newdata=cr_test,type='response')
fr_p <- ifelse(fr_p > 0.5,1,0)
plot(fr_p)
View(fr-P)
logl<-log(LGD[,6])
#LGD=data.frame(LGD,logl)

table(cr_test$Gender,cr_test$)
tapply(cr_test$)