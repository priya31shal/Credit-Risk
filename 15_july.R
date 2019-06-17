data=read.csv("C:\Users\shalini.priya\Desktop\credit card\15_july\Credit_Risk_Train_data.csv",header=T,na.strings=c(""," ","NA"))
test=read.csv(file.choose())



data1=read.csv("C:/Users/shalini.priya/Desktop/credit card/july/Credit_Risk_Train_data.csv",header=T,na.strings=c(""," ","NA"))
test1=read.csv("C:/Users/shalini.priya/Desktop/credit card/july/Credit_Risk_Validate_data.csv",header=T,na.strings=c(""," ","NA"))

full1=rbind(data1,test1)
##checking missing value

sapply(data1,function(x) sum(is.na(x)))
sapply(test1, function(x) sum(is.na(x)))
sapply(full1, function(x) sum(is.na(x)))

data1=full1

##labelling data and converting for missing value replacement


levels(data1$Gender)
table(data1$Gender)
data1$Gender=as.numeric(data1$Gender,
                       levels=c("Female","Male"),
                       labels=c(1,2))


data1$Married =as.numeric(data1$Married,
                          levels=c("No","Yes"),
                          labels=c(1,2))

data1$Self_Employed=as.numeric(data1$Self_Employed,
                              levels=c("Yes","No"),
                              labels=c(1,2))

data1$Dependents=as.numeric(data1$Dependents,
                            levels=c("0","1","2","3+"),
                            labels=c(1,2,3,4))

summary(data1)

data1$Gender[is.na(data1$Gender)] = 1

data1$Married=as.numeric(data1$Married)

hist(data1$Married)

data1$Married[is.na(data1$Married)] =1

hist(data1$Self_Employed)

data1$Self_Employed[is.na(data1$Self_Employed)] =2

hist(data1$Dependents)

data1$Dependents[is.na(data1$Dependents)] =4

hist(data1$Credit_History)

data1$Credit_History[is.na(data1$Credit_History)] =0

sapply(data1, function(x) sum(is.na(x)))




#converting na to mean

data1$LoanAmount[is.na(data1$LoanAmount)]=
  mean(data1$LoanAmount[!is.na(data1$LoanAmount)])

data1$Loan_Amount_Term[is.na(data1$Loan_Amount_Term)]=
  mean(data1$Loan_Amount_Term[!is.na(data1$Loan_Amount_Term)])

sapply(data, function(x) sum(is.na(x)))


str(data1)

summary(data1)


data1$Gender=as.factor(data1$Gender)
data1$Married=as.factor(data1$Married)
data1$Dependents=as.factor(data1$Dependents)
data1$Self_Employed=as.factor(data1$Self_Employed)
data1$Credit_History=as.factor(data1$Credit_History)
str(data1)
attach(data1)

library(caTools)

split=sample.split(data1$Loan_Status,SplitRatio = 0.625)
training_set = subset(data1,split == TRUE)
test=subset(data1,split == FALSE)



#FITTING LOGISTIC Regression to the training set

classifier =step(glm(formula = Loan_Status ~ .-Loan_ID-Loan_Amount_Term,
                     family = binomial,data=data1),direction = "backward")







summary(data1)



acc=function(model){
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}











acc(classifier)
## Prediction
test$probs = predict(classifier,test,type = 'response')
test$Predict = as.factor(ifelse(test$probs>0.70,1,0))
str(test$Predict)
table(test$Predict,test$Loan_Status)

library(caret)
confusionMatrix(test$Loan_Status,test$Predict)  ##Got error while running

library(ROCR)
library(car) 
vif(reg.model1)
library(oddsratio)
library(vcd)

#Make prediction on training data set
predictTrain=predict(classifier,test,type = "response")
#prediction functiom

ROCRpred=prediction(predictTrain,test$Loan_Status)

#performance function

ROCRpref=performance(ROCRpred,"tpr","fpr")

#plot roc curve

plot(ROCRpref)



















                                                                



