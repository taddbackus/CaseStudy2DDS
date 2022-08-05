library(aws.s3)
library(RCurl)
library(class)
library(caret)
library(e1071)
library(tidyr)
library(tidyverse)
library(olsrr)
library(forecast)

options(scipen=999)
attrData = read.csv('/Users/taddbackus/School/summer22/doingDataScience/caseStudy2/CaseStudy2-data.csv',header=TRUE)
attrTest = read.csv('/Users/taddbackus/School/summer22/doingDataScience/caseStudy2/CaseStudy2CompSet No Attrition.csv',header=TRUE)
miTest = read.csv('/Users/taddbackus/School/summer22/doingDataScience/caseStudy2/CaseStudy2CompSet No Salary.csv',header=TRUE)

aDF = attrData
aDF$Department = factor(aDF$Department)
aDF$EducationField = factor(aDF$EducationField)
aDF$BusinessTravel = factor(aDF$BusinessTravel)
aDF$Gender = factor(aDF$Gender)
aDF$JobRole = factor(aDF$JobRole)
aDF$MaritalStatus = factor(aDF$MaritalStatus)
aDF$OverTime = factor(aDF$OverTime)
aDF$Attrition = factor(aDF$Attrition)

attrY <- aDF %>% filter(Attrition=='Yes')
attrN <- aDF %>% filter(Attrition=='No')
################################################################################
# Attrition Analysis
################################################################################
# Job Role/Department
ggplot(aDF,aes(x=Department,fill=Attrition))+
  geom_bar()+
  coord_flip()+
  ggtitle('Attrition by Department')+
  ylab('Count')+
  xlab('Department')

summary(attrY$Department)
summary(attrN$Department)

ggplot(aDF,aes(x=JobRole,fill=Attrition))+
  geom_bar()+
  coord_flip()+
  ggtitle('Attrition by Job Role')+
  ylab('Count')+
  xlab('Job Role')
summary(attrY$JobRole)
summary(attrN$JobRole)

ggplot(aDF,aes(x=EducationField,y=JobRole,color=Attrition))+
  geom_point(position='jitter')+
  ggtitle('Attrition of Job Roles by Education Field')+
  xlab('Education Field')+
  ylab('Job')

# Monthly Income
ggplot(aDF,aes(x=log(MonthlyIncome),y=Attrition,fill=Attrition))+
  geom_boxplot()+
  ggtitle('Attrition based on Monthly Income')+
  xlab('Monthly Income')

ggplot(aDF,aes(x=log(MonthlyIncome),fill=Attrition))+
  geom_histogram(bins=20)+
  facet_wrap(~Attrition)+
  ggtitle('Histogram of Monthly Incomes')+
  xlab('Monthly Income')+
  ylab('Count')

t.test(x=log(attrY$MonthlyIncome), y=log(attrN$MonthlyIncome), conf.int=.95, var.equal=TRUE, alternative="two.sided")

# Over Time
ggplot(aDF,aes(x=OverTime,fill=Attrition))+
  geom_bar()+
  ggtitle('Attrition for Over Time workers')+
  xlab('Count')+
  ylab('Over Time status')

summary(attrY$OverTime)
summary(attrN$OverTime)
################################################################################
# Job Role Analysis
################################################################################
# Monthly Income
ggplot(aDF,aes(x=MonthlyIncome,y=JobRole,fill=Attrition))+
  geom_boxplot()+
  ggtitle('Monthly Income for Job Roles')+
  xlab('Monthly Income')+
  ylab('Job Role')

# Job Satisfaction
ggplot(aDF,aes(x=JobSatisfaction,y=JobRole,color=Attrition))+
  geom_point(position='jitter')+
  ggtitle('Job Role and Satisfaction')+
  xlab('Job Satisfaction')+
  ylab('Job Role')

################################################################################
# Attrition Model
################################################################################
attrModel = attrTest
happyKnn = subset(aDF, select=c(Attrition,Age,DistanceFromHome,EnvironmentSatisfaction,
                                JobSatisfaction,RelationshipSatisfaction,WorkLifeBalance))
payKnn = subset(aDF, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,MonthlyRate,
                              PercentSalaryHike,PerformanceRating,StockOptionLevel))
yearKnn = subset(aDF, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,JobSatisfaction,
                               NumCompaniesWorked,TotalWorkingYears,TrainingTimesLastYear,
                               YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                               YearsWithCurrManager))
happyNB = subset(aDF, select=c(Attrition,Age,DistanceFromHome,
                               EnvironmentSatisfaction,JobSatisfaction,
                               RelationshipSatisfaction,WorkLifeBalance,
                               Gender,JobRole,OverTime,MaritalStatus))
payNB = subset(aDF, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,
                             MonthlyRate,PercentSalaryHike,PerformanceRating,
                             StockOptionLevel,Department,EducationField,
                             Gender,JobRole))
yearNB = subset(aDF, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,
                              JobSatisfaction,NumCompaniesWorked,TotalWorkingYears,
                              TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,
                              YearsSinceLastPromotion,YearsWithCurrManager))

happyKnnTest = subset(attrModel, select=c(Age,DistanceFromHome,EnvironmentSatisfaction,
                                JobSatisfaction,RelationshipSatisfaction,WorkLifeBalance))
payKnnTest = subset(attrModel, select=c(Age,DailyRate,HourlyRate,MonthlyIncome,MonthlyRate,
                              PercentSalaryHike,PerformanceRating,StockOptionLevel))
yearKnnTest = subset(attrModel, select=c(Age,Education,JobLevel,JobInvolvement,JobSatisfaction,
                               NumCompaniesWorked,TotalWorkingYears,TrainingTimesLastYear,
                               YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                               YearsWithCurrManager))
happyNBTest = subset(attrModel, select=c(Age,DistanceFromHome,
                               EnvironmentSatisfaction,JobSatisfaction,
                               RelationshipSatisfaction,WorkLifeBalance,
                               Gender,JobRole,OverTime,MaritalStatus))
payNBTest = subset(attrModel, select=c(Age,DailyRate,HourlyRate,MonthlyIncome,
                             MonthlyRate,PercentSalaryHike,PerformanceRating,
                             StockOptionLevel,Department,EducationField,
                             Gender,JobRole))
yearNBTest = subset(attrModel, select=c(Age,Education,JobLevel,JobInvolvement,
                              JobSatisfaction,NumCompaniesWorked,TotalWorkingYears,
                              TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,
                              YearsSinceLastPromotion,YearsWithCurrManager))

happyKnnMod = train(happyKnn[,2:7],happyKnn$Attrition,method='knn')
payKnnMod = train(payKnn[,2:9],payKnn$Attrition,method='knn')
yearKnnMod = train(yearKnn[,2:13],yearKnn$Attrition,method='knn')

happyNBMod = naiveBayes(happyNB[,2:11],happyNB$Attrition)
payNBMod = naiveBayes(payNB[,2:13],payNB$Attrition)
yearNBMod = naiveBayes(yearNB[,2:13],yearNB$Attrition)

attrModel$happyKnnPred = predict(object=happyKnnMod,happyKnnTest[,1:6])
attrModel$payKnnPred = predict(object=payKnnMod,payKnnTest[,1:8])
attrModel$yearKnnPred = predict(object=yearKnnMod,yearKnnTest[,1:12])
attrModel$happyKnnProb = predict(object=happyKnnMod,happyKnnTest[,1:6],type='prob')
attrModel$payKnnProb = predict(object=payKnnMod,payKnnTest[,1:8],type='prob')
attrModel$yearKnnProb = predict(object=yearKnnMod,yearKnnTest[,1:12],type='prob')

attrModel$happyNBPred = predict(happyNBMod,happyNBTest[,1:10])
attrModel$payNBPred = predict(payNBMod,payNBTest[,1:12])
attrModel$yearNBPred = predict(yearNBMod,yearNBTest[,1:12])
attrModel$happyNBProb = predict(happyNBMod,happyNBTest[,1:10],'raw')
attrModel$payNBProb = predict(payNBMod,payNBTest[,1:12],'raw')
attrModel$yearNBProb = predict(yearNBMod,yearNBTest[,1:12],'raw')

attrModel$probAvgNB = (attrModel$happyNBProb[,'Yes']+attrModel$payNBProb[,'Yes']+attrModel$yearNBProb[,'Yes'])/3
attrModel$probWeightNB = attrModel$happyNBProb[,'Yes']*.4+attrModel$payNBProb[,'Yes']*.2+attrModel$yearNBProb[,'Yes']*.4

attrModel$probAvgKnn = (attrModel$happyKnnProb$Yes+attrModel$payKnnProb$Yes+attrModel$yearKnnProb$Yes)/3
attrModel$probWeightKnn = attrModel$happyKnnProb$Yes*.6+attrModel$payKnnProb$Yes*.2+attrModel$yearKnnProb$Yes*.2

attrModel$probAvg = (attrModel$probAvgNB+attrModel$probAvgKnn)/2
attrModel$probWeight = (attrModel$probWeightNB+attrModel$probWeightKnn)/2

attrModel$predAvg = as.factor(ifelse(attrModel$probAvg>0.5,'Yes','No'))
attrModel$predWeight = as.factor(ifelse(attrModel$probWeight>0.5,'Yes','No'))

classPred = subset(attrModel, select=c(ID,predWeight))
write.csv(classPred,'/Users/taddbackus/School/summer22/doingDataScience/caseStudy2/Case2PredictionsBackus Attrition.csv',row.names = FALSE)


################################################################################
# Training
iterations = 200
splitPerc = .6
avgResults = data.frame(acc=numeric(200),sensit=numeric(200),specif=numeric(200),iter=numeric(200))
weightResults = data.frame(acc=numeric(200),sensit=numeric(200),specif=numeric(200),iter=numeric(200))
for(i in 1:iterations)
{
  tSet = sample(1:dim(aDF)[1],round(splitPerc*dim(aDF)[1]))
  setTrain = aDF[tSet,]
  setTest = aDF[-tSet,]
  
  setTrainHappyKnn = subset(setTrain, select=c(Attrition,Age,DistanceFromHome,EnvironmentSatisfaction,
                                            JobSatisfaction,RelationshipSatisfaction,WorkLifeBalance))
  setTestHappyKnn = subset(setTest, select=c(Attrition,Age,DistanceFromHome,EnvironmentSatisfaction,
                                          JobSatisfaction,RelationshipSatisfaction,WorkLifeBalance))
  setTrainPayKnn = subset(setTrain, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,MonthlyRate,
                                          PercentSalaryHike,PerformanceRating,StockOptionLevel))
  setTestPayKnn = subset(setTest, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,MonthlyRate,
                                        PercentSalaryHike,PerformanceRating,StockOptionLevel))
  setTrainYearKnn = subset(setTrain, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,JobSatisfaction,
                                           NumCompaniesWorked,TotalWorkingYears,TrainingTimesLastYear,
                                           YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                                           YearsWithCurrManager))
  setTestYearKnn = subset(setTest, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,JobSatisfaction,
                                         NumCompaniesWorked,TotalWorkingYears,TrainingTimesLastYear,
                                         YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                                         YearsWithCurrManager))
  setTrainHappyNB = subset(setTrain, select=c(Attrition,Age,DistanceFromHome,
                                              EnvironmentSatisfaction,JobSatisfaction,
                                              RelationshipSatisfaction,WorkLifeBalance,
                                              Gender,JobRole,OverTime,MaritalStatus))
  setTestHappyNB = subset(setTest, select=c(Attrition,Age,DistanceFromHome,
                                            EnvironmentSatisfaction,JobSatisfaction,
                                            RelationshipSatisfaction,WorkLifeBalance,
                                            Gender,JobRole,OverTime,MaritalStatus))
  setTrainPayNB = subset(setTrain, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,
                                            MonthlyRate,PercentSalaryHike,PerformanceRating,
                                            StockOptionLevel,Department,EducationField,
                                            Gender,JobRole))
  setTestPayNB = subset(setTest, select=c(Attrition,Age,DailyRate,HourlyRate,MonthlyIncome,
                                          MonthlyRate,PercentSalaryHike,PerformanceRating,
                                          StockOptionLevel,Department,EducationField,
                                          Gender,JobRole))
  setTrainYearNB = subset(setTrain, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,
                                             JobSatisfaction,NumCompaniesWorked,TotalWorkingYears,
                                             TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,
                                             YearsSinceLastPromotion,YearsWithCurrManager))
  setTestYearNB = subset(setTest, select=c(Attrition,Age,Education,JobLevel,JobInvolvement,
                                           JobSatisfaction,NumCompaniesWorked,TotalWorkingYears,
                                           TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,
                                           YearsSinceLastPromotion,YearsWithCurrManager))
  
  happyNBMod = naiveBayes(setTrainHappyNB[,2:11],setTrainHappyNB$Attrition)
  payNBMod = naiveBayes(setTrainPayNB[,2:13],setTrainPayNB$Attrition)
  yearNBMod = naiveBayes(setTrainYearNB[,2:13],setTrainYearNB$Attrition)
  
  setTest$happyNBPred = predict(happyNBMod,setTestHappyNB[,2:11])
  setTest$payNBPred = predict(payNBMod,setTestPayNB[,2:13])
  setTest$yearNBPred = predict(yearNBMod,setTestYearNB[,2:13])
  setTest$happyNBProb = predict(happyNBMod,setTestHappyNB[,2:11],'raw')
  setTest$payNBProb = predict(payNBMod,setTestPayNB[,2:13],'raw')
  setTest$yearNBProb = predict(yearNBMod,setTestYearNB[,2:13],'raw')
  
  setTest$probAvg = (setTest$happyNBProb[,'Yes']+setTest$payNBProb[,'Yes']+setTest$yearNBProb[,'Yes'])/3
  setTest$probWeight = setTest$happyNBProb[,'Yes']*.4+setTest$payNBProb[,'Yes']*.2+setTest$yearNBProb[,'Yes']*.4
  setTest$predAvg = as.factor(ifelse(setTest$probAvg>.5,'Yes','No'))
  setTest$predWeight = as.factor(ifelse(setTest$probWeight>.5,'Yes','No'))
  
  happyKnnMod = train(setTrainHappyKnn[,2:7],setTrainHappyKnn$Attrition,method='knn')
  payKnnMod = train(setTrainPayKnn[,2:9],setTrainPayKnn$Attrition,method='knn')
  yearKnnMod = train(setTrainYearKnn[,2:13],setTrainYearKnn$Attrition,method='knn')
  
  setTest$happyKnnPred = predict(object=happyKnnMod,setTestHappyKnn[,2:7])
  setTest$payKnnPred = predict(object=payKnnMod,setTestPayKnn[,2:9])
  setTest$yearKnnPred = predict(object=yearKnnMod,setTestYearKnn[,2:13])
  setTest$happyKnnProb = predict(object=happyKnnMod,setTestHappyKnn[,2:7],type='prob')
  setTest$payKnnProb = predict(object=payKnnMod,setTestPayKnn[,2:9],type='prob')
  setTest$yearKnnProb = predict(object=yearKnnMod,setTestYearKnn[,2:13],type='prob')
  
  setTest$probAvgNB = (setTest$happyNBProb[,'Yes']+setTest$payNBProb[,'Yes']+setTest$yearNBProb[,'Yes'])/3
  setTest$probWeightNB = setTest$happyNBProb[,'Yes']*.4+setTest$payNBProb[,'Yes']*.2+setTest$yearNBProb[,'Yes']*.4
  
  setTest$probAvgKnn = (setTest$happyKnnProb$Yes+setTest$payKnnProb$Yes+setTest$yearKnnProb$Yes)/3
  setTest$probWeightKnn = setTest$happyKnnProb$Yes*.6+setTest$payKnnProb$Yes*.2+setTest$yearKnnProb$Yes*.2
  
  setTest$probAvg = (setTest$probAvgNB + setTest$probAvgKnn)/2
  setTest$probWeight = (setTest$probWeightNB + setTest$probWeightKnn)/2
  
  setTest$predAvg = as.factor(ifelse(setTest$probAvg>0.5,'Yes','No'))
  setTest$predWeight = as.factor(ifelse(setTest$probWeight>0.5,'Yes','No'))
  
  avgCM = confusionMatrix(table(setTest$Attrition,setTest$predAvg))
  weightCM = confusionMatrix(table(setTest$Attrition,setTest$predWeight))
  
  avgResults$acc[i] = avgCM$overall[1]
  avgResults$sensit[i] = avgCM$byClass[1]
  avgResults$specif[i] = avgCM$byClass[2]
  avgResults$iter[i] = i
  weightResults$acc[i] = weightCM$overall[1]
  weightResults$sensit[i] = weightCM$byClass[1]
  weightResults$specif[i] = weightCM$byClass[2]
  weightResults$iter[i] = i
  
}

ggplot(avgResults,aes(x=iter))+
  geom_line(aes(y=acc),color='darkred')+
  geom_line(aes(y=sensit),color='steelblue')+
  geom_line(aes(y=specif),color='forestgreen')+
  ylim(0,1)
ggplot(weightResults,aes(x=iter))+
  geom_line(aes(y=acc),color='darkred')+
  geom_line(aes(y=sensit),color='steelblue')+
  geom_line(aes(y=specif),color='forestgreen')+
  ylim(0,1)
ggplot(weightResults,aes(x=iter))+
  geom_smooth(aes(y=acc),color='darkred',se=FALSE)+
  geom_smooth(aes(y=sensit),color='steelblue',se=FALSE)+
  geom_smooth(aes(y=specif),color='forestgreen',se=FALSE)+
  ylim(0,1)+
  ggtitle('Results of training')+
  xlab('Iterations')+
  ylab('Score')
summary(avgResults)
summary(weightResults)
ggplot(avgResults,aes(x=iter,y=acc))+
  geom_point()
################################################################################
# Monthly Income Model
################################################################################
miModel = miTest
fullFit = lm(MonthlyIncome~JobLevel+JobRole+TotalWorkingYears+DistanceFromHome+
               Age+PerformanceRating+YearsAtCompany+YearsSinceLastPromotion+OverTime,data=aDF)
MonthlyIncome = predict(fullFit,newdata=miModel)
miPred = cbind(miModel,MonthlyIncome)
miPred = subset(miPred,select=c(ID,MonthlyIncome))
write.csv(miPred,'/Users/taddbackus/School/summer22/doingDataScience/caseStudy2/Case2PredictionBackus Salary.csv',row.names = FALSE)

CV(fullFit)
sqrt(mean(fullFit$residuals^2))
aDF$lmPred = fullFit$fitted.values
ggplot(aDF,aes(x=lmPred,y=MonthlyIncome))+
  geom_point()+
  geom_smooth(method='lm')+
  ggtitle('Linear Regression for Monthly Income')+
  xlab('Predicted Values')+
  ylab('Monthly Income')


