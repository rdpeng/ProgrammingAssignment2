#Question 1

# 

library(dplyr)
d <- read.csv("bank-full.csv",sep = ";",stringsAsFactors = FALSE)
glimpse(d)
str(d)

## grouping and dummy creation
## create Dummy Veriable

d.new=d
str(d.new)
sort(table(d.new$job.admin),decending = true)
d.new=d.new %>%
        mutate(job.bluecoll=as.numeric(job=="blue-collar"),
               job.management=as.numeric(job=="management"),
               job.technician=as.numeric(job=="technician"),
               job.admin=as.numeric(job=="admin."),
               job.services=as.numeric(job=="services"),
               job.retired=as.numeric(job=="retired"),
               job.selfemp =as.numeric(job=="self-employed"),
               job.entrepreneur=as.numeric(job=="entrepreneur"),
               job.unemployed=as.numeric(job=="unemployed"),
               job.housemaid=as.numeric(job=="housemaid"),
               job.student=as.numeric(job=="student"))%>%
        select(-job)
str(d.new)
sort(table(d.new$marital),decending = true)

d.new=d.new %>%
        mutate(marital.married=as.numeric(marital=="married"),
               marital.single=as.numeric(marital=="single"))%>%
        select(-marital)

str(d.new)

sort(table(d.new$education),decending = true)

d.new=d.new %>%
        mutate(edu.secondary=as.numeric(education=="secondary"),
               edu.tertiary=as.numeric(education=="tertiary"),
               edu.primary=as.numeric(education=="primary"))%>%
        select(-education)
str(d.new)
sort(table(d.new$default),decending = true)

d.new=d.new %>%
        mutate(default.no=as.numeric(default== "no"))%>%
        select(-default)
str(d.new)
sort(table(d.new$housing),decending = true)

d.new=d.new %>%
        mutate(housing.yes=as.numeric(housing== "yes"))%>%
        select(-housing)

str(d.new)
sort(table(d.new$loan),decending = true)

d.new=d.new %>%
        mutate(loan.no=as.numeric(loan== "no"))%>%
        select(-loan)

str(d.new)
sort(table(d.new$contact),decending = true)

d.new=d.new %>%
        mutate(contact.phone=as.numeric(contact== "cellular"),
               contact.unknown=as.numeric(contact== c("unknown")))%>%
        select(-contact)

str(d.new)
sort(table(d.new$month),decending = true)

d.new=d.new %>%
        mutate(month.may=as.numeric(month=="may"),
               month.jul=as.numeric(month=="jul"),
               month.aug =as.numeric(month=="aug"),
               month.jun=as.numeric(month=="jun"),
               month.nov=as.numeric(month=="nov"),
               month.apr=as.numeric(month=="apr"),
               month.feb=as.numeric(month=="feb"),
               month.jan=as.numeric(month=="jan"),
               month.oct=as.numeric(month=="oct"),
               month.sep=as.numeric(month=="sep"),
               month.mar=as.numeric(month=="mar"))%>%
        select(-month)

str(d.new)
sort(table(d.new$poutcome),decending = true)

d.new=d.new %>%
        mutate(poutcome=as.numeric(poutcome=="unknown"),
               poutcome=as.numeric(poutcome=="success"),
               poutcome =as.numeric(poutcome=="other"))%>%
        select(-poutcome)

str(d.new)
sort(table(d.new$y),decending = true)

d.new=d.new %>%
        mutate(y.yes=as.numeric(y== "yes"))%>%
        select(-y)
str(d.new)
cor(d.new)

set.seed(12345)
sam=sample(1:nrow(d.new),0.70*nrow(d.new))
D_train=d.new[sam,]
test=d.new[-sam,]

###answer of  Q 2
## data prepration
library(tidyr)
library(psych)
summary(D_train)
describe(D_train)
### corelation test using H2o librry

#Finding Correlation
library(h2o)
cor(D_train$y.yes,D_train$campaign)

##vif check > 5
#importing car to use vif function

library(car)
fit=lm(y.yes~., data = D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecoll,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecoll-edu.secondary,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

fit=lm(y.yes~.-month.may-job.bluecoll-edu.secondary-contact.unknown,data=D_train)
summary(fit)
sort(vif(fit),decreasing = TRUE)[1:3]

### drop those var which vif >5
##building the data frame from D_train data removing the predictor variables `month,may`,`job,blue-collar`,`poutcome,unknown`,`education,secondary`-`contact,unknown`

D_train=D_train%>%
        select(-month.may-job.bluecoll-edu.secondary-contact.unknown)
### question 3
# Buiding a logistic regression model

fit1=glm(y.yes~.,family = "binomial", data=D_train)

summary(fit1)
fit1=step(fit1)
summary(fit1)
formula(fit1)

fit2 = glm(y.yes ~ balance + day + duration + campaign + pdays + previous + 
                   job.bluecoll + job.admin + job.retired + job.entrepreneur + 
                   job.housemaid + job.student + marital.married + marital.single + 
                   edu.tertiary + edu.primary + default.no + housing.yes + loan.no + 
                   contact.phone + contact.unknown + month.may + month.jul + 
                   month.aug + month.jun + month.nov + month.apr + month.feb + 
                   month.jan + month.mar, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + pdays + previous + 
                   job.bluecoll + job.admin + job.retired + job.entrepreneur + 
                   job.housemaid + job.student + marital.married + marital.single + 
                   edu.tertiary + edu.primary + housing.yes + loan.no + 
                   contact.phone + contact.unknown + month.may + month.jul + 
                   month.aug + month.jun + month.nov + month.apr + month.feb + 
                   month.jan + month.mar, data = D_train,family = "binomial")

summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + pdays + previous + 
                   job.bluecoll + job.admin + job.retired + job.entrepreneur + 
                   job.housemaid + job.student + marital.married + marital.single + 
                   edu.tertiary + edu.primary + housing.yes + loan.no + 
                   contact.unknown + month.may + month.jul + 
                   month.aug + month.jun + month.nov + month.apr + month.feb + 
                   month.jan + month.mar, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + pdays + previous + 
                   job.bluecoll + job.admin + job.retired + job.entrepreneur + 
                   job.student + marital.married + marital.single + 
                   edu.tertiary + edu.primary + housing.yes + loan.no + 
                   contact.unknown + month.may + month.jul + 
                   month.aug + month.jun + month.nov + month.apr + month.feb + 
                   month.jan + month.mar, data = D_train,family = "binomial")
summary(fit2)

fit2 = glm(y.yes ~ balance + day + duration + campaign + pdays + previous + 
                   job.bluecoll + job.admin + job.retired +job.student +
                   marital.married + marital.single + edu.tertiary + 
                   edu.primary + housing.yes + loan.no + 
                   contact.unknown + month.may + month.jul + 
                   month.aug + month.jun + month.nov + month.apr + month.feb + 
                   month.jan + month.mar, data = D_train,family = "binomial")
summary(fit2)

D_train$score = predict(fit2, newdata = D_train, type = "response")


#PLOT  GRAPHICALLY
library(ggplot2)
ggplot(D_train,aes(y=y.yes,x=score,color=factor(y.yes)))+geom_point()+geom_jitter()



#Question 4
# particular Cutoff
#Assuming an arbitrary Cutoff to be 0.2
# finding cutoff based on KS from train data

cutoff=0.2
predicted=as.numeric(D_train$score>cutoff)
TP=sum(D_train$y.yes==predicted & predicted==1)
FP=sum(D_train$y.yes!=predicted & predicted==1)
TN=sum(D_train$y.yes==predicted & predicted==0)
FN=sum(D_train$y.yes!=predicted & predicted==0)
P=TP+FN
N=TN+FP
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

Sn=TP/P
Sp=TN/N
KS=Sn - (FP/N)

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#Getting optimal cutoff for KS

cutoff_data=data.frame(cutoff=99, KS=99)
cutoffs=seq(0,1,length=1000)
for( cutoff in cutoffs){
        predicted=as.numeric(D_train$score>cutoff)
        TP=sum(D_train$y.yes==predicted & predicted==1)
        FP=sum(D_train$y.yes!=predicted & predicted==1)
        TN=sum(D_train$y.yes==predicted & predicted==0)
        FN=sum(D_train$y.yes!=predicted & predicted==0)
        P=TP+FN
        N=TN+FP
        Sn=TP/P
        Sp=TN/N
        KS=Sn - (FP/N)
        cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
cutoff_data=cutoff_data[-1,]
library(tidyr)
cutoff_data %>%
        gather(Metric,Value) %>%
        ggplot(aes(x=cutoff,y=Value,color=Metric))+geom_line()
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
test$score=predict(fit2,test,type="response")

#Performance on Test Data
#-----------------------------------------------------------------------------------------------------
# Confusion Matrix for KS cutoff
table(test$y.yes,as.numeric(test$score>cutoff_KS))

#-----------------------------------------------------------------------------------------------------

library(pROC)
roccurve = roc(D_train$y.yes ~ D_train$score)
plot(roccurve)
auc(roccurve)

library(InformationValue)

plotROC(D_train$y.yes ,D_train$score, returnSensitivityMat = TRUE)
ks.test(D_train$y.yes ,D_train$score)
ks_plot(actuals = D_train$y.yes, predictedScores = D_train$score)

#-------------------------------------------------------------------------------------
#Question 5

#Random forest
library(randomForest)
D_train$y.yes=as.factor(D_train$y.yes)
#Create the forest.
rf=randomForest(y.yes ~.,data=D_train,do.trace = T) #it will build 500 trees by default
rf

#predicting on test data
test.rf=predict(rf,newdata=test)
table(test$y.yes,test.rf)

###  Performance Random forest
library(caret)
table(test$y.yes,test.rf)

varimp <- importance(rf) 
varImpPlot(rf) 
varImp(rf, scale = FALSE)
sort(varimp,decreasing = TRUE)[1:20]
 
#---------------------------------------------------------
#question no 6

varimp <- importance(rf) 
varImpPlot(rf, type = 2)
sort(varimp,decreasing = TRUE)[1:20]
importance(rf, type=1, scale=TRUE)
 varImp(rf, scale = FALSE)
head(varimp)
library(rpart)
fitt=rpart(factor(y.yes)~., D_train)
plot(fitt)
text(fitt)

sorted <- rf$importance
sort(sorted,decreasing = TRUE)[2:7]

#----------
# variable importance based on Generalized cross validation using Earh library
selected <- data.frame(D_train$duration,D_train$balance,D_train$age,D_train$day,D_train$pdays,D_train$campaign,D_train$y.yes)
View(selected)


selected$D_train.y.yes=as.numeric(selected$D_train.y.yes==1)

str(selected)
dd.new = selected
#split into 70% and  30% in train and test data
set.seed(12345)
sam=sample(1:nrow(dd.new),0.70*nrow(dd.new))
dd_train=dd.new[sam,]
dd_test=dd.new[-sam,]


summary(dd_train)
summary(dd_test)
describe(dd_test)
describe(dd_train)
### corelation test using H2o librry
#describe the data & data prepration
##vif check > 5
#Finding Correlation

cor(dd_train)

fit_1=lm(D_train.y.yes~., data = dd_train)
summary(fit_1)
sort(vif(fit_1),decreasing = TRUE)[1:3]

# Buiding a logistic regression model

fit_2=glm(D_train.y.yes~.,family = "binomial", data=dd_train)

summary(fit_2)
fit1=step(fit_2)
summary(fit_2)
formula(fit_2)

fit_3 = glm(D_train.y.yes ~ D_train.duration + D_train.balance + D_train.age + 
                   D_train.day + D_train.pdays + D_train.campaign, data = dd_train,family = "binomial")

summary(fit_3)

fit_3 = glm(D_train.y.yes ~ D_train.duration + D_train.balance + D_train.age +
                    D_train.pdays + D_train.campaign, data = dd_train,family = "binomial")

summary(fit_3)

dd_train$score = predict(fit_3, newdata = dd_train, type = "response")


#PLOT  GRAPHICALLY
library(ggplot2)
ggplot(dd_train,aes(y=D_train.y.yes,x=score,color=factor(D_train.y.yes)))+geom_point()+geom_jitter()


# particular Cutoff
#Assuming an arbitrary Cutoff to be 0.2
# finding cutoff based on KS from train data
new <- dd_train$D_train.y.yes
cutoff1=0.2
predicted=as.numeric(dd_train$score>cutoff1)
TP=sum(new==predicted & predicted==1)
FP=sum(new!=predicted & predicted==1)
TN=sum(new==predicted & predicted==0)
FN=sum(new!=predicted & predicted==0)
P=TP+FN
N=TN+FP
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

Sn=TP/P
Sp=TN/N
KS=Sn - (FP/N)


#-----------------------------------------------------------------------------------------------------
#Getting optimal cutoff for KS

cutoff_data1=data.frame(cutoff1=99, KS=99)
cutoffs=seq(0,1,length=1000)
for( cutoff1 in cutoffs){
        predicted=as.numeric(dd_train$score>cutoff1)
        TP=sum(new==predicted & predicted==1)
        FP=sum(new!=predicted & predicted==1)
        TN=sum(new==predicted & predicted==0)
        FN=sum(new!=predicted & predicted==0)
        P=TP+FN
        N=TN+FP
        Sn=TP/P
        Sp=TN/N
        KS1=Sn - (FP/N)
        cutoff_data1=rbind(cutoff_data1,c(cutoff1,KS))
}
cutoff_data1=cutoff_data1[-1,]

library(tidyr)
cutoff_data1 %>%
        gather(Metric,Value) %>%
        ggplot(aes(x=cutoff1,y=Value,color=Metric))+geom_line()
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
cutoff_KS1=cutoff_data1$cutoff1[which.max(cutoff_data1$KS)][1]
dd_test$score= predict(fit_3, dd_test,type="response")

#Performance on Test Data
#-----------------------------------------------------------------------------------------------------
# Confusion Matrix for KS cutoff
table(dd_test$D_train.y.yes,as.numeric(dd_test$score>cutoff_KS1))

#-----------------------------------------------------------------------------------------------------

library(pROC)
roc_curve = roc(new ~ dd_train$score)
plot(roc_curve)
auc(roc_curve)

library(InformationValue)

plotROC(new ,dd_train$score, returnSensitivityMat = TRUE)
ks.test(new ,dd_train$score)
ks_plot(actuals = new, predictedScores = dd_train$score)


# Earlier Logistic Regression model =  Area under the curve: 0.8938 and AUROC is 0.8926 and
auc(roccurve)
library(InformationValue)
plotROC(D_train$y.yes ,D_train$score, returnSensitivityMat = TRUE)
#-----------------------------------------------------------------------------------------
## Now Logistic Regression model =  Area under the curve: 0.8269 and AUROC is 0.8235 

auc(roc_curve)
plotROC(new ,dd_train$score, returnSensitivityMat = TRUE)

