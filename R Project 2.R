data<-read.csv("E:/Kedar Business/Acadgild/session 21/Churn.csv")

View(data)
nrow(data)
head(data)
str(data)
names(data)
table(data$Churn)
class(data)
0.2*nrow(data) #test count

set.seed(1)
index<-sample(1:nrow(data),0.8*nrow(data))
index

train_data<-data[index,]
View(train_data)
test_data<-data[-index,]
View(test_data)
str(train_data)
str(test_data)

glm_model1<-glm(Churn~.,family=binomial(link = "logit"),data=train_data)
glm_model1

summary(glm_model1)

glm_model2<-glm(Churn~CustServ.Calls+Int.l.Plan+VMail.Plan+Intl.Calls,family=binomial,data=train_data)
glm_model2

summary(glm_model2)

glm_model3<-glm(Churn~CustServ.Calls+Int.l.Plan+VMail.Plan ,family=binomial,data=train_data)
glm_model3

summary(glm_model3)


pred1<-predict(glm_model1,test_data,type="response")
pred2<-predict(glm_model2,test_data,type="response")
pred3<-predict(glm_model3,test_data,type="response")

pred1
pred2
pred3

#hist(pred)
plot(data$Churn[-index]~pred1)
plot(data$Churn[-index]~pred2)
plot(data$Churn[-index]~pred3)


outcome1=floor(pred1+0.50)
outcome1

outcome2=floor(pred2+0.50)
outcome2

outcome3=floor(pred3+0.50)
outcome3


table(outcome1)
ttt1=table(      data$Churn[-index]    ,    outcome1    )

ttt1

Accuracy1<-(553+2)/(553+2+80+32)
Accuracy1

table(outcome2)
ttt2=table(      data$Churn[-index]    ,    outcome2    )

ttt2

Accuracy2<-(566+2)/(566+2+80+19)
Accuracy2

table(outcome3)
ttt3=table(      data$Churn[-index]    ,    outcome3    )

ttt3

Accuracy3<-(570+4)/(570+4+78+15)
Accuracy3

0.3*nrow(data) #test count

set.seed(1)
index1<-sample(1:nrow(data),0.7*nrow(data))
index1

train_data1<-data[index1,]
View(train_data1)
test_data1<-data[-index1,]
View(test_data1)
str(train_data1)
str(test_data1)


glm_model4<-glm(Churn~.,family=binomial(link = "logit"),data=train_data1)
glm_model4

summary(glm_model4)

glm_model5<-glm(Churn~Account.Length+VMail.Message+Eve.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan +VMail.Plan+Day.Calls +Eve.Charge+Night.Charge,family=binomial,data=train_data1)
glm_model5

summary(glm_model5)

glm_model6<-glm(Churn~ VMail.Message+Eve.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Eve.Charge+Night.Charge,family=binomial,data=train_data1)
glm_model6

summary(glm_model6)


glm_model8<-glm(Churn~ CustServ.Calls+Int.l.Plan+VMail.Plan+Night.Charge,family=binomial,data=train_data1)
glm_model8

summary(glm_model8)



pred4<-predict(glm_model4,test_data1,type="response")
pred5<-predict(glm_model5,test_data1,type="response")
pred6<-predict(glm_model6,test_data1,type="response")
pred7<-predict(glm_model7,test_data1,type="response")
pred8<-predict(glm_model8,test_data1,type="response")

pred4
pred5
pred6
pred7
pred8


outcome4=floor(pred4+0.50)
outcome4

outcome5=floor(pred5+0.50)
outcome5

outcome6=floor(pred6+0.50)
outcome6

outcome7=floor(pred7+0.50)
outcome7

outcome8=floor(pred8+0.50)
outcome8


table(outcome4)
ttt4=table(      data$Churn[-index1]    ,    outcome4    )

ttt4

Accuracy4<-(619+107)/(619+107+34+240)
Accuracy4

table(outcome5)
ttt5=table(      data$Churn[-index1]    ,    outcome5    )

ttt5

Accuracy5<-(611+106)/(611+106+248+35)
Accuracy5

table(outcome6)
ttt6=table(      data$Churn[-index1]    ,    outcome6    )

ttt6

Accuracy6<-(618+102)/(618+102+39+241)
Accuracy6

table(outcome7)
ttt7=table(      data$Churn[-index1]    ,    outcome7    )

ttt7

Accuracy7<-(623+96)/(623+96+45+236)
Accuracy7

table(outcome8)
ttt8=table(      data$Churn[-index1]    ,    outcome8    )

ttt8

Accuracy8<-(614+95)/(614+95+245+46)
Accuracy8




outcome11=floor(pred1+0.70)
outcome11

outcome12=floor(pred2+0.70)
outcome12

outcome13=floor(pred3+0.70)
outcome13


table(outcome11)
ttt11=table(      data$Churn[-index]    ,    outcome11    )

ttt11

Accuracy11<-(500+17)/(500+17+79+71)
Accuracy11

table(outcome12)
ttt12=table(      data$Churn[-index]    ,    outcome12    )

ttt12

Accuracy12<-(517+13)/(517+13+62+75)
Accuracy12

table(outcome13)
ttt13=table(      data$Churn[-index]    ,    outcome13    )

ttt13

Accuracy13<-(526+11)/(526+11+77+53)
Accuracy13

outcome14=floor(pred1+0.40)
outcome14

outcome15=floor(pred2+0.40)
outcome15

outcome16=floor(pred3+0.40)
outcome16


table(outcome14)
ttt14=table(      data$Churn[-index]    ,    outcome14    )

ttt14

Accuracy14<-(564+5)/(564+5+15+83)
Accuracy14

table(outcome15)
ttt15=table(      data$Churn[-index]    ,    outcome15    )

ttt15

Accuracy15<-(573+1)/(573+1+6+87)
Accuracy15

table(outcome16)
ttt16=table(      data$Churn[-index]    ,    outcome16    )

ttt16

Accuracy16<-(573+2)/(573+2+6+86)
Accuracy16


0.4*nrow(data) #test count

set.seed(1)
index21<-sample(1:nrow(data),0.6*nrow(data))
index21

train_data21<-data[index21,]
View(train_data21)
test_data21<-data[-index21,]
View(test_data21)
str(train_data21)
str(test_data21)


glm_model21<-glm(Churn~.,family=binomial(link = "logit"),data=train_data21)
glm_model21

summary(glm_model4)

glm_model5<-glm(Churn~Account.Length+VMail.Message+Eve.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan +VMail.Plan+Day.Calls +Eve.Charge+Night.Charge,family=binomial,data=train_data1)
glm_model5

summary(glm_model5)

glm_model6<-glm(Churn~ VMail.Message+Eve.Mins+Intl.Mins+CustServ.Calls+Int.l.Plan+VMail.Plan+Eve.Charge+Night.Charge,family=binomial,data=train_data1)
glm_model6

summary(glm_model6)


glm_model8<-glm(Churn~ CustServ.Calls+Int.l.Plan+VMail.Plan+Night.Charge,family=binomial,data=train_data1)
glm_model8

summary(glm_model8)



pred4<-predict(glm_model4,test_data1,type="response")
pred5<-predict(glm_model5,test_data1,type="response")
pred6<-predict(glm_model6,test_data1,type="response")
pred7<-predict(glm_model7,test_data1,type="response")
pred8<-predict(glm_model8,test_data1,type="response")

pred4
pred5
pred6
pred7
pred8


outcome4=floor(pred4+0.50)
outcome4

outcome5=floor(pred5+0.50)
outcome5

outcome6=floor(pred6+0.50)
outcome6

outcome7=floor(pred7+0.50)
outcome7

outcome8=floor(pred8+0.50)
outcome8


table(outcome4)
ttt4=table(      data$Churn[-index1]    ,    outcome4    )

ttt4

Accuracy4<-(619+107)/(619+107+34+240)
Accuracy4

table(outcome5)
ttt5=table(      data$Churn[-index1]    ,    outcome5    )

ttt5

Accuracy5<-(611+106)/(611+106+248+35)
Accuracy5

table(outcome6)
ttt6=table(      data$Churn[-index1]    ,    outcome6    )

ttt6

Accuracy6<-(618+102)/(618+102+39+241)
Accuracy6

table(outcome7)
ttt7=table(      data$Churn[-index1]    ,    outcome7    )

ttt7

Accuracy7<-(623+96)/(623+96+45+236)
Accuracy7

table(outcome8)
ttt8=table(      data$Churn[-index1]    ,    outcome8    )

ttt8

Accuracy8<-(614+95)/(614+95+245+46)
Accuracy8


########################  Overcoming Imbalanced Dataset   #########################

data_1<-data[data$Churn==1,]
nrow(data_1)

ind_1<-sample(rownames(data_1),350)

data_0<-data[data$Churn==0,]

ind_0<-sample(rownames(data_0),350)

train_data1<-data[c(ind_1,ind_0) , -21 ]

str(train_data1)

table(train_data1$Churn)

glm_model1<-glm(Churn~.,family=binomial,train_data1)

glm_model1

pred1<-predict(glm_model1,test_data,type="response")
pred1

gg1=floor(pred1+0.5)
gg1
ttt1=table(data$Churn[-index],gg1)
ttt1

exp(cbind(Odds_and_OR=coef(glm_model1), confint(glm_model1)))
########################################################################################

data_3<-data[data$Churn==1,]
nrow(data_3)

ind_3<-sample(rownames(data_3),483)

data_4<-data[data$Churn==0,]

ind_4<-sample(rownames(data_4),483)

train_data3<-data[c(ind_3,ind_4) , -21 ]

str(train_data3)

table(train_data3$Churn)

glm_model11<-glm(Churn~.,family=binomial,train_data3)

glm_model11

pred11<-predict(glm_model11,test_data,type="response")
pred11

gg11=floor(pred11+0.5)
gg11
ttt11=table(data$Churn[-index],gg11)
ttt11

exp(cbind(Odds_and_OR=coef(glm_model1), confint(glm_model1)))

######################## Removing Variables ###########################

str(data)

table(data$Churn)

index<-sample(1:nrow(data),0.8*nrow(data))

train_data<-data[index,-c(19,21)]
test_data<-data[-index,-c(8,19,21)]

str(train_data)
str(test_data)

glm_model<-glm(Churn~.,family=binomial,data=train_data)
glm_model

pred<-predict(glm_model,test_data,type="response")
pred

hist(pred)
plot(data$Churn[-index]~pred)

gg=floor(pred+0.5)
gg
ttt=table(data$Churn[-index],gg)
ttt






###########################################################################
###########################################################################
###########################################################################

glm_model.null<-glm(Churn~1,family=binomial(link="logit"),data=train_data)
summary(glm_model.null)

step(glm_model.null, scope = list(upper=glm_model), direction="both", test="Chisq", data=train_data)





























