#R Project 
heart=read.csv("C:/Users/kpiaz/OneDrive/Junior Year Semester 1/Stats 308/heart.csv", header=TRUE)
summary(heart)

#makes it so that 1=heart disease
heart$target=abs(heart$target-1)

#######full model###### #AIC=224
fit.logit = glm(target~ ï..age +  sex +  as.factor(cp)+ trestbps +chol +fbs +restecg +oldpeak+as.factor(ca)+as.factor(thal)+thalach+ as.factor(slope),data = heart, family = "binomial")
summary(fit.logit)
confint.default(fit.logit)

#######peary example bad model###### AIC= 365
peary=glm(target~chol +sex +as.factor(slope)+ chol*as.factor(slope), data=heart)
summary(peary)
exp(coef(peary))

#######sample model 2####### AIC=288
model2= glm(target~ï..age+ sex + chol + as.factor(cp)+oldpeak, data=heart)
summary(model2)
exp(coef(model2))

#######sample model 9####### AIC=306
model9= glm(target~ï..age+ sex + chol + as.factor(ca)+oldpeak, data=heart)
summary(model9)
exp(coef(model9))

#######sample model 10####### AIC=275
model10= glm(target~ï..age+ sex + chol + as.factor(cp)+oldpeak+as.factor(slope), data=heart)
summary(model10)
exp(coef(model10))

#######sample model 8####### AIC=314
model8= glm(target~ï..age+ chol + as.factor(cp)+oldpeak, data=heart)
summary(model8)
exp(coef(model8))

#######sample model 5####### AIC=314
model5= glm(target~ï..age+ chol + as.factor(cp)+oldpeak, data=heart)
summary(model5)
exp(coef(model5))

#######sample model 6####### AIC=279
model6= glm(target~ï..age+ sex + chol + as.factor(cp)+oldpeak+thalach, data=heart)
summary(model6)
exp(coef(model6))

#######sample model 7####### AIC=289
model7= glm(target~ï..age+ sex + chol + as.factor(cp)+oldpeak +ï..age*sex, data=heart)
summary(model7)
exp(coef(model7))

#######sample model 4####### AIC=318
model4= glm(target~ï..age+ sex + chol + as.factor(cp), data=heart)
summary(model4)
exp(coef(model4))

#######sample model 3####### #AIC=399
model3= glm(target~ï..age+ sex + chol + fbs , data=heart)
summary(model3)
exp(coef(model3))

#######target on age###### AIC=405
fit.logit2=glm(target~ï..age, data=heart, family= "binomial")
summary(fit.logit2)
plot(target~ï..age, data=heart)
confint.default(fit.logit2)


#######target on sex####### AIC=396
fit.logit3=glm(target~sex, data=heart, family= "binomial")
summary(fit.logit3)
plot(target~sex, data=heart)
confint.default(fit.logit3)

#########target on cp######## AIC=339
fit.logit4=glm(target~as.factor(cp), data=heart, family= "binomial")
summary(fit.logit4)
plot(target~as.factor(cp), data=heart)
confint.default(fit.logit4)

########target on trestbps###### AIC=415
fit.logit5=glm(target~trestbps, data=heart, family= "binomial")
summary(fit.logit5)
plot(target~trestbps, data=heart)
confint.default(fit.logit5)

#######target on chol####### AIC= 419
fit.logit6=glm(target~chol, data=heart, family= "binomial")
summary(fit.logit6)
plot(target~chol, data=heart)
confint.default(fit.logit6)

#######target on fbs######## AIC=421
fit.logit7=glm(target~fbs, data=heart, family= "binomial")
summary(fit.logit7)
plot(target~fbs, data=heart)
confint.default(fit.logit7)

########target on restecg###### #AIC=413
fit.logit8=glm(target~as.factor(restecg), data=heart, family= "binomial")
summary(fit.logit8)
plot(target~as.factor(restecg), data=heart)
confint.default(fit.logit8)


########target on oldpeak######## AIC=359
fit.logit9=glm(target~oldpeak, data=heart, family= "binomial")
summary(fit.logit9)
plot(target~oldpeak, data=heart)
confint.default(fit.logit9)

#########target on ca######## AIC=349
fit.logit10=glm(target~as.factor(ca), data=heart, family= "binomial")
summary(fit.logit10)
plot(target~as.factor(ca), data=heart)
confint.default(fit.logit10)

#########target on thal####### POSSIBLE THROWOUT. DATA FUCK UP AIC=336
fit.logit11=glm(target~as.factor(thal), data=heart, family= "binomial")
summary(fit.logit11)
plot(target~as.factor(thal), data=heart)
confint.default(fit.logit11)

#############target on thalach######## AIC=363
fit.logit12=glm(target~thalach, data=heart, family= "binomial")
summary(fit.logit12)
plot(target~thalach, data=heart)
confint.default(fit.logit12)

#######TARGET ON SLOPE####### AIC=374
fit.logit13=glm(target~as.factor(slope), data=heart, family= "binomial")
summary(fit.logit13)
plot(target~as.factor(slope), data=heart)
confint.default(fit.logit13)







#extended 17 *exang interaction not sig w: trestbps, thalach, chol - fbs slightly significant 
modelG= glm(target~ sex + as.factor(cp) + as.factor(ca) + as.factor(exang) + as.factor(slope) , data=heart)
summary(modelG)
exp(coef(modelG))

#####model 18###### *no thalach, oldpeak, chol, age, trestbps 238
model18= glm(target~ sex + as.factor(cp) + as.factor(ca) + as.factor(exang) + as.factor(slope) +as.factor(slope)*as.factor(exang) , data=heart)
summary(model18)
exp(coef(model18))

#####model 19##### *keep cp, ca. No: slope*ca, slope*sex, slope*exang AIC=238
modelH= glm(target~ sex + as.factor(cp) + as.factor(ca) + as.factor(exang) + as.factor(slope) + as.factor(slope)*as.factor(cp), data=heart)
summary(modelH)
exp(coef(modelH))

#####model 20##### AIC=235
model20= glm(target~ sex + as.factor(cp) + as.factor(ca) + as.factor(exang) + as.factor(slope) + as.factor(slope)*as.factor(cp) + sex*as.factor(exang), data=heart)
summary(model20)

  
  #######Forward MEthod########
fit.hearts = glm(target~1,data=heart,family="binomial")
forwards = step(fit.hearts,scope=list(lower=formula(fit.hearts),upper=formula(fit.logit)), direction="forward")

#######stepwise MEthod########
fit.hearts = glm(target~1,data=heart,family="binomial")
steps = step(fit.hearts,scope=list(lower=formula(fit.hearts),upper=formula(fit.logit)), direction="both")

#Step:  AIC=230.22
#target ~ as.factor(cp) + as.factor(ca) + oldpeak + sex + as.factor(slope) + 
# trestbps + thalach


#####lots of commands##### TRASHING
plot(exp(predict(fit.logit))/(1 + exp(predict(fit.logit)))~heart$oldpeak)


fit.logit = glm(target~  sex +  as.factor(cp)+ trestbps +chol +fbs +restecg +oldpeak+as.factor(ca)+thalach+ as.factor(slope),data = heart, family = "binomial")
summary(fit.logit)
confint.default(fit.logit)

library(ROCR)
pred = predict(fit.logit,type="response")
preds = prediction(pred, heart$target)


roc = performance(preds,"tpr","fpr")
plot(roc)
abline(0,1)

auc = performance(preds, measure = "auc")
auc@y.values

