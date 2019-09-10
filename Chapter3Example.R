library(gee)
library(geepack)
library(glm.predict)
library(wgeesel)
library(HSAUR)

#Section 3.3 : Example of respiratory status
# Dataset of respiratory status
data("respiratory", package = "HSAUR")
str(respiratory)
respiratory
resplot <- subset(respiratory, month > "0")
resplot$baseline <- rep(subset(respiratory, month == "0")$status, rep(4, 111))
resplot$nstat <- as.numeric(resplot$status == "good")
respiratory$astat<-as.numeric(respiratory$status == "good")
resplot
#------------------------------------------------------------------------------------------#
#Section 3.3.1
  
# Table3.2 : Estimations of regression parameters by fiting equation(3.23) with GEE approache#
resgee1<- geeglm(nstat~centre+ treatment + sex + baseline+age, family=binomial,
                   data=resplot, id=subject, corstr = "independence")


#Section 3.3.2
#Table3.3 : Comparison between GEE and GLM approaches
  
  
# Estimations of regression parameters by fiting equation(3.23) with generalized linear model 
resglm <- glm(status ~ centre + treatment + sex + baseline +
                     + age, data = resplot, family = "binomial")
summary(resglm)
#Estimations of regression parameters by fiting equation(3.23) with GEE approach and "Independent" working structure
summary(resgee1)

#Estimations of regression parameters by fiting equation(3.23) with GEE approach and "Exchangeable" working structure
resgee2<- geeglm(nstat~centre+ treatment + sex + baseline+age, family=binomial,
                 data=resplot, id=subject, corstr = "exchangeable")
summary(resgee2)
#-----------------------------------------------------------------------------------------#
#Section 3.3.3
#Extract the working correlation structure

# Estimations of regression parameters by fiting equation(3.23) 
# with GEE approach and "Unstructured" working structure
resgee3<- geeglm(nstat~centre+ treatment + sex + baseline+age, family=binomial,
                 data=resplot, id=subject, corstr = "unstructured")

# Estimations of regression parameters by fiting equation(3.23) 
# with GEE approach and autoregression correlation structure
resgee4<- geeglm(nstat~centre+ treatment + sex + baseline+age, family=binomial,
                 data=resplot, id=subject, corstr = "ar1")

summary(resgee1)
summary(resgee2)
summary(resgee3)
summary(resgee4)
# The working correlation structures can be obtained from "Estimated Scale Parameters"
#-------------------------------------------------------------------------------------#
# Extract the Quasi-likelihood value with different working correlation structure.

# Model with "Autoregression" correlation structure : 
arfit<- wgee(nstat~centre+ treatment + sex + baseline+age, family="binomial",
            data=resplot, id=resplot$subject, corstr = "ar1")
# Model with "Exchangeable" correlation structure : 
efit<- wgee(nstat~centre+ treatment + sex + baseline+age, family="binomial",
            data=resplot, id=resplot$subject, corstr = "exchangeable")
# Model with "Independent" correlation structure : 
ifit<- wgee(nstat~centre+ treatment + sex + baseline+age, family="binomial",
            data=resplot, id=resplot$subject, corstr = "independence")
# Model with "Unstructured" correlaiton structure : 
ufit<-wgee(nstat~centre+ treatment + sex + baseline+age, family="binomial",
                  data=resplot, id=resplot$subject, corstr = "unstructured")

# Extract the Quasi-likelihood value from models above
QIC.gee(arfit)
QIC.gee(efit)
QIC.gee(ifit)
QIC.gee(ufit)
#------------------------------------------------------------------------------------#
#Section3.4.4#

# Estimations of refression model by fiting the equation(3.24) with generalized linear mixed model#
respglmm <-subset(respiratory)
respglmm$time<-rep(c(0,1,2,3,4),times=111)
respglmm$baseline <- rep(subset(respiratory, month == "0")$status)
respglmm$nstat <- as.numeric(respglmm$status == "good")

resp1<- glmer(nstat~ baseline+centre+treatment + time+treatment:time
              +(1|subject), family=binomial(link="logit"),
              data=respglmm)
#Table3.6 : Estimates of fixed regression parameters 
summary(resp1)


