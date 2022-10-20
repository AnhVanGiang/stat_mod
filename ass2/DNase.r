## Nonlinear regression for dataset DNase
attach(DNase)
n=length(density)
form=as.formula(density~theta1/(1+exp((theta2-log(conc))/theta3)))
plot(conc,density,xlab="Concentration",ylab="Optical density") # plot of dataset
nmodel=nls(form,DNase,start=c(theta1=3,theta2=0,theta3=1)) 
nmodel # cantains the estimates of the parameter and RSS
hat.th=coef(nmodel); hat.th ## the LSE estimates of theta's
summary(nmodel) # more informative output
# goodness of fit (should be closer to 1):cor(density,predict(nmodel)) #0.9969554
# residual sum of squares S(hat.th)=RSS=sum(residuals(nmodel)^2) # the same as
RSS=deviance(nmodel);RSS # the same info in nmodel
# the estimate of the error variance is 
hat.var=RSS/(n-p)=RSS/(n-2); hat.var
se=sqrt(hat.var)  # residual standard error, the same as in summary(nmodel)
# the estimates of the variances of theta's: diag(vcov(nmodel))
# sqrt(diag(nmodel)) # the same as Std.Error in summary(nmodel) 

## nonlinear regression function
f=function(x,theta)return(theta[1]/(1+exp((theta[2]-log(x))/theta[3])))
coef(nmodel) # estimates for theta
x=seq(from=0.1,to=13,by=0.1)
lines(x,f(x,coef(nmodel)),col="red")  # fitted curve
# or: lines(x,predict(nmodel,newdata=data.frame(conc=x)),col="red")


# the estimated covariance matrix for hat.theta
cov.est=vcov(nmodel);cov.est

# confidence intervals for coordinates of theta computed by R: confint(nmodel)
# which gives different intervals as compared with directly computed ones below,
# possibly because in confint(nmodel) the method based on F-statistics is used.
# Indeed, notice that the intervals produced by confint(nmodel) are not symmetric 
# around the coef(nmodel). We compute the confidence intervals directly
lb=numeric(3); ub=numeric(3); # rownames(lb)=names(coef(nmodel))
for(i in 1:3) {lb[i]=coef(nmodel)[i]-qt(0.975,n-length(coef(nmodel)))*sqrt(cov.est[i,i])
      ub[i]=coef(nmodel)[i]+qt(0.975,n-length(coef(nmodel)))*sqrt(cov.est[i,i])}
ci=cbind(lb,ub); rownames(ci)=names(coef(nmodel)); ci

## Bootstrap procedure CI's for theta1, theta2 and theta3
B=1000 # 1000
par.boot=matrix(NA,B,length(coef(nmodel)))
rownames(par.boot)=paste("B",1:B,sep="")
colnames(par.boot)=names(coef(nmodel))
res.centered=resid(nmodel)-mean(resid(nmodel))

for(b in 1:B){
  # cat("b = ",b,"\n")
  # Bootstrap samples from centered residuals
  res=sample(res.centered,replace=T)
  # Calculate bootstrap values for the response
  yboot=fitted(nmodel)+res #Y*_1,...Y*_n
  # Fit model using new response and get bootstrap estimates for parameter 
  modelBoot=nls(yboot~theta1/(1+exp((theta2-log(conc))/theta3)),
    data=data.frame(yboot,conc),start=list(theta1=3,theta2=0,theta3=1))
  # Store estimated (by bootstrap) parameters  
  par.boot[b,]=coef(modelBoot) #\theta*_1,..., \theta*_B
}

# Bootstrap estimated variances
c(var(par.boot[,1]),var(par.boot[,2]),var(par.boot[,2]))
# Compute the bootstrap 95% confidence intervals for the thetas
lb.boot=2*coef(nmodel)-apply(par.boot,2,quantile,prob=0.975)
ub.boot=2*coef(nmodel)-apply(par.boot,2,quantile,prob=0.025)
cbind(lb.boot,ub.boot)

  
## estimate of the mean response f(4,theta)
f4=f(4,coef(nmodel));f4  
## or f4=predict(nmodel,newdata=data.frame(conc=4))

## 90%-confidence interval for the mean response f(4,theta)
# first compute the gradient function (represented as column)
# this can also be done by usin deriv() function
grad<-function(x,theta){rbind(1/(1+exp((theta[2]-log(x))/theta[3])),
-(theta[1]*exp((theta[2]-log(x))/theta[3])/theta[3])/(1+exp((theta[2]-log(x))/theta[3]))^2,
(theta[1]*exp((theta[2]-log(x))/theta[3])*(theta[2]-log(x))*(theta[3]^(-2))/(1+exp((theta[2]-log(x))/theta[3]))^2))}
gradvec=grad(4,coef(nmodel))
se=sqrt(t(gradvec)%*%vcov(nmodel)%*%gradvec) #0.006986284
lb=f4-qt(0.95,n-length(coef(nmodel)))*se #1.156622
ub=f4+qt(0.95,n-length(coef(nmodel)))*se #1.179728
c(lb,ub) # approximate confidence interval for f(4,theta)
## Make the bootstrap confidence interval for f(4,theta) yourself!

## estimates and 90%-confidence approximate intervals 
## the mean responses for many x's from (0,13]
x=seq(0.1,13,by=0.1)
# estimates for f(x,theta) for x from (0,13]
fe=f(x,coef(nmodel)) # or: fe=predict(nmodel,newdata=data.frame(conc=x))
mygrad=grad(x,coef(nmodel)) # estimated gradients for x's from (0,13]
se<-sqrt(apply(mygrad,2,function(xx) t(xx)%*%vcov(nmodel)%*%xx))
lb<-fe-qt(0.95,n-length(coef(nmodel)))*se 
ub<-fe+qt(0.95,n-length(coef(nmodel)))*se
# plot of confidence intervals for all c from (0,13]
plot(conc,density,xlab="Concentration",ylab="Optical density") # data
lines(x,fe,t="l",lwd=0.5,col="red") # fitted curve
segments(x,lb,x,ub,col="grey") # confidence intervals
# another plot of confidence intervals for all x from (0,13]
plot(conc,density,xlab="Concentration",ylab="Optical density") 
polygon(c(x,rev(x)),c(lb,rev(ub)),col="grey",border=NA)
lines(x,fe,lwd=0.5,col="red")

## Validity of model assumptions
# residuals against the fitted values
plot(fitted(nmodel),resid(nmodel));abline(h=0,lty=3) # not good
# qq-plot
qqnorm(resid(nmodel)); qqline(resid(nmodel),col="red") # not good
hist(resid(nmodel))   # not good

# Comparing nested models
form2=as.formula(density~theta1/(1+exp((-log(conc))/theta3)))
nmodel2=nls(form2,DNase,start=c(theta1=3,theta3=1)) # smaller model for theta2=0
anova(nmodel,nmodel2) # smaller model gives a bad fit
# Let us derive this also directly
SSq=sum(resid(nmodel)^2); SSq # RSS for the big model # deviance(nmodel)
SSp=sum(resid(nmodel2)^2); SSp # RSS for the small model
n=length(resid(nmodel)); q=length(coef(nmodel)); p=length(coef(nmodel2)) 
f=((SSp-SSq)/(q-p))/(SSq/(n-q));f  # f-statistic, now p-value 
1-pf(f,q-p,n-q) # H0 is rejected, the small model is not good 

## Now by Akaike Information Criterion (smaller value means better model)
AIC(nmodel) #-572.7897, 
# if computing AIC directly (remember sigma^2 is one more parameter): 
# n*log(SSq/n)+n*log(2*pi)+n+2*(q+1) # the same as -2*logLik(nmodel)+2*(q+1)
AIC(nmodel2) # AIC bigger for the smaller model, nmodel2 is not good also by AIC
## Bayesian Information Criterion
# BIC(nmodel);BIC(nmodel2) # BIC is again much bigger for the smaller model
## the smaller model nmodel2 is not good as confirmed by both AIC and BIC 


#### Compare the models by cross validation #####
train=sample(1:length(density),0.67*length(density)) # train by using 2/3 of the x rows 
x.train=conc[train];y.train=density[train]  # data to train
x.test=conc[-train];y.test=density[-train] # data to test the prediction quality
x=x.train
nmodel1=nls(as.formula(y.train~theta1/(1+exp((theta2-log(x))/theta3))),
            start=c(theta1=3,theta2=0,theta3=1))
x=x.test
y1.predict=predict(nmodel1,newdata=data.frame(x)) 
#command predict() is tricky for nls-models, stick to this syntax 
mse1=mean((y.test-y1.predict)^2);mse1 # prediction quality of nmodel1
x=x.train
nmodel2=nls(as.formula(y.train~theta1/(1+exp((-log(x))/theta3))),
            start=c(theta1=3,theta3=1))
x=x.test
y2.predict=predict(nmodel2,newdata=data.frame(x))
mse2=mean((y.test-y2.predict)^2);mse2 # prediction quality of nmodel2 is worse

## Now let us fit a DNN ##########
# Import Required packages
library(neuralnet)
library(MASS)

# Remarks: better python-based packages: library(tensorflow);library(keras)
# sequential model (hidden layers) building, also relu activation 
# Sometimes the data is normalized, for example:
# maxs=apply(data,2,max);mins=apply(data,2,min)
# scaled=as.data.frame(scale(data,center=mins,scale=maxs-mins))

# DNN model, two hidden layes, (3,3) neurons, activation=logistic 
nn=neuralnet(density~conc,data=DNase[train,],hidden=c(3, 3))
# Predict on test data
pr.nn=predict(nn,newdata=data.frame(conc[-train]))
# # prediction quality of the proposed DNN
mse.nn=mean((y.test-pr.nn)^2); mse.nn

# Plot the neural network: plot(nn)
# Plot true versus predicted
plot(y.test,pr.nn,col="red",main='True vs Predicted')
abline(0,1,lwd = 2)
