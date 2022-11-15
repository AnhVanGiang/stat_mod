### Useful R-commands to extract info from a GLM model model=glm(...) ###

# coef(model) # returns the coefficients hat.beta
# fitted(model) #or fitted.values(model) #extracts the fitted values hat.mu
# extracting the residual degrees of freedom:
# df.residual(model) # or model$df.resid # must be n-p-1
# vcov(model) # hat.I matrix, estimated covariance matrix of hat.beta
# also summary(model)$cov.scaled
# sqrt(vcov(model)[3,3]) # Std. Error of coefficient beta3
# in summary(model) p-value for coefficient beta with test statistics value t 
# is appr. 2*(1-pnorm(t)), but we use t-distr.:2*(1-pt(t,model$df.resid))
# weights(model,type="working") # weights matrix
# 4 types of residuals (deviance residuals are default)
# residuals(model,type="response")	# Response residuals R_i=y_i-hat.mu_i
# residuals(model,type="deviance")	# sqrt(d_i)*sign(R_i) # default ones
# residuals(model,type="pearson")	# R_i/sqrt(b''(hat.theta_i)/A_i)
# residuals(model,type="working")	# R_i*g'(hat.mu_i)
# Predictions # default is type="link", others are "response" and "terms".
# predict(model,type="link",se.fit=TRUE) # linear predictions hat.eta and se
## the same as model$linear.predictors 
# predict(model,type="response",se.fit=TRUE) # predictions hat.mu and se
# one can use those for constructing CI's for mu's
# confint(model) # CI's for beta, but we use t-distribution

setwd("your directory with data")

########### Melanoma data ###########

data0=read.table("melanoma.txt",header=TRUE)
attach(data0)
plot(sunspot,incidence)
plot(years,incidence,main="Poisson Regression for Melanoma Data",
     xlab="Year of Diagnosis",ylab="Incidence of Melanoma")
# by default: family=poisson(link="log")
model0=glm(incidence~years+sunspot,data=data0,family=poisson)
summary(model0) # sunspot is not significant, remove it
# later we also consider anova(model0,test="Chisq")

model1=glm(incidence~years,data=data0,family=poisson) # sunspot removed
summary(model1) # in reduced model years remains significant
# later consider also anova(model1,test="Chisq")  
lines(years,fitted(model1),t="p",pch=16) # fitted(model1) gives hat.mu
legend("topleft",legend=c("Observed","Fitted"),pch=c(1,16))

##### Some more staff #####
df.residual(model0) # must be n-p-1=37-2-1=34
sqrt(vcov(model0)[3,3]) # Std. Error of beta3, to see in summary(model0) 
2*(1-pnorm(0.957)) # p-value for sunspot (see summary(model0)), not significant
2*(1-pt(0.957,model0$df.resid)) # use t-distr., still not significant
#### plots: fitted against observed, fitted against deviance residuals
plot(fitted(model0),data0$incidence,main="Fitted against observed") #seems ok
plot(fitted(model0),residuals(model0),xlab="Fitted values",ylab="Deviance residuals")
## apply variance-stabilizing transformation to the fitted values sqrt(hat.mu)
plot(sqrt(fitted(model0)),residuals(model0)) # not really better


####################### Byssinosis data ############

data1=read.table("lung.txt",header=TRUE);data1
names(data1)=c("Sick","Healthy","DustLevel","Race","Sex","Smoking","Length")
attach(data1)
 # declare factor variables
data1$DustLevel=factor(data1$DustLevel)
data1$Race=factor(data1$Race)
data1$Sex=factor(data1$Sex)
data1$Smoking=factor(data1$Smoking)
data1$Length=factor(data1$Length)

## for aggregated data, as response variable you need to specify the pair: 
## the numbers of sick and  healthy workers for each combination of factors. 
## By default: family=binomial(link="logit")
model2=glm(cbind(Sick,Healthy)~DustLevel+Race+Sex+Smoking+Length,
           data=data1,family=binomial)
summary(model2) # later we also consider drop1(model2,test="Chisq")
# omit factors Race and Sex as these appear to be not significant 
# and fit the reduced model  
model3=glm(cbind(Sick,Healthy)~DustLevel+Smoking+Length,
           data=data1,family=binomial)
summary(model3) # later: drop1(model3,test="Chisq")
# later: testing whether the reduced model fits 
# anova(model3,model2,test="Chisq") # the reduced model fits
plot(fitted(model3),Sick/(Sick+Healthy),pch=16,xlab="Fitted Values",
ylab="Observed Response Proportion") # problematic (zero) counts for some cells


#### Handling inconsistencies in byssinosis data (not essential) ####

## there are bad rows: the ones with zero workers in the cell
## the proportions for those rows are not computable
# how many bad rows
length(data1$Healthy[data1$Sick+data1$Healthy==0]) 
# remove those 7 bad rows, get "cleaned" data in data frame data2
data2=data1[data1$Sick+data1$Healthy>0,]
rownames(data2)=1:nrow(data2)
# compute proportions for the cleaned data
prop=data2$Sick/(data2$Sick+data2$Healthy)
# append the column of proportions
data3=cbind(prop,n.tot=data2$Sick+data2$Healthy,data2)
# data3=data3[-c(3,4)] # in principle we can remove columns 3 and 4 
## as now we can create the model in the following way 
# model2=glm(prop~DustLevel+Race+Sex+Smoking+Length,
# data=data3,weights=n.tot,family=binomial) # need to give weights A_i=n_i
# as R should know how many observations a proportion is based on
### now make boxplot for the "cleaned" data
boxplot(prop~DustLevel,data=data3,names=c("High","Medium","Low"), 
 xlab="Workplace dust level",ylab="Proportion of workers with byssinosis")
## data without "outliers"
# data3=data[data$prop<0.35,]
## boxplot for data without outliers
# boxplot(prop~DustLevel,data=data3,names=c("High","Medium","Low"),
#   xlab="Workplace dust level",ylab="Proportion of workers with byssinosis")
####################################################
                          

####### Inference for byssinosis data ############

## Fisher inf. matrix X^T\hat{W}X (recall that phi=1 for the binomial model) 
# I=t(model.matrix(model2))%*%diag(model2$weights)%*%model.matrix(model2)
# then you need to invert this matrix FisherInv=solve(I)
# simpler, extract the covariance matrix from the GLM object model2
FisherInv=vcov(model2);FisherInv #or FisherInv=summary(model2)$cov.scaled
# round(FisherInv,3)
# diagonal elements sqrt(FisherInv[i,i]) are also in summary(model2) 
# and can be accessed as summary(model2)$coefficients[i,i]
# In principle, all the info needed for testing and CI's is in summary(model2)

## 95% CI for beta1 corresponding to moderate dust level
coef(model2)[2]+c(-1,1)*qt(0.975,model2$df.resid)*sqrt(FisherInv[2,2])
# or use summary(model2)$coefficients[2,2] instead of sqrt(FisherInv[2,2])
## 95% CI for beta4 (corresponding to female gender)
coef(model2)[5]+c(-1,1)*qt(0.975,model2$df.resid)*sqrt(FisherInv[5,5])
# the CI's can also be found from confint(model2) 
# but these are based on normal quantiles

## compute t-statistics for testing beta1=0 and beta4=0
t1=coef(model2)[2]/sqrt(FisherInv[2,2]); t1 # it is also in summary(model2) 
t2=coef(model2)[5]/sqrt(FisherInv[5,5]); t2 # it is also in summary(model2) 
# then compare abs(t1) and abs(t2) with t-quantile qt(0.975,model2$df.resid)
# for example, abs(t1)>= qt(0.975,model2$df.resid), if TRUE, reject H0
# or compute the p-values 
2*(1-pt(abs(t1),model2$df.resid)); 2*(1-pt(abs(t2),model2$df.resid))
# you can also find p-values of summary(model2) but remember that those 
# p-values are computed by using normal(?) quantiles: 2*(1-pnorm(abs(t2)))


####### estimating phi ########

## if you need to estimate phi in some GLM model, you can use 2 estimates: 
## the Pearson estimator and the mean deviance estimator 
## the Pearson chi-squared test statistic P  
# P=sum(residuals(model,type="pearson")^2); P
## to compute the number of degrees of freedom of P
# model$df.resid # or df.residual(model) 
# phi=P/model$df.resid # the Pearson estimator of phi

# if phi is unknown, another way to compute the Pearson estimator of phi
summary(model)$dispersion # the same as P/df.residual(model), but if phi is known 
# summary(model)$dispersion will report phi, like in binomial model (phi=1) 
# phi is always given in summary(model): Pearson estimator or fixed value (if known)
# here e.g.: (Dispersion parameter for Gamma family taken to be 0.0195856)

# the mean deviance estimator of phi
# the residual deviance D can be found by
# D=sum(residuals(model,type="deviance")^2) # or D=deviance(model)
D/df.residual(model1)
# even if it is known that phi=1, we still can estimate phi by 
# P/model$df.resid or D/model$df.resid to check for overdispersion
# If this value deviate from 1 too much, one speaks of overdispersion


###### Testing by using the deviance table for melanoma data ######

# deviance table for the melanoma data with variables years and sunspots
anova(model0,test="Chisq") # sunspot is not relevant, drop it
# deviance table for the melanoma data only with variable years
anova(model1,test="Chisq") # covariate years is significant
## test reduced model model1 within the full model model0
anova(model1,model0,test="Chisq") 
# the same as the last p-value in anova(model0,test="Chisq")

# # estimate the mean number mu of the melanoma cases for years=2025,sunspots=55
# out=predict(model0,newdata=data.frame(years=2025,sunspot=55),se.fit=TRUE)
# exp(out$fit)
# # or out2=predict(model0,newdata=data.frame(years=2025,sunspot=55),se.fit=TRUE,type="response") 
# # then out2$fit must be the same as exp(out$fit)
# # compute an approximate 95% confidence interval for mu 
# ci.lo=exp(out$fit-qnorm(0.975)*out$se.fit) # for 95% CI
# ci.hi=exp(out$fit+qnorm(0.975)*out$se.fit)
# c(Lower=ci.lo,Estimate=exp(out$fit),Upper=ci.hi)

# the residual deviance D (very close to P in this case)
deviance(model1) 
# or sum(residuals(model1)^2) as default residuals are deviance residuals 

# AIC for the full model
AIC(model0) # which is -2*logLik(model0)+2*length(coef(model0))
# Compare with the AIC for the reduced model (a small improvement)
AIC(model0,model1)

##### Overdispersion for melanoma and byssinosis #######
D1=deviance(model1);D1;D1/df.residual(model1) #no overdispersion for melanoma
D2=deviance(model3);D2;D2/df.residual(model3) #no overdispersion for byssinosis


################# Toeslagfraude ####################

toeslagfraude=read.table("toeslagfraude.txt",header=T)
attach(toeslagfraude)
# sum(fraude) # fraud 65 out of 100, the sample from suspicious cases
xtabs(~fraude+friesland,data=toeslagfraude)
 
### Testing in logistic model
is.factor(toeslagfraude$friesland) #no
toeslagfraude$friesland=as.factor(toeslagfraude$friesland) # make it factor
# in this case it is not needed to make friesland factor, why?

fraudeglm=glm(fraude~toeslag+friesland,data=toeslagfraude,family=binomial)
anova(fraudeglm,test="F") # only p-value for friesland is relevant
fraudeglm=glm(fraude~friesland+toeslag,data=toeslagfraude,family=binomial)
anova(fraudeglm,test="Chisq") # now only p-value for toeslag is relevant
# look at the tests for individual coefficients
summary(fraudeglm) # tests for individual coefficients
# to see the order of levels of friesland: levels(toeslagfraude$friesland) 
# the coefficient for factor friesland: friesland1-friesland0=-2.01
# conclude that being from friesland leads to smaller probability of fraud
drop1(fraudeglm,test="Chisq") # both p-values are relevant
# to test for interaction between factor friesland and variable toeslag
fraudeglm2=glm(fraude~friesland*toeslag,data=toeslagfraude,family=binomial)
summary(fraudeglm2)
anova(fraudeglm2,test="Chisq") # or drop1(fraudeglm2,test="Chisq") 
# there is no interaction as it should be logically, back to additive model

