#### remainder: t-test for two population categories #####

# setwd("your directory") # fill in your directory where 
# you keep the data sets, e.g, "~/stat_models/Data Sets"
coffee<-read.table("C:/Users/Zalgo/Desktop/stat_mod/ass1/CoffeeData1.txt")

t.test(coffee$Amsterdam,coffee$Rotterdam)
y1=coffee$Amsterdam 
y2=coffee$Rotterdam 
pooledVar=(sum((y1-mean(y1))^2)+sum((y2-mean(y2))^2))/(length(y1)+length(y2)-2); pooledVar
#pooledVar=sum(var(y1)+var(y2))/2; pooledVar
t=(mean(y1)-mean(y2))/sqrt(pooledVar*(1/length(y1)+1/length(y2))); t
#t=sqrt(length(y1))*(mean(y1)-mean(y2))/sqrt(2*pooledVar); t
## p-value computed manually: 2*(1-pt(abs(t),length(y1)+length(y2)-2))


############# one-way ANOVA #############

sales=as.vector(unlist(coffee))
## city=as.factor(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10)))
city=factor(c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10)),
            labels =c("Amsterdam","Den Haag","Haarlem","Rotterdam","Utrecht"))
data=data.frame(sales,city)
boxplot(sales~city,data=data) # have a look at the boxplots per Location
model=lm(sales~city,data=data) # or aovmodel=aov(sales~city,data=data)
anova(model)
# coefficient estimates, by default for parametrization contr.treatment: alpha1=0 
# these are actually the estimates of (mu1,mu2-mu1,mu3-mu1,mu4-mu1,mu5-mu1) 
summary(model)
# the estimates of (mu1,mu2,mu3,mu4,mu5) for the standard parametrization mu=0
coef(model)+c(0,rep(coef(model)[1],4)) # or model.tables(aovmodel,"means")

## to get the estimates of mu,alpha1,alpha2,alpha3,alpha4,alpha5 
## for the sum-parametrization: sum alphai=0 run
# contrasts(data$city)=contr.sum
# model=lm(sales~city,data=data) 
# summary(model) # remember that the estimate of alpha5 is not shown


## checking the model assumptions
## normality
qqnorm(resid(model)) # qqline(resid(model),lty=3,col="red")
shapiro.test(resid(model)) # H0:normality
## homoscedasticity
plot(fitted(model),resid(model),xlab="fitted values",ylab="residuals", main="residuals vs fitted")
## plot(residuals(model)~fitted(model),xlab="fitted values",ylab="residuals",main="residuals vs fitted")
abline(h=0,lty=3,col="red")
## Bartlett's test of homogeneity (homoscedasticity) of variances
# H0 that all k population variances are equal  
# against the alternative that at least two are different.
bartlett.test(sales~city,data=data) # H0: homoscedasticity
 
# or, to get the first two diagnostic plot
# par(mfcol=c(1,2)); plot

#One-way ANOVA on ranks is a non-parametric method (no normality assumed)
#for testing whether samples originate from the same distribution.
kruskal.test(sales,city) # stat=33.027,df=4,p-value=1.179e-06



##################### two-way ANOVA #############

coffee<-read.table("CoffeeData2.txt",header=TRUE)
# sales increase from Sep to Aug
salesincr=as.vector(unlist(coffee)) ## or salesincr=as.vector(as.matrix(coffee)) 
location=factor(c(rep(1,15),rep(2,15),rep(3,15),rep(4,15),rep(5,15)),
           labels =c("Amsterdam","Den Haag","Haarlem","Rotterdam","Utrecht"))
strategy=factor(rep(c(rep(1,5),rep(2,5),rep(3,5)),5))
coffee=data.frame(salesincr,location,strategy)
### apply the right contrast to get the correct (sum) parametrization
### in terms of mu,a1,...aI,b1,..bJ, g11,...,gIJ
contrasts(coffee$location)=contr.sum 
contrasts(coffee$strategy)=contr.sum
model2=lm(salesincr~location*strategy,coffee)#=location+strategy+location:strategy
anova(model2) ## two-way ANOVA with interaction, additive model is rejected 

## we have estimates of mu, alpha's, beta's, gamma's in sum-parametrization 
## not all parameters are given, those which are not there, can be found via constrains.
summary(model2) ## or model2$coeff 

## Another way to get the right parameter estimates (without contrast commands)
# aovmod=aov(salesincr~location*strategy)
# model.tables(aovmod) # estimates for alpha's, beta's and gamma's
## the grand mean mu is found as follows
# mean(salesincr)


#### two-way ANOVA: testing interaction in the general way
## by direct comparing two models
## General F-test: H0: small (sub)model mod1 versus H1: big model mod2 
## mod1 must be a submodel of mod2
## mod1=lm(...); mod2=lm(...); anova(mod1,mod2)

mod.full=lm(salesincr~location*strategy)   # full model
anova(mod.full) # last line is relevant, test for interaction
## H0: no interaction versus H1: there is an interaction 
## H0: smaller model holds versus H1: smaller model is not true 
mod.add=lm(salesincr~location+strategy) # additive model
anova(mod.add,mod.full) # another (general) way to test for interaction

###### two-way ANOVA: additive model
### we work with the additive model although it was rejected 
anova(mod.add) # both factors in add. model are significant
# by using the general approach:
mod1=lm(salesincr~location) # smaller model: no strategy
mod2=lm(salesincr~strategy)  # smaller model: no location  
mod3=lm(salesincr~location+strategy) # big model: location+strategy  
anova(mod1,mod.add) # testing: is factor strategy important?
anova(mod2,mod.add) #testing: is factor location important?



### ANCOVA: besides two factors, add one more continuous variable area

## area (of the outlet) is a fake variable: we create it ourselves
# coffee$area=rnorm(75,12,4) # this variable should not have any effect
# create variable area that should have an effect: the more area the more sales
coffee$area=c(rep(rnorm(1,10,2),15),rep(rnorm(1,8,2),15),rep(rnorm(1,9,2),15),
              rep(rnorm(1,11,2),15),rep(rnorm(1,7,2),15))+salesincr*0.01+rnorm(75)
plot(salesincr~coffee$area,pch=as.character(coffee$location))

## Checking whether factor location is important (ignore factor strategy)
## while taking into account also variable area  
modB=lm(salesincr~area+location,data=coffee) # Location second!
# compare with anova(lm(salesincr~location+area,data=coffee))
anova(modB)
# or by using drop1
drop1(modB,test="F")
# when using drop1, the p-values shown correspond to removing one variables 
# at a time from the full model, whereas the p-vales in the output 
# of ANOVA sort of sequential, like in step-up strategy. 
# This problem does not arise in the balanced ANOVA, 
# but it does in the unbalanced ANOVA and in ANCOVA.
# The order of variables is important in ANCOVA! More on this is below.

# Testig for interaction between contin. variable area and factor location. 
modA=lm(salesincr~area*location,data=coffee) 
anova(modA)  # only the interaction line is relevant: H0 is rejected
# an interaction between area and location
# slopes are not the same = not parallel regression lines for all locations
# The F-test one line earlier indicates whether you can 
# subsequently remove location, and the one in the first line to 
# removing area, leaving an empty model.
# Alternatively, you can read the table from top to bottom as adding 
# terms describing more and more of the total sum of squares.

# in additive model, one can test area without removing location
# that corresponds  to testing that the two  parallel regression lines 
# can be assumed horizontal. 
modC=lm(salesincr~location+area,data=coffee) # now area second
anova(modC) # compare to anova(modB)



### When does the order of the factors in AN(C)OVA model matter? ## 

### the order of factors does not matter in ANOVA with balanced design
y=rnorm(8) # some fictive response variable
f1=factor(c(1,1,1,1,2,2,2,2)) # factor f1 
f2=factor(c(1,2,3,4,1,2,3,4)) # factor f2
mod1=lm(y~f1+f2)
mod2=lm(y~f2+f1) # change the order of factors in the model formula
anova(mod1) # balanced design for anova
anova(mod2) # order of factors doesn't matter

### the order of factors in ANOVA matters when design is unbalanced
f1=factor(c(1,1,1,1,2,2,2,2)) # factor f1 
f2a=factor(c(1,2,3,1,2,3,1,2)) # new factor f2, leading to unbalanced design  
mod3=lm(y~f1+f2a) # the same MeanSq for f1 as in anova(lm(y~f1)) 
mod4=lm(y~f2a+f1) # unbalanced additive anova
anova(mod3) # the p-value for f2 (second factor in the formula) is correct
anova(mod4) # the p-value for f1 *second factor in the formula) is correct
## we see that the order of factors in unbalance anova matters
# for mod3: the first line f1 is test omega1(f1) within Omega(f1+f2) 
# the second line f2 is the test omega2(f2\f1) within Omega(f1+f2)
# the second line f2 is the right test for f2, with f1 taken into account 
# for unbalanced designs omega2(f2\f1) is not the same as omega2(f2)
# the best way to perform the test for f2 is 
modf1=lm(y~f1); anova(modf1,mod3) # or anova(modf1,mod4) 

### the order of factors in ANCOVA matters 
x3=rnorm(8,0.2,0.8)
mod1a=lm(y~f1+f2+x3) # x3 is last when testing for the relevance of x3
mod2a=lm(y~x3+f1+f2) # f2 is last when testing for the relevance of f2
# the p-values for f1, f2 and x3 are different for mod1a and mod2a
anova(mod1a) # although the design is balanced
anova(mod2a) # the order of factors matters in ancova

# the same relevant p-values can be derived by testing submodels within 
# the full model. The relevant p-value for x3  
mod1b=lm(y~f1+f2) # submodel (without x3) of the model mod1a=lm(y~f1+f2+x3)
anova(mod1b,mod1a) # relevant p-value for x3, anova(mod1b,mod2a) gives the same
# the relevant p-value for f2  
mod2b=lm(y~f1+x3) # create submodel without f2 inside mod2a=lm(y~x3+f1+f2)
anova(mod2b,mod2a) # relevant p-value for f2, anova(mod2b,mod1a) gives the same
# to get all these relevant p-values at once, just run 
drop1(mod1a,test="F") # this works for all the models anova,ancova


## Conclusions:
## order of factors does not matter in anova with balanced design
## order of factors in ancova matters 
## order of factors in anova matters when design is unbalanced



### Example: lasso for the data set mtcars (show this one at the lecture) #####

mtcars # dataset mtcars: mpg is the response
x=as.matrix(mtcars[,-1])
y=mtcars[,1]

train=sample(1:nrow(x),0.67*nrow(x)) # train by using 2/3 of the x rows 
x.train=x[train,]; y.train=y[train]  # data to train
x.test=x[-train,]; y.test = y[-train] # data to test the prediction quality

# Prediction by using the linear model
# first fit linear model on the train data
lm.model=lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars,subset=train)
y.predict.lm=predict(lm.model,newdata=mtcars[-train,]) # predict for the test rows
mse.lm=mean((y.test-y.predict.lm)^2); mse.lm # prediction quality by the linear model

# Now apply lasso for selecting the variables and prediction 
library(glmnet) 
lasso.model=glmnet(x.train,y.train,alpha=1) # alpha=1 for lasso
#more options: standardize=TRUE, intercept=FALSE,nlambda=1000
lasso.cv=cv.glmnet(x.train,y.train,alpha=1,type.measure="mse",nfolds=5)
# option nfolds=5 means 5-fold cross validation. By default, the method 
# performs 10-fold cross validation to choose the best lambda.
# plots
plot(lasso.model,label=T,xvar="lambda") #standardize=T,type.coef="2norm",xvar="norm") "coef"
#plot(lasso.cv$glmnet.fit,xvar="lambda",label=T) # the same plot
plot(lasso.cv) 
plot(lasso.cv$glmnet.fit,xvar="lambda",label=T)
# With label="T" in plot commando you see which curve corresponds 
# to which coefficients. The glmnet plot above shows the shrinkage of 
# the lasso coefficients as you move from the right to the left, 
# but unfortunately, it is not clearly labelled. 
# Lasso contrasts with ridge regression, which flattens out 
# everything, but does not zero out any of the regression coefficients.

lambda.min=lasso.cv$lambda.min; lambda.1se=lasso.cv$lambda.1se; 
lambda.min; lambda.1se # best lambda by cross validation
coef(lasso.model,s=lasso.cv$lambda.min) # cyl,hp,wt,am and carb are relevant
coef(lasso.model,s=lasso.cv$lambda.1se) # only cyl,hp and wt are releveant

# lambda.min is the value of lambda that gives minimum mean cross-validated 
# error. The other lambda saved is lambda.1se, which gives the most regularized 
# model such that error is within one standard error of the minimum. 

lasso.pred1=predict(lasso.model,s=lambda.min,newx=x.test) 
lasso.pred2=predict(lasso.model,s=lambda.1se,newx=as.matrix(x.test))
mse1.lasso=mean((y.test-lasso.pred1)^2); mse1.lasso
mse2.lasso=mean((y.test-lasso.pred2)^2); mse2.lasso

# By default, the glmnet function standardizes all the independent 
# variables, but here the dependent variable can also be standardized 
# by the function standardize=function(x){(x-mean(x))/sd(x)}).
# Then one may want not to include an intercept in lm mand glmnet models, 
# because all the variables have already been standardized to a mean of zero. 



######### Multiple testing procedures for the coffee data ##########

model2=lm(salesincr~location*strategy,coffee) 
anova(model2); summary(model2)
# The p-values produced above with anova and summary are not simultaneous. 
# For example, the p-values in the lines locationCity are for the hypotheses 
# H0: alpha2=alpha1,...,H0: alpha5=alpha1 for the main effects of location.

## Let us perform Tukey's procedure for the factor location 
# all pairwise comparisons between all levels of factor location
library(multcomp)
coffee.mult=glht(model2,linfct=mcp(location="Tukey"))
summary(coffee.mult) 
## these are simultaneous p-values for all the null hypothesis 
# H0: beta2=beta1, H0: beta3=beta1,... H0: beta5=beta4, where 
# betaj is the main effect of the j-th level of factor location.
# We can "safely" say that ALL differences with p-values <0.05 are nonzero.


########## Next we apply the Bonferroni, Holm, Hochberg, BH and BY procedures 
# to all the p-values from the model model2
p.raw=summary(model2)$coef[,4] 
# there are all p-values for the model model2, if desired one 
# can restrict oneself to p-values of one particular factor
p.raw=p.raw[order(p.raw)]; p.val=as.data.frame(p.raw)
p.val$Bonferroni=p.adjust(p.val$p.raw,method="bonferroni")
p.val$Holm=p.adjust(p.val$p.raw,method="holm")
p.val$Hochberg=p.adjust(p.val$p.raw,method="hochberg")
p.val$BH=p.adjust(p.val$p.raw,method="BH")
p.val$BY=p.adjust(p.val$p.raw,method="BY")
# now put all the adjusted p-values (according to 5 methods) in one table
round(p.val,3) 

