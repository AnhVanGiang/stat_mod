################# Lecture 8 ###############

# The multivariate time series EuStockMarkets (included in the 
# standard R-installation) gives the daily closing prices of four major 
# European stock indices (German DAX, Swiss SMI, French CAC and British FTSE) 
# from 1991 to 1998, in business time (excludes weekends and holidays).   
# Plot data
plot(EuStockMarkets)


## population in the US in millions
population=scan("population.txt")/10^6
year=seq(1790,2000,by=10)
plot(year,population,t="b",pch=20,lty=1,xlab="Year",
     ylab="Population (in millions)",main="U.S. Population")

## Accidental deaths in the US
# accidents=scan("accidents.txt") #actually build-in data set USAccDeaths
# plot(1:72,accidents,t="l",xlab="Month in period from 1973 to 1978",
# ylab="Number of accindental deaths",main="Accidental Deaths in the U.S.")
# abline(v=c(12,24,36,48,60),lty=3)
## or much better 
plot(USAccDeaths)


## Trend removal for the time series US population
yearsq=year^2
trend=lm(population~year+yearsq); trend$coef
plot(year,population,t="b",pch=20,lty=1,xlab="Year",
     ylab="Population (in millions)",main="U.S. Population")
lines(year,trend$coef[1]+trend$coef[2]*year+trend$coef[3]*yearsq,col="blue")
pop.detr=population-(trend$coef[1]+trend$coef[2]*year+trend$coef[3]*yearsq)
plot(year,pop.detr,t="l",
     xlab="Time",main="Detrended population (in millions)")

## Seasonability removal for the accidental death time series
# des.accidents=diff(accidents,lag=12)
# plot(13:72,des.accidents,t="l",xlab="Month in period from 1973 to 1978",
#      main="Deseasonalized number of accindental deaths")
plot(diff(USAccDeaths,lag=12)) 
# because of differencing with lag 12, no data for 1973  
# there seems to be a trend there


# ## estimating trend and seasonal components by R-command decompose
# accidents.ts=ts(accidents, start = c(1973,1), freq=12)
decomp=decompose(USAccDeaths)
plot(decomp); decomp$figure
# for example detrended and deseasonalized component is
plot(decomp$random)
plot(decomp$x-decomp$trend) # detrended component


################## Lecture 9 #################


par(mfrow=c(3,1))
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.8")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.8)
lines(SM,col="red",lwd=2)
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.5")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.5)
lines(SM,col="red",lwd=2)
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.2")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.2)
lines(SM,col="red",lwd=2)

par(mfrow=c(1,1))
plot.ts(sunspots[1:1000],ylab="Sunspots",main="spar=0.5")
SM=smooth.spline(1:1000,sunspots[1:1000],spar=0.5)
lines(SM,col="red",lwd=2)

Residuals=sunspots[1:1000]-SM$y
plot.ts(Residuals)



par(mfrow=c(1,3))

ma.model=list(ma=c(0.7,-1.2))
ma=arima.sim(100,model=ma.model,sd=sqrt(0.16))
plot(ma,ylab="",main="MA(2),b1=0.7,b2=-1.2,sgm=0.4")

ar.model=list(ar=-0.9)
ar=arima.sim(100,model=ar.model,sd=sqrt(0.81))
plot(ar,ylab="",main="AR(1), a1=-0.9,sgm=0.9")

arma.model=list(ar=c(-0.1,0.8),ma=1.4)
arma=arima.sim(100,model=arma.model,sd=sqrt(2.25))
plot(arma,ylab="",main="ARMA(2,1), a1=-0.1,a2=0.8,b1=1.4,sgm=1.5")

par(mfrow=c(1,1))

acf(ma)
ma.mle=arima(ma,order=c(0,0,2),method="ML",include.mean=F)
ma.mle
Box.test(resid(ma.mle),type="Ljung-Box")$p.value 
#Box.test(ma,type="Ljung-Box")$p.value 

# Use the model ma.mle to predict future values of the time series 
pred=predict(ma.mle,n.ahead=6) # for details: ?predict.Arima
plot(pred$pred)

acf(ar,type="partial") #PACF
ar.mle=arima(ar,order=c(1,0,0),method="ML",include.mean=F)
ar.mle # this is ML estimate of parameter of AR model
# Yule-Walker estimates (better then MLE?)
ar(ar) # or ar(ar,order.max=1), the same as ar.yw(ar,order.max=1)

arma.mle=arima(arma,order=c(2,0,1),method="ML",include.mean=F)
arma.mle
# Sample ACF of the residuals
acf(resid(arma.mle), main="Sample ACF of the residuals")
Box.test(resid(arma.mle),type="Box-Pierce")$p.value # the Portmanteau test
# Box.test(arma,type="Box-Pierce")$p.value


## Moving Average Convergence/Divergence

# A trading strategy: MACD (Moving Average Convergence Divergence). 
# In a moving average crossovers strategy two averages (usually of closing 
# prices) are computed, a slow moving average and a fast moving average. 
# The difference between the fast moving average and slow moving average is 
# called MACD line. A third average called signal line, a 9 day exponential 
# moving average of MACD signal, is also computed. If the MACD line crosses 
# from below to above the signal line then it is a bullish sign (buy signal) 
# and we go long. If the MACD line crosses from above to below the signal 
# line then it is a bearish sign (sell signal) and we go short.

# Load R packages
library('TTR')
library('quantmod')

getSymbols("TSLA",from='2021-10-19',to='2022-11-23')
stock=TSLA
# # To extract columns, use Op, Hi, Lo, Cl, Vo and Ad
# Open <- Op(stock) # Open price
# High <- Hi(stock) # High price
# Low <- Lo(stock) # Low price
# Close<- Cl(stock) # Closing price # we use this one
# Volume <- Vo(stock) # Volume
# AdjClose <- Ad(stock) # Adjusted close

# MACD stock technical indicator calculation for the closing price Cl(stock)
macd<-MACD(Cl(stock),nFast=12,nSlow=26,nSig=9,percent=FALSE); macd
## The percent is whether MACD is in percentage form or in difference form
## nFast, nSlow, and nSig are resp. S, L, and K in the formula for MACD. 

n.na=sum(is.na(macd$signal)) # 33=26-1+9-1, the number of NA's in macd$signal
# the first time moment when both macd and signal are well defined is
t1=time(macd[n.na+1,]); t1 #=time(macd[nSlow+nSig-1,]) # row 34=26+9-1 
macd2=macd[time(macd)>=t1] # macd2 starting from the date t1

# extract the closing stock prices for trading starting per time t1
price<-as.numeric(Cl(stock[time(stock)>=t1])); n=length(price)
gain1=price[n]-price[1]; gain1 # gain without using any trading strategy

# when do we wait to buy, wait to sell, when to buy, when to sell
buy<-as.numeric(ifelse(macd2$macd < macd2$signal,1,-1)); buy
# 1=waiting to buy,-1=waiting to sell 
# switch from 1 to -1 is the moment to buy; from -1 to 1 is the moment to sell

## Charting 
# chartSeries(macd2$macd); chartSeries(macd2$signal)
plot(Cl(stock))  # plot of the closing prices of the stock
plot(macd2$macd) # plot of MACD line
lines(macd2$signal,col="blue") # plot of Signal line

chartSeries(stock,type="line",theme=chartTheme('white'),TA="addMACD()") 
# some more possibilities to plot 
# chartSeries(stock) #subset='2021-11::2022-11',subset='2007',TA=NULL no volume
# chartSeries(stock,TA="addMACD()") # chart of the stock with MACD
# addSMA(n=30,on=1,col="blue"); addSMA(n=200,on=1,col="red") # add SMA plots
# barChart(stock,theme=chartTheme('white'),TA=NULL) # bar chart
# addMACD(fast=12,slow=26,signal=9) #type="EMA") # add MACD to it




