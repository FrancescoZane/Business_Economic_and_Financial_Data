#### Lab 2: Nonlinear regression, ARIMA models, ARMAX models, Exponential Smoothing ####

###########################################
####            Libraries              ####
###########################################

library(readxl)
library(DIMORA)

###########################################
#### 1. German energy transition case  ####
###########################################

# read the data
bp <- read_excel("BP1.xlsx")
str(bp)

# we consider nuclear and renewable
GN <- bp$GermanyN[1:55]
GR <- bp$GermanyR[28:55] 

# plots
plot(GN, type="b", xlab="Year", ylab="Annual consumption", pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])

plot(GR, type="b", xlab="Year", ylab="Annual consumption", pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])

# we Perform a competition study between nuclear and renewable
year <- bp$year[1:55]

# make a plot of the two series
plot(year,GN,xlab="year", ylab="Consumption (Mtoe)", type= "b", pch=16, lty=3, cex=0.7)
points(year[28:55],GR, type= "b", pch=16, lty=3, cex=0.7, col=2)

# estimate the UCRCD (with delta and gamma)
ucrcdNR <- UCRCD(GN,GR, display=T)
summary(ucrcdNR)    # almost all variables are significance --> good news for us because this means that indeed 
                    # there is a relationship between the two time series

coef(ucrcdNR)    # since q1c and q2-gamma are both negative here we have full competition

# we make a plot of the UCRCD model (used for explanatory proposes)
plot(year, GN, xlab="year", ylab="Consumption (Mtoe)", type= "b", pch=16, lty=3, cex=0.7)
points(year[28:55], GR, type= "b", pch=16, lty=3, cex=0.7, col=2)

lines(year[1:55], (ucrcdNR$fitted.i[[1]]), lwd=2, col=1)
lines(year[28:55], (ucrcdNR$fitted.i[[2]]), lwd=2, col=2)

# we also add a line and some text within the plot
abline(v=1991, lty=2)
text(1991, 0.8, pos=4, "renewables enter in 1992")


###########################################
#### 2.Competition between cassettes   ####
####   and CDs                         ####
###########################################

music <- read_excel("music.xlsx")

colors <- grey(seq(0, 1, length=5))

year <- music$year[1:36]
cass <- music$cassette[1:36]
cd <- music$cd[10:36]

# exploratory plots with instantaneous data
plot(year[10:36], cass[10:36], xlab="Year", ylab="Annual sales", 
     type= "b", pch=16, lty=3, cex=0.7, ylim=c(0,1000), col=colors[1])
points(year[10:36], cd, type= "b", pch=2, lty=3, cex=0.7, col=colors[3])
legend("topleft", legend=c("Cassette", "CD"), pch=c(16,2), col=c(colors[1], colors[3]))

# cumulative
plot(year[10:36], sum(cass[1:9])+cumsum(cass[10:36]), xlab="Year", ylab="Cumulative sales",
     type= "b", pch=16, lty=3, cex=0.7, ylim=c(0,15000), col=colors[1])
points(year[10:36], cumsum(cd), type= "b", pch=2, lty=3, cex=0.7, col=colors[3])
legend("topleft", legend=c("Cassette", "CD"), pch=c(16,2), col=c(colors[1], colors[3]))

# UCRCD model with delta and gamma (par = "double")
ucrcd <- UCRCD(cass, cd, display=T)
summary(ucrcd)
coef(ucrcd)    # in this case q1c < 0 while (q2-gamma) > 0 this means that cd competes with cassette while cassette 
               # collaborates with cd

# plot observed vs fitted
# instantaneous data
plot(year[10:36], cass[10:36], xlab="Year", ylab="Annual sales", type= "b", pch=16, lty=3, cex=0.7,
     ylim=c(0,1000), col=colors[1])
points(year[10:36], cd, type= "b", pch=2, lty=3, cex=0.7, col=colors[3])
lines(year[10:36], (ucrcd$fitted.i[[1]])[10:36], lwd=1, col=colors[1])
lines(year[10:36], (ucrcd$fitted.i[[2]]), lwd=1, col=colors[3])
legend("topleft", legend=c("Cassette", "CD"), lty=1, lwd=2, col=colors[c(1,3)], pch=c(16,2), cex=0.7)

# cumulative data
plot(year[10:36], sum(cass[1:9])+cumsum(cass[10:36]), xlab="Year", ylab="Cumulative sales",
     type= "b", pch=16, lty=3, cex=0.7, ylim=c(0,15000), col=colors[1])
points(year[10:36], cumsum(cd), type= "b", pch=2, lty=3, cex=0.7, col=colors[3])
lines(year[10:36], cumsum(ucrcd$fitted.i[[1]])[10:36], lwd=1, col=colors[1])
lines(year[10:36], cumsum(ucrcd$fitted.i[[2]]), lwd=1, col=colors[3])
legend("topleft", legend=c("Cassette", "CD"), lty=1, lwd=2, col=colors[c(1,3)], pch=c(16,2), cex=0.7)


###############################################################################################################################
###############################################################################################################################

###########################################
####            Libraries              ####
###########################################

library(fpp2)
library(forecast) 

###########################################
####            ARIMA models           ####
###########################################

###########################################
#### 1. Data on quarterly percentage   ####
####    change in US consumption,      ####
####    income, production, savings,   ####
####    unemployment                   ####
###########################################

uschange <- uschange
str(uschange)
plot(uschange)        # they are already stationary
autoplot(uschange)    #different way of seeing the same series

# consider the series of consumption
cons <- uschange[,1]
plot(cons)
Acf(cons)
Pacf(cons)
consts <- tsdisplay(cons)
# general indication: if the ACF is exponentially decaying or sinusoidal and there is a significant spike at lag p in PACF
# and nothing else, it may be an ARMA(p,d,0). If the PACF is exponentially decaying or sinusoidal and there is a significant
# spike at lag p in ACF and nothing else, it may be an ARMA(0,d,q). 

arima1 <- Arima(cons, order=c(0,0,3))
summary(arima1)

resid1 <- residuals(arima1)
tsdisplay(resid1)

plot(cons)
lines(fitted(arima1), col=2)

for1 <- forecast(arima1)
plot(for1)


###########################################
#### 2. Data on retail trade index in  ####
####    Euro area (1996-2011)          ####
###########################################

plot(euretail, ylab="retail index", xlab="year") # they are quarterly data
tsdisplay(euretail)

# try to stationarize the time series:
# first difference
diff1 <- diff(euretail) 
tsdisplay(diff1)          # we can notice that seasonality are still here

# seasonal difference
diff4 <- diff(euretail, lag=4) # to obtain seasonal differentiation (in this case with step 4)
tsdisplay(diff4)

# first seasonal Arima model 
a1 <- Arima(euretail, order=c(0,1,1), seasonal=c(0,0,1))
fit1 <- fitted(a1)

plot(euretail)
lines(fit1, col=2)

f1 <- forecast(a1)
plot(f1)            # not very well, there is a shift in the forecasted values

r1 <- residuals(a1)
tsdisplay(r1) 

# second Arima model
a2 <- Arima(euretail, order=c(0,1,1), seasonal=c(0,0,2))
fit2 <- fitted(a2)

plot(euretail)
lines(fit2, col=2)

f2 <- forecast(a2)
plot(f2)

r2 <- residuals(a2)
tsdisplay(r2) 

# third Arima model
a3 <- Arima(euretail, order=c(0,1,1), seasonal=c(0,1,1))
fit3 <- fitted(a3)

plot(euretail)
lines(fit3, col=2)

f3 <- forecast(a3)
plot(f3)

r3 <- residuals(a3)
tsdisplay(r3) 

# fourth Arima model 
a4 <- Arima(euretail, order=c(0,1,2), seasonal=c(0,1,1))
fit4 <- fitted(a4)

plot(euretail)
lines(fit4, col=2)

f4 <- forecast(a4)
autoplot(f4)

r4 <- residuals(a4)
tsdisplay(r4) 

# fifth Arima model 
auto.a <- auto.arima(euretail)
auto.a

forecasted_values <- forecast(auto.a)$fitted
plot(euretail)
lines(forecasted_values, col=2)

autoplot(forecast(auto.a))
checkresiduals(auto.a)


###########################################
#### 3. Cortecosteroid drug sales in   ####
####    Australia                      ####
###########################################

lh02 <- log(h02)

# plot the data
plot(h02, ylab="sales", xlab="year")

# plot log transformation
plot(lh02, ylab="logsales", xlab="year")

# plot of seasonal differentiated data, with ACF and PACF
tsdisplay(diff(lh02, 12), main="seasonal differenced data", xlab="year")

# we fit an ARIMA model base on the inspection of ACF and PACF with 3 AR components
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
summary(fit)

# check residuals
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")

# perform the forecasting
f <- forecast(fit)
plot(f, ylab="sales", xlab="year")


###########################################
####            ARMAX models           ####
###########################################

# we want to extend the ARIMA by combining the regression model and ARIMA model to obtain regression with ARIMA errors
# note: forecasts are available only if we have future values of 'income'

###########################################
#### 1. data on US personal            ####
####    consumption and income         ####
###########################################

uschange
plot(uschange)       # 'classical view'
autoplot(uschange)   # 'all the series together'

# if we want to see just the first two series
par(mfrow=c(2,1))
plot(uschange[,1])
plot(uschange[,2])

tsdisplay(uschange[,1])

# to go back to 1 panel 
par(mfrow=c(1,1))


# estimate an ARMAX model
# model 1
armax1 <- Arima(uschange[,1], xreg=uschange[,2], order=c(1,0,1))
res1 <- residuals(armax1)
Acf(res1)

fitted(armax1)
plot(uschange[,1])
lines(fitted(armax1), col=2)

AIC(arima1)

# model 2
armax2 <- Arima(uschange[,1], xreg=uschange[,2], order=c(1,0,2))
res2 <- residuals(armax2)
Acf(res2)

fitted(armax2)
plot(uschange[,1])
lines(fitted(armax2), col=2)

AIC(armax2)

# procedure also available with auto.arima
auto.arima <- auto.arima(uschange[,1], xreg=uschange[,2])

res_auto.arima <- auto.arima$residuals 
Acf(res_auto.arima)

plot(uschange[,1])
lines(auto.arima$fitted, col=2)

auto.arima$aic


###########################################
#### 2. Forecasting electricity demand ####
###########################################

elecdaily
plot(elecdaily)

# fit a quadratic regression model with ARMA errors using the auto.arima function
# demand = a1 * Temperature + a2 * Temperature^2 + a3 * WorkDay
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])

fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)

checkresiduals(fit)              # the model has some significant autocorrelation in the residuals

# using the estimated model we forecast 14 days ahead, setting for the next 14 days a temperature at a constant level of 26.
fcast <- forecast(fit, xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14), Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))

plot(fcast)


###########################################
#### 3. Quarterly international        ####
####    arrivals (in thousands) to     ####
####    Australia from Japan, New      ####
####    Zealand, the UK and the US.    ####
####    1981Q1 - 2012Q3                ####
###########################################

?arrivals
autoplot(arrivals)
autoplot(arrivals[,c(1,2)])

Japan <- arrivals[,1]
NZ <- arrivals[,2]
UK <- arrivals[,3]
US <- arrivals[,4]

# we try with a simple arima model, ARMAX
auto.a <- auto.arima(NZ, xreg=Japan) 
summary(auto.a)
AIC(auto.a)

# we try with a regression model with trend, season, and external variable 'Japan'
mod <- tslm(NZ ~ trend+season+Japan) 
summary(mod)
fitted(mod)
plot(NZ)
lines(fitted(mod), col=2)
plot(residuals(mod))

# analysis of residuals: autocorrelation? 
tsdisplay(residuals(mod))

# fit an arima model to residuals
aar <- auto.arima(residuals(mod))
fitted(aar)

# complete the analysis by summing predictions made with linear model and arma on residuals
plot(NZ)
lines(fitted(mod)+fitted(aar), col=3)

# comparison
plot(NZ)
lines(fitted(mod), col=2)
lines(fitted(mod)+fitted(aar), col=3)

# notice the difference between the two methods: ARMAX and linear regression + Arima on residuals

# another way of performing the same linear regression

tt <- (1:length(NZ))
seas <- factor(c(rep(1:4,length(NZ)/4),1:3)) #1:3 because there are three observations 'out' 
mod2 <- lm(NZ~ tt+seas+Japan)
summary(mod2)
AIC(mod2)
AIC(mod)


###########################################
####        Lagged Predictors          ####
###########################################

###########################################
#### 1. Insurance quotations and       ####
####    advertising datal              ####
###########################################

plot(insurance, main="Insurance advertising and quotations", xlab="year", xaxt="n")
axis(1, at=c(2002,2003,2004,2005),labels=c("2002","2003","2004","2005"), line=-1, lwd=0, lwd.ticks=0.5)

# we consider 1 lagged predictor
advert <- cbind(insurance[,2], c(NA,insurance[1:39,2]))
colnames(advert) <- paste("AdLag", 0:1, sep="")
fit <- auto.arima(insurance[,1], xreg=advert[,1:2],d=0)
summary(fit)

# we provide the forecast by assuming that advertising is 8 units in each future month
f <-forecast(fit,h=20, xreg=cbind(AdLag0=rep(8,20), AdLag1=c(advert[40,1], rep(8,19))))
plot(f)


###########################################
####             SARMAX               ####
###########################################

###########################################
#### 1. Twitter Revenues               ####
###########################################

twitter <- read_excel("twitter.xlsx")
length(twitter$twitter)
tw <- (twitter$twitter)

# GGM 
GGM_tw <- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
summary(GGM_tw)

pred_GGM_tw <- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst <- make.instantaneous(pred_GGM_tw)

plot(tw, type="b", xlab="Quarter", ylab="Quarterly revenues", pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)
lines(pred_GGM_tw.inst, lwd=2, col=3)

# analysis of residuals
res_GGMtw <- residuals(GGM_tw)
acf <- acf(residuals(GGM_tw))

fit_GGMtw <- fitted(GGM_tw)
fit_GGMtw_inst <- make.instantaneous(fit_GGMtw)

# SARMAX model with external covariate 'fit_GGM' 
s2 <- Arima(cumsum(tw), order=c(3,0,1), seasonal=list(order=c(3,0,1), period=4), xreg=fit_GGMtw)
summary(s2)    # the lambda parameter here is call xreg
pres2 <- make.instantaneous(fitted(s2))

plot(twitter$twitter, type="b", xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
lines(fit_GGMtw_inst, lwd=1, lty=2)
lines(pres2, lty=1,lwd=1, col=2)

###########################################
#### 2.Interest in Zoom according to   ####
####   searches in Google              ####
###########################################

interest <- read.csv("zoomgoogle.csv")
int <- interest$zoom[90:180]
week <- as.Date(interest$week[90:180])
# we set a 'timing' more refined for graphical purposes only
tfine150 <- seq(1,150,0.01) 

# exploratory plots
# instantaneous 
plot(int, type= "b",xlab="Week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))

# cumulative
plot(cumsum(int), type= "b",xlab="Week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))

# GBM with one exponential shock
gbme1.go <- GBM(int,shock = "exp",nshock = 1,prelimestimates = c(4.046997e+03, 0.001, 0.1, 30,-01,5))
# prediction with GBMe1
pred.gbme1go<- predict(gbme1.go, newx=tfine150)
pred.gbme1goi<- make.instantaneous(pred.gbme1go)[-1]

# Plots observed vs predicted
plot(int, type= "b", xlab="Week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=colors[3])
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(tfine150[-1], pred.gbme1goi*100, lwd=2, col=1)

plot(cumsum(int), type= "b",xlab="Week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n",col=colors[3])
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(tfine150, pred.gbme1go, lwd=2, col=1)

# SARMAX refinement
fit.google <- fitted(gbme1.go)
s2 <- Arima(cumsum(int), order = c(1,0,1), seasonal=list(order=c(0,0,1), period=52), xreg = fit.google)
summary(s2)

pres2 <- make.instantaneous(fitted(s2))

# plots observed vs predicted with SARMAX refinement
plot(int, type= "b",xlab="week", ylab="Weekly Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=colors[3])
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(tfine150[-1], pred.gbme1goi*100, lwd=1, lty=2)
lines(pres2, lty=1,lwd=1)
legend("topright", legend=c("GBMe1","GBMe1+SARMAX"), lty=c(2,1))

plot(cumsum(int), type= "b",xlab="week", ylab="Cumulative Google searches",  pch=16, lty=3, cex=0.6, xaxt="n", col=colors[3])
axis(1, at=c(1,18,36,54,72,90), labels=format(week[c(1,18,36,54,72,90)], "%d/%m/%y"))
lines(tfine150, pred.gbme1go, lwd=1, lty=2)
lines(cumsum(pres2), lty=1,lwd=1)
legend("bottomright", legend=c("GBMe1","GBMe1+SARMAX"), lty=c(2,1))























##############################################################################
##############################################################################
##############################################################################
##Exponential Smoothing methods

##1.Simple exponential smoothing
oildata<- window(oil, start=1996)
autoplot(oildata)+ylab("Oil (millions of tonnes)")+xlab("Year")

fit1<- ses(oildata, alpha=0.2, initial="simple", h=5)
fit2<- ses(oildata, alpha=0.6, initial="simple", h=5)
fit3<- ses(oildata, h=5)


plot(oildata, ylab="Oil", xlab="Year")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")

fc<- ses(oildata, h=5)
round(accuracy(fc), 2)

summary(fc)

autoplot(fc)+
  autolayer(fitted(fc), series="Fitted")+ylab("Oil (millions of tonnes)")+xlab("Year")


##2.Trend methods (Holt method)

air<- window(ausair, start=1990)
fc<- holt(air, h=15)
fc2<- holt(air, damped=T, phi=0.9, h=15)

autoplot(air)+
  autolayer(fc, series="Holt's method", PI=F)+
  autolayer(fc2, series="Damped Holt's method", PI=F)

###3.Trend and seasonality methods (Holt-Winters method)
aust<- window(austourists, start=2005)
autoplot(aust)

fit1<- hw(aust, seasonal="additive")
fit2<- hw(aust, seasonal="multiplicative")

autoplot(aust)+
  autolayer(fit2, series="Holt-Winters' method", PI=F)

