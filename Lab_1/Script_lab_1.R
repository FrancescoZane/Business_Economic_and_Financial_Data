####Lab 1: Linear regression and nonlinear regression####

###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
##############################################

##some plots for time series##################
plot(a10, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")
seasonplot(a10, ylab="million dollars", xlab="Year", main="Seasonal plot: Antidiabetic drugs", year.labels=T, year.labels.left=T, col=1:20, pch=19)

###########################################
#### Linear regression for time series ####
####           Facebook example        ####
###########################################

###read the data
facebook<- read_excel("facebook.xlsx")
str(facebook)
##create a variable 'time'
tt<- 1:NROW(facebook)

#create the variable 'fb'
fb <- facebook$fb

##make a plot
plot(tt, fb, xlab="Time", ylab="Facebook users")

##acf of variable "fb"

acf(fb)

##fit a linear regression model 
fit1 <- lm(fb~ tt)
summary(fit1)

##plot of the model
plot(tt, fb, xlab="Time", ylab="Facebook users")
abline(fit1, col=3)

##check the residuals? are they autocorrelated? Test of DW
dwtest(fit1)

##check the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )


##let us do the same with a linear model for time series, so we transform the data into a 'ts' object
fb.ts <- ts(fb, frequency = 4)       # transform fb into a time series object
ts.plot(fb.ts, type="o")             # ts.plot plot for time series

## we fit a linear model with the tslm function
fitts<- tslm(fb.ts~trend)

###obviously it gives the same results of the first model
summary(fitts)

dwtest(fitts)


#################################### 
#### Linear regression for iMac ####
####################################

apple<- read_excel("apple.xlsx")
str(apple)
imac <- apple$iMac
#data visualization
plot(imac,type="l", xlab="quarter", ylab="iMac sales")

#variable tt for a linear model 
tt<- 1:NROW(apple)

# linear model
fit2 <- lm(imac~tt)
summary(fit2)

plot(imac,type="l", xlab="quarter", ylab="iMac sales")
abline(fit2, col=3)

dwtest(fit2)

###check the residuals
res2<- residuals(fit2)
plot(res2, xlab="quarter", ylab="residuals", type="l")

acf(res2)


#data transformed as time series
mac.ts<-ts(imac, frequency=4)

#Model with trend and seasonality
fit3 <- tslm(mac.ts~ trend+season)
summary(fit3)

#check the residuals
res3 <- residuals(fit3)

plot(res3, ylab="residuals")
dwtest(fit3)

###plot of the model
plot(mac.ts, ylab="iMac sales", xlab="Time")
lines(fitted(fit3), col=2)


##########################################################################################################
#### Data on quarterly percentage change in US consumption, income, production, savings, unemployment ####
##########################################################################################################

uschange<- uschange
str(uschange)
plot(uschange)
autoplot(uschange) 
pairs(uschange)
#different way of seeing the same series

cons<- uschange[,1]
inc<- uschange[,2]
prod<- uschange[,3]
sav<- uschange[,4]
unem<- uschange[,5]


####consider the series of consumption as dependent variable and study with the other explanatory variables in a multiple regression model
fit.cons<- tslm(cons~inc+prod+sav+unem)
summary(fit.cons)
CV(fit.cons)     # about the predictive ability of our model

plot(cons)
lines(fitted(fit.cons), col=2)

res<- residuals(fit.cons)
plot(res)
acf(res)
Acf(res)

##we remove the 'production' variable  
fit.cons1<- tslm(cons~inc+sav+unem)
summary(fit.cons1)
CV(fit.cons1)




#####
################################################
####Linear regression with trend and seasonality and forecasting exercise 

#####################################
###data on Australian beer production
#####################################

beer<- ausbeer
beer
plot(beer)
Acf(beer)


#take a portion of data and fit a linear model with tslm
beer1<- window(ausbeer, start=1992, end=2006 -.1)
beer1
plot(beer1)
m1<- tslm(beer1~ trend+ season)
summary(m1)
fit<- fitted(m1)

plot(beer1)
lines(fitted(m1), col=2)


fore <- forecast(m1)
plot(fore)
##forecasts from regression model for beer production, The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res<- residuals(m1) 
plot(res) 
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res)

dw<- dwtest(m1, alt="two.sided")
dw




#############################################################
#############################################################
####Nonlinear models for new product growth (diffusion models)
#############################################################

###Music data (RIAA)
music<- read_excel("music.xlsx")
str(music)

##create the variable cassette
cassette<- music$cassette[1:36]

###some simple plots
plot(cassette, type="b")
plot(cumsum(cassette), type="b")

###a better plot of the yearly time series
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])

###we estimate a simple Bass Model 
bm_cass<-BM(cassette,display = T)
summary(bm_cass)

# ascolta da minuto 1:10
###prediction (out-of-sample)
pred_bmcas<- predict(bm_cass, newx=c(1:50))
pred.instcas<- make.instantaneous(pred_bmcas)

###plot of fitted model 
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas, lwd=2, col=2)


###we estimate the model with 50% of the data

bm_cass50<-BM(cassette[1:18],display = T)
summary(bm_cass50)

pred_bmcas50<- predict(bm_cass50, newx=c(1:50))
pred.instcas50<- make.instantaneous(pred_bmcas50)

plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas50, lwd=2, col=2)


###we estimate the model with 25% of the data
bm_cass75<-BM(cassette[1:9],display = T)
summary(bm_cass75)

pred_bmcas75<- predict(bm_cass75, newx=c(1:50))
pred.instcas75<- make.instantaneous(pred_bmcas75)


###Comparison between models (instantaneous)
###instantaneous
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas75, lwd=2, col=2)
lines(pred.instcas50, lwd=2, col=3)
lines(pred.instcas, lwd=2, col=4)
#ascolta 1:19:00

###Comparison between models (cumulative)
plot(cumsum(cassette), type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred_bmcas75, lwd=2, col=2)
lines(pred_bmcas50, lwd=2, col=3)
lines(pred_bmcas, lwd=2, col=4)


###exercise: try the same with the CD time series


###Twitter (revenues)

twitter<- read_excel("twitter.xlsx")
length(twitter$twitter)


pdf("twitter.pdf", width=7, height=6)
plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
dev.off()


###BM
tw<- (twitter$twitter)
Acf(tw)


bm_tw<-BM(tw,display = T)
summary(bm_tw)


pred_bmtw<- predict(bm_tw, newx=c(1:60))
pred.insttw<- make.instantaneous(pred_bmtw)


plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred.insttw, lwd=2, col=2)


###GBMr1
GBMr1tw<- GBM(tw,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))


######GBMe1

GBMe1tw<- GBM(tw,shock = "exp",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 12,-0.1,0.1))
summary(GBMe1tw)

pred_GBMe1tw<- predict(GBMe1tw, newx=c(1:60))
pred_GBMe1tw.inst<- make.instantaneous(pred_GBMe1tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GBMe1tw.inst, lwd=2, col=2)




#######################################################################################################################
#######################################################################################################################
######GGM 
GGM_tw<- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
summary(GGM_tw)

pred_GGM_tw<- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst<- make.instantaneous(pred_GGM_tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)
lines(pred.insttw, lwd=2, col=3)

###Analysis of residuals
res_GGMtw<- residuals(GGM_tw)
acf<- acf(residuals(GGM_tw))


fit_GGMtw<- fitted(GGM_tw)
fit_GGMtw_inst<- make.instantaneous(fit_GGMtw)



########################################################################################################################
########################################################################################################################
####SARMAX refining
library(forecast)
####SARMAX model with external covariate 'fit_GGM' 
s2 <- Arima(cumsum(tw), order = c(3,0,1), seasonal=list(order=c(3,0,1), period=4),xreg = fit_GGMtw)
summary(s2)
pres2 <- make.instantaneous(fitted(s2))


plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
lines(fit_GGMtw_inst, lwd=1, lty=2)
lines(pres2, lty=1,lwd=1)