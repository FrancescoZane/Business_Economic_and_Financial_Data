####  Lab 1: Linear regression and nonlinear regression  ####

###########################################
####            Libraries              ####
###########################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)


###########################################
#### some plots for time series        ####
###########################################

plot(a10, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")
seasonplot(a10, ylab="million dollars", xlab="Year", main="Seasonal plot: Antidiabetic drugs", year.labels=T, year.labels.left=T, col=1:20, pch=19)

log_y <- log(a10)
plot(log_y, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")

lin <- tslm(log_y ~ trend)
plot(log_y, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")
lines(fitted(lin), col=2)
summary(lin)
reslin <- residuals(lin)
plot(reslin, xlab="Time", ylab="residuals" )
Acf(reslin)

lin_12 <- tslm(log_y ~ trend+season)
plot(log_y, ylab="million dollars", xlab="Year", main="Antidiabetic drugs")
lines(fitted(lin_12), col=2)
summary(lin_12)
reslin_12 <- residuals(lin_12)
plot(reslin_12, xlab="Time", ylab="residuals" )
Acf(reslin_12)


###########################################
#### Linear regression for time series ####
#### Facebook example                  ####
###########################################

# read the data
facebook<- read_excel("facebook.xlsx")
str(facebook)
# create a variable 'time'
tt<- 1:NROW(facebook)

# create the variable 'fb'
fb <- facebook$fb

# make a plot
plot(tt, fb, xlab="Time", ylab="Facebook users")

# acf of variable "fb"
acf(fb)

##fit a linear regression model 
fit1 <- lm(fb ~ tt)
summary(fit1)

# plot of the model
plot(tt, fb, xlab="Time", ylab="Facebook users")
abline(fit1, col=3)

# check the residuals? are they autocorrelated? Test of DW
dwtest(fit1)

# check the residuals
resfit1 <- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )    # sign of seasonality
acf(resfit1)

# let us do the same with a linear model for time series, so we transform the data into a 'ts' object
fb.ts <- ts(fb, frequency = 4)       # transform fb into a time series object
ts.plot(fb.ts, type="o")             # ts.plot plot for time series


# we fit a linear model with the tslm function
fitts <- tslm(fb.ts ~ trend)

# obviously it gives the same results of the first model
summary(fitts)
dwtest(fitts)


###########################################
#### Linear regression for iMac        ####
###########################################

apple <- read_excel("apple.xlsx")
str(apple)
imac <- apple$iMac
# data visualization
plot(imac,type="l", xlab="quarter", ylab="iMac sales")

# variable tt for a linear model 
tt <- 1:NROW(apple)

# linear model
fit2 <- lm(imac ~ tt)
summary(fit2)

plot(imac,type="l", xlab="quarter", ylab="iMac sales")
abline(fit2, col=3)

dwtest(fit2)

# check the residuals
res2<- residuals(fit2)
plot(res2, xlab="quarter", ylab="residuals", type="l")

acf(res2)


# data transformed as time series
mac.ts<-ts(imac, frequency=4)

# Model with trend and seasonality
fit3 <- tslm(mac.ts ~ trend+season)
summary(fit3)

# check the residuals
res3 <- residuals(fit3)

plot(res3, ylab="residuals")    # sign of non-linear trend
dwtest(fit3)

# plot of the model
plot(mac.ts, ylab="iMac sales", xlab="Time")
lines(fitted(fit3), col=2)


###########################################
#### Data on quarterly percentage      ####
#### change in US consumption, income, ####
#### production, savings, unemployment ####
###########################################

uschange <- uschange
str(uschange)

# different way of seeing the same series
plot(uschange)
autoplot(uschange) 
pairs(uschange)


cons <- uschange[,1]
inc <- uschange[,2]
prod <- uschange[,3]
sav <- uschange[,4]
unem <- uschange[,5]


# consider the series of consumption as dependent variable
# and study with the other explanatory variables in a
# multiple regression model
fit.cons<- tslm(cons ~ inc+prod+sav+unem)
summary(fit.cons)
CV(fit.cons)     # about the predictive ability of our model

plot(cons)
lines(fitted(fit.cons), col=2)

res <- residuals(fit.cons)
plot(res)
acf(res)
Acf(res)

# we remove the 'production' variable 
# recall that in case of no significance variables we try to remove 
# one at a time, NEVER more than one in a single step
fit.cons1 <- tslm(cons ~ inc+sav+unem)
summary(fit.cons1)
CV(fit.cons1)



############################################
#### Linear regression with trend and   ####
#### seasonality and forecasting        ####
#### exercise                           ####
############################################
#### data on Australian beer production ####
############################################

beer <- ausbeer
str(beer)
plot(beer)
Acf(beer)


# take a portion of data and fit a linear model with tslm
beer1 <- window(ausbeer, start=1992, end=2006 -.1)
beer1
plot(beer1)
m1 <- tslm(beer1 ~ trend+season)
summary(m1)
fit<- fitted(m1)

plot(beer1)
lines(fitted(m1), col=2)

# forecasts from regression model for beer production, the dark
# shaded region shows 80% prediction intervals and the light
# shaded 95% prediction intervals (range of values the random
# variable could take with relatively high probability). 
fore <- forecast(m1)
plot(fore)

# analysis of residuals
res <- residuals(m1) 
plot(res) 
# the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res)

dw<- dwtest(m1, alt="two.sided")
dw


############################################
#### Nonlinear models for new product   ####
#### growth (diffusion models)          ####
############################################

# Music data (RIAA)
music<- read_excel("music.xlsx")
str(music)

# create the variable cassette
cassette <- music$cassette[1:36]

# some simple plots
plot(cassette, type="b")
plot(cumsum(cassette), type="b")

# a better plot of the yearly time series
plot(cassette, type="b", xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])

# we estimate a simple Bass Model 
bm_cassb <- BM(cassette, display = T)   # inside the BM method we have to put the z'(t) value, instantaneous values
summary(bm_cass)

# prediction (out-of-sample)
pred_bmcas<- predict(bm_cass, newx=c(1:50))  # use the model to predict the output cumulative values from t=1 to t=50
pred.instcas<- make.instantaneous(pred_bmcas) # pass from cumulative to instantaneous values

# plot of fitted model 
plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas, lwd=2, col=2)


# we estimate the model with 50% of the data
bm_cass50 <- BM(cassette[1:18],display = T)
summary(bm_cass50)

pred_bmcas50 <- predict(bm_cass50, newx=c(1:50))
pred.instcas50 <- make.instantaneous(pred_bmcas50)

plot(cassette, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas50, lwd=2, col=2)


# we estimate the model with 25% of the data
bm_cass75 <-BM(cassette[1:9],display = T)
summary(bm_cass75)

pred_bmcas75 <- predict(bm_cass75, newx=c(1:50))
pred.instcas75 <- make.instantaneous(pred_bmcas75)


# Comparison between models (instantaneous)
plot(cassette, type= "b", xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred.instcas75, lwd=2, col=2)
lines(pred.instcas50, lwd=2, col=3)
lines(pred.instcas, lwd=2, col=4)
# ascolta 1:19:00

# Comparison between models (cumulative)
plot(cumsum(cassette), type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=music$year[c(1,10,19,28,37)])
lines(pred_bmcas75, lwd=2, col=2)
lines(pred_bmcas50, lwd=2, col=3)
lines(pred_bmcas, lwd=2, col=4)


############################################
#### Analysis with the CD time series   ####
############################################


############################################
#### Twitter (revenues)                 ####
############################################

twitter <- read_excel("twitter.xlsx")
length(twitter$twitter)

pdf("twitter.pdf", width=7, height=6)
plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
dev.off()


#### BM ####
tw <- (twitter$twitter)
Acf(tw)

bm_tw <- BM(tw, display = T)  # the BM cumulative the data inside, it wants instantaneous data as input
summary(bm_tw)

# BM used as initial approximation
pred_bmtw <- predict(bm_tw, newx=c(1:60))        # predict the cumulative up to time 60
pred.insttw <- make.instantaneous(pred_bmtw)     # from the cumulative data it calculates the instantaneous

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred.insttw, lwd=2, col=2)

# inspect the differences between the bm model and the instantaneous data, and then decide if we need to use GBM or data are 
# already well explained


#### GBM ####

# GBMr1   rectangular shock
# The initialize values are obtained from the previous BM model              m             p              q
#                                                                                                             a   b   c
GBMr1tw <- GBM(tw, shock = "rett", nshock = 1, prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24, 38, -0.1))
summary(GBMr1tw)

pred_GBMr1tw <- predict(GBMr1tw, newx=c(1:60))
pred_GBMr1tw.inst <- make.instantaneous(pred_GBMr1tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GBMr1tw.inst, lwd=2, col=2)

# GBMe1   exponential shock
GBMe1tw <- GBM(tw,shock = "exp", nshock = 1, prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 12, -0.1, 0.1))
summary(GBMe1tw)    # here m and p are not significant

pred_GBMe1tw <- predict(GBMe1tw, newx=c(1:60))
pred_GBMe1tw.inst <- make.instantaneous(pred_GBMe1tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GBMe1tw.inst, lwd=2, col=2)


#### GGM #### 

GGM_tw <- GGM(tw, prelimestimates=c(4.463368e+04, 0.001, 0.01, 1.923560e-03, 9.142022e-02))
summary(GGM_tw)

pred_GGM_tw <- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst <- make.instantaneous(pred_GGM_tw)

plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)           # GGM
lines(pred.insttw, lwd=2, col=3)                # BM

# analysis of residuals
res_GGMtw <- residuals(GGM_tw)
plot(res_GGMtw)
acf <- acf(residuals(GGM_tw))


############################################
#### SARMAX refining                    ####
############################################

# Note: this is a more complex analysis that needs the knowldege of ARIMA models before being performed
library(forecast)

# SARMAX model with external covariate 'fit_GGM' 
s2 <- Arima(cumsum(tw), order = c(3,0,1), seasonal=list(order=c(3,0,1), period=4),xreg = fit_GGMtw)
summary(s2)
pres2 <- make.instantaneous(fitted(s2))


plot(twitter$twitter, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46), labels=twitter$quarter[c(1,10,19,28,37,46)])
lines(fit_GGMtw_inst, lwd=1, lty=2)
lines(pres2, lty=1,lwd=1)


############################################
#### Let us consider the Germany energy ####
#### transition case. We study          ####
#### consumption of coal, nuclear, and  ####
#### renewable, until year 2019.        ####
############################################

# read the data
bp <- read_excel("BP1.xlsx")
str(bp)

# we consider coal, nuclear and renewable
GC <- bp$GermanyC[1:55]
GN <- bp$GermanyN[1:55]
GR <- bp$GermanyR[28:55] 
# eliminate some NA in the data

# plots
plot(GC, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])

# to save an image as a pdf pdf("name of the image") and dev.off()
pdf("GerN.pdf", width=7, height=6)
plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
dev.off()

gn_tw <- BM(GN,display = T)  # the BM cumulative the data inside, it wants instantaneous data as input
summary(gn_tw)

########################## PROVA A TROVARE DEI BUONI PARAMETRI DI INIZIO PER I PARAMETRI
GBMe1gn <- GBM(GN, shock = "exp", nshock = 2, prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02,
                                                                30, -0.1, -0.1, 50, -0.1, -0.1))
summary(GBMe1gn)

pdf("GerG.pdf", width=7, height=6)
plot(GC, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
dev.off()

pdf("GerR.pdf", width=7, height=6)
plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])
dev.off()


# Coal modelling - BM

bm_GC <-BM(GC,display = T)
summary(bm_GC)

pred_bmGC <- predict(bm_GC, newx=c(1:60))
pred.instGC <- make.instantaneous(pred_bmGC)

plot(GC, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGC, lwd=2, col=2)


# Nuclear modelling

# BM
bm_GN <- BM(GN,display = T)
summary(bm_GN)

pred_bmGN <- predict(bm_GN, newx=c(1:60))
pred.instGN <- make.instantaneous(pred_bmGN)

plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6, ylim=c(0,2))
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGN, lwd=2, col=2)

# GGM
# we fit a GGM (better model...but we need to interpret well the parameters)
GGM_GN <- GGM(GN, prelimestimates=c(56.339566617, 0.001, 0.01, 0.001481412, 0.129385437))
summary(GGM_GN)

pred_GGMGN <- predict(GGM_GN, newx=c(1:60))
pred.instGGMGN <- make.instantaneous(pred_GGMGN)

plot(GN, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46,55), labels=bp$year[c(1,10,19,28,37,46,55)])
lines(pred.instGGMGN, lwd=2, col=2)


# Renewable modelling

# BM 
bm_GR<-BM(GR,display = T)
summary(bm_GR)

pred_bmGR <- predict(bm_GR, newx=c(1:60))
pred.instGR <- make.instantaneous(pred_bmGR)


plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])
lines(pred.instGR, lwd=2, col=1)


# GBM with one exponential shock
GBMe1GR <- GBM(GR,shock = "exp",nshock = 1,prelimestimates = c(3.250461e+01, 5.708759e-04, 1.914512e-01, 15, -0.1, 0.1))
summary(GBMe1GR)

pred_GBMe1GR <- predict(GBMe1GR, newx=c(1:60))
pred.instGBMe1GR <- make.instantaneous(pred_GBMe1GR)


plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=bp$year[28:56][c(1,10,19,28,37)])
lines(pred.instGBMe1GR, lwd=2, col=1)


# Comparison between BM and GBMe1
plot(GR, type= "b",xlab="Year", ylab="Annual consumption",  pch=16, lty=3, cex=0.6, xlim=c(1,60), ylim=c(0,3))
lines(pred.instGBMe1GR, lwd=2, col=2)
lines(pred.instGR, lwd=2, col=3)


