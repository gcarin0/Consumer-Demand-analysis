#Giancarlo Carino
#EC 410
#Lab 1 - Consumer Demand analysis

install.packages(pkgs=c("psych", "stargazer", "lmtest", "car"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(readxl)

setwd("/Users/gc/Desktop/GitHub/EC-410-Food-and-Agricultural-Economics/Lab 1/data")
Lab1data <- read_excel("Lab1data.xlsx")
attach(Lab1data)

summary(Lab1data) #Summary Statistics
describe(Lab1data) #Summary statistics in Psych package
                   #Summary statistics in Stargazer package
stargazer(
as.data.frame(Lab1data[c("QChuck", "PChuck", "Income")]), type = "text", title = "Descriptive Statistics", digits = 2, out = "table.txt", summary.stat = c("mean", "sd", "min", "max")
)

#Convert all nominal prices into real prices (base period: 2007)
Lab1data$realPchuck <- (Lab1data$PChuck/Lab1data$CPI)*211.445
Lab1data$realSirloin <- (Lab1data$PSirloin/Lab1data$CPI)*211.445
Lab1data$realPChkn <- (Lab1data$PChkn/Lab1data$CPI)*211.445
Lab1data$realIncome <- (Lab1data$Income/Lab1data$CPI)*211.445

#Order/Sort by year and month
Lab1data <- Lab1data[with(Lab1data, order(Year, Month)),]

#Create time series variable t
Lab1data$t <- time(Lab1data$QChuck)

#Convert vector Qchuck into time series object
Lab1data$QChuck <- ts(Lab1data$QChuck, start = c(2001, 1), end = c(2007,12), frequency = 12)
plot.ts(Lab1data$QChuck, ylab = "Demand for Chuck Roast")

#Data Summary Statistics
stargazer(
  as.data.frame(Lab1data[c("QChuck", "QChkn", "realPchuck", "realPChkn", "realIncome")]), type = "text", title = "Descriptive Statistics", digits = 2, out = "table.htm", summary.stat = c("n","mean", "sd", "min", "max")
)

#Data Analysis, part 5
#Simple Regression (DV: QChuck, IndV: realPchuck)

simpReg <- lm(QChuck ~ realPchuck, data = Lab1data)
summary(simpReg)

#Beta0 less than .05, significantly different from 0, different from 0 on a 5% level
#interpret only if p-value less than .05
#estimates give best guess of the population
#beta0 = 0, no relationship between price and quantity
#if p-value is less than .05, reject the null hypothesis (beta0), beta1 is significant
#if p-value > .05, fail to reject null, beta1 is not significantly different from 0
#no enough evidence to show that beta1 is not 0, possibility that there is no relationship, 
#estimate could be 0
#follows law of demand since beta1 < 0, demand curve for QChuck is downward sloping

#Part 6
#Multiple Regression (DV: QChuck, IndV: realPchuck, realIncome, realPChkn)

multReg <- lm(QChuck ~ realPchuck + realPChkn + realIncome , data = Lab1data)
summary(multReg)

#Part 7
#Multiple Regression w/ time var (DV: QChuck, IndV: realPchuck, realIncome, realPChkn, t)

multRegtime <- lm(QChuck ~ realPchuck + realPChkn + realIncome + t , data = Lab1data)
summary(multRegtime)

#Output results from all 3 regression models into a file models.htm
stargazer(simpReg, multReg, multRegtime, type = "html",
          dep.var.labels = "Quantity demanded of Chuck Roast",
          covariate.labels = c("Real Price of Chuck Roast", "Real Price of Chicken", "Real Income", "Time Trend"), out = "models.htm"
          )
#Part 8
#Log-Log Regression w/ time var (DV: ln(QChuck), IndV: ln(realPchuck), ln(realIncome), ln(realPChkn), t)

Lab1data$ln_QChuck <- log(Lab1data$QChuck)
Lab1data$ln_realPChuckk <- log(Lab1data$realPchuck)
Lab1data$ln_realPChkn <- log(Lab1data$realPChkn)
Lab1data$ln_realIncome <- log(Lab1data$realIncome)

logReg <- lm(ln_QChuck ~ ln_realPChuckk + ln_realPChkn + ln_realIncome + t, data = Lab1data)
summary(logReg)

#Output results from log-log regression into a file logmodels
stargazer(logReg, type = "html",
          dep.var.labels = "Log Quantity demanded of Chuck Roast",
          covariate.labels = c("Log Real Price of Chuck Roast", "Log Real Price of Chicken", "Log Real Income", "Time Trend"), out = "logmodels.htm"
)


