#Part 5a: Using Nasdaq Data Link/Quandl as a data source for predictive models

#IMPORTANT: 
# 1. Please create a free student account in Nasdaq Data Link/Quandl using your CSUF email
# 2. Please make sure to follow the terms and conditions of use for education
# 3. Confirm your account with Nasdaq Data Link/Quandl using the confirmation email
# 4. You will need your API key (check the Account Settings) to access data - DO NOT SHARE IT WITH ANYONE
# 5. Install the Quandl package in R

rm(list=ls(all=T)) # this just removes everything from memory

require(Quandl) # Did you install this package?

#SET KEY
#I store my key in a file which is in my private folder
#Please do not use the link to my file, use your file
path.to.private.key<-'C:/Users/PKalczynski/Dropbox (CSU Fullerton)/My Teaching/ISDS570/quandl.api.txt'

Quandl::Quandl.api_key(
  readChar(path.to.private.key,file.info(path.to.private.key)$size)
)

#Check https://fred.stlouisfed.org/ for symbols of over 0.8M time series
#IMPORTANT: make sure you check the format and available range

#Assume we want to start from 1980-Qtr1
#We need 1979-Qtr4 if we plan to use differencing (we do)

#Quarterly GDP  (start one period earlier)
gdp=Quandl('FRED/GDPC1', start_date='1979-09-01',collapse='quarterly',type='xts')
names(gdp)<-'GDP'
head(gdp)
tail(gdp)
plot(gdp)

#Quarterly unemployment  (start one period earlier)
unrate=Quandl('FRED/UNRATE', start_date='1979-09-01',collapse='quarterly',type='xts')
names(unrate)<-'UNRATE'
head(unrate)

#Quarterly inflation (start one period before GDP)
cpi=Quandl('FRED/CPIAUCSL', start_date='1979-09-01',collapse='quarterly',type='xts')
names(cpi)<-'CPI'

#Transform GDP to GDP growth for forecasting
gdp<-diff(log(gdp))
plot(gdp)

#Combine predictors which will be lagged by the same # of periods
predictors<-merge(unrate,cpi)
head(predictors)

#Difference predictors (note if the series is already stationary you may loose some of its predictive power)
predictors<-diff(log(predictors))

#lag predictors by one period
predictors.lag<-lag(predictors)
head(predictors.lag)
tail(predictors.lag)
names(predictors.lag)<-paste(names(predictors.lag),".Lag1",sep="")

#Combine everything with merge
time.series.xts<-merge(gdp,predictors.lag)
head(time.series.xts)
tail(time.series.xts)
nrow(time.series.xts)

#Preserve the last row (predictors)
#Why now? Because now we know the last available GDP value
new.predictors<-tail(time.series.xts,1)
new.predictors$GDP<-NULL
head(new.predictors)

#Remove rows with missing data
time.series.xts<-na.omit(time.series.xts)
head(time.series.xts)
tail(time.series.xts)
nrow(time.series.xts)

#We can now use time.series.xts and  new.predictors to build predictive models