#EC581 Assignment #1
#Mustafa Kutay Yabas
library(zoo)
library(Quandl)
library(quantmod)

#get data from Quandl in zoo format
data_xu100 = Quandl("GOOG/INDEXIST_XU100", api_key="XXX", start_date="2003-07-01", type="zoo")

head(data_xu100)
tail(data_xu100)

Price<-data_xu100$Close
plot(Price)

#log returns
Return<-vector(length=length(Price)-1)
for (i in 2:length(Price)) {
  Return[i-1] <- (log( Price[[i]]/Price[[i-1]] ))
}

#log returns alternative method
#Return<-diff(log(Price), lag=1)

summary(Price)

#histogram of log returns
histogram = hist(Return)

#daily average return
daily_average_return = mean(Return) # %0.06

#average annualized return
annualized_return = ( ((daily_average_return+1)^220) - 1  ) * 100 # %14.44

# t-test function
ttest<-function(x1,x2) {
  s1<-var(x1)
  s2<-var(x2)
  n1<-length(x1)
  n2<-length(x2)
  mu1<-mean(x1)
  mu2<-mean(x2)

  st <- ( (n1-1)*s1 + (n2-1)*s2 ) / ( (n1-1) + (n2-1) )
  t <- (mu1 - mu2) / ( sqrt(st) * sqrt( 1/n1 + 1/n2 ) )
  return(t)

}

#Simulate random asset price to compare with Price
Test<-vector(length = length(Price))
Test[1]<-rnorm(1)/100
for (i in 2:length(Test)) {
  Test[i]<-0.9*Test[i-1]+rnorm(1)/100
}
Test<-exp(cumsum(Test))

#Now make the t-test
result_ttest = ttest(Price,Test)
