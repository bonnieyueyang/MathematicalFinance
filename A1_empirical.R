############## set up ##############
library(readxl)
library(tidyverse)
library(zoo)
library(pracma)
library(jrvFinance)
library(lubridate)
library(ggplot2)
getwd()
setwd("/Users/bonnie/Desktop/APM466")

############# load files & cleaning the titles ##############
data=read_excel("Bonds_Price_Records.xlsx", 2)
data2=read_excel("Bonds_Price_Records.xlsx", 13)
colnames(data) = gsub("\r\n|\\s*\\s|\\s*\n", "", colnames(data))
df=as.data.frame(data)
df2=as.data.frame(data2)

############# Calculate each of 11 selected bonds’ yield to maturity ##############
ytm=data.frame(matrix(ncol=10, nrow=11))
days= c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13",
               "2020-01-14","2020-01-15")
for (i in c(1:11)){
  for (j in c(1:10)){
    ytm[i,j]=bond.yield(days[j], # settle date
                        df$maturitydate[i], #maturity
                        df$Coupon[i], #coupon rate
                        freq=2, #coupon frequency
                        df[i,j+5], #clean price
                        convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"), 
                        comp.freq=2, #compounding frequency
                        redemption_value = 100) #face value
  }
}
view(ytm)


############## Plot YTM ##############
plot(seq(1,6,0.5), ytm2$X1,type="l",ylim=c(0.015,0.021), col=1,
     xlab="Year",ylab="Yield", main ="YTM")
lines(seq(1,6,0.5),ytm2$X2,col=2)
lines(seq(1,6,0.5),ytm2$X3,col=3)
lines(seq(1,6,0.5),ytm2$X4,col=4)
lines(seq(1,6,0.5),ytm2$X5,col=5)
lines(seq(1,6,0.5),ytm2$X6,col=6)
lines(seq(1,6,0.5),ytm2$X7,col=7)
lines(seq(1,6,0.5),ytm2$X8,col=8)
lines(seq(1,6,0.5),ytm2$X9,col=9)
lines(seq(1,6,0.5),ytm2$X10,col=10)
legend("topright", days, lwd=c(2,2),cex=.5, col=c(1:10))

############### Calculate Spot Rate ############### 
# The first spot rate
for (i in c(1:10)){
  price=df2[1,1+i]
  coupon=df$Coupon[i]*100/2
  ttm=df$monthsTillMaturity[i]/12
  spot_rate[1,i]=2*((price/(coupon+100))^(-1/(2*ttm))-1)
}

# Bootsrapping - Induction
spot_rate=data.frame(matrix(ncol=10, nrow=11))
for (i in c(2:11)){
  for (j in c(1:10)){
    price=df2[i,1+j]
    coupon=df$Coupon[i]*100/2 
    pv_coupon=0 
    ttm=df$monthsTillMaturity[i]/12
    coupon_times = seq((6-df$monthsFromLastMaturity[i])/12,(df$monthsTillMaturity[i]-1)/12,1/2)
    for (h in c(1:length(coupon_times))){
      pv_coupon=pv_coupon+coupon*(1+spot_rate[h,j]/2)^(-2*coupon_times[h])
    }
    price2=price-pv_coupon
    pv_coupon=0
    spot_rate[i,j]=2*((price2/(coupon+100))^(-1/(2*ttm))-1)
  }
}
View(spot_rate)

############## Interpolation ##############
spot_rate2=data.frame(matrix(ncol=10, nrow=11))
for (i in c(1:10)){
  for (j in c(1:10)){
    spot_rate2[j,i]=approx(df$monthsTillMaturity,spot_rate[[i]],xout=j)$y
  }
}
rownames(spot_rate2)=seq(1,6,0.5)
view(spot_rate2)

############## Plot Spot Curve ##############
plot(seq(1,6,0.5),spot_rate2$X1,type="l", ylim=c(0.015,0.025), col=1,
     xlab="Year",ylab="Spot Rate", main ="Spot Curve")
lines(seq(1,6,0.5),spot_rate2$X2,col=2)
lines(seq(1,6,0.5),spot_rate2$X3,col=3)
lines(seq(1,6,0.5),spot_rate2$X4,col=4)
lines(seq(1,6,0.5),spot_rate2$X5,col=5)
lines(seq(1,6,0.5),spot_rate2$X6,col=6)
lines(seq(1,6,0.5),spot_rate2$X7,col=7)
lines(seq(1,6,0.5),spot_rate2$X8,col=8)
lines(seq(1,6,0.5),spot_rate2$X9,col=9)
lines(seq(1,6,0.5),spot_rate2$X10,col=10)
legend("topright", days, lwd=c(2,2),cex=.5, col=c(1:10))


############## Calculate Forward Rate ##############
forward_rate=data.frame(matrix(ncol=10, nrow=4))
for (j in c(1:4)){
  for (i in c(1:10)){
    one_yr=(1+spot_rate[2,i]/2)^2
    n_yr=(1+spot_rate[2+2*j,i]/2)^(2+2*j)
    forward_rate[j,i]=2*((n_yr/one_yr)^(1/(2*j))-1)
  }
}
view(forward_rate)

################## Plot Forward Rate ##################
plot(seq(2,5),forward_rate$X1,type="l",ylim=c(0.010, 0.018), col=1,
     xlab="Years",ylab="Forward Rate", main ="Forward Curve")
lines(seq(2,5),forward_rate$X2,col=2)
lines(seq(2,5),forward_rate$X3,col=3)
lines(seq(2,5),forward_rate$X4,col=4)
lines(seq(2,5),forward_rate$X5,col=5)
lines(seq(2,5),forward_rate$X6,col=6)
lines(seq(2,5),forward_rate$X7,col=7)
lines(seq(2,5),forward_rate$X8,col=8)
lines(seq(2,5),forward_rate$X9,col=9)
lines(seq(2,5),forward_rate$X10,col=10)
legend("topleft", days, lwd=c(2,2),cex=.5, col=(1:10))



############ Calculate Covariance Matrices for Log-return of Yields ############
log_return_yields1=vector("numeric", length=9)
log_return_yields2=vector("numeric", length=9)
log_return_yields3=vector("numeric", length=9)
log_return_yields4=vector("numeric", length=9)
log_return_yields5=vector("numeric", length=9)

for (i in c(1:9)){
  log_return_yields1[i]=log(ytm2[2,i]/ytm2[2,i+1])
  log_return_yields2[i]=log(ytm2[4,i]/ytm2[4,i+1])
  log_return_yields3[i]=log(ytm2[6,i]/ytm2[4,i+1])
  log_return_yields4[i]=log(ytm2[8,i]/ytm2[8,i+1])
  log_return_yields5[i]=log(ytm2[10,i]/ytm2[10,i+1])
}
log_returns_yields=data.frame(log_return_yields1,
                              log_return_yields2,
                              log_return_yields3,
                              log_return_yields4,
                              log_return_yields5)
covariance_of_log_returns=cov(log_returns_yields,log_returns_yields)
view(covariance_of_log_returns)

############ Calculate Covariance Matrices for Forward Rates ################
fwd1=vector("numeric", length=9)
fwd2=vector("numeric", length=9)
fwd3=vector("numeric", length=9)
fwd4=vector("numeric", length=9)
for(i in c(1:9)){
  fwd1[i]=log(forward_rate[1,i]/forward_rate[1,i+1])
  fwd2[i]=log(forward_rate[2,i]/forward_rate[2,i+1])
  fwd3[i]=log(forward_rate[3,i]/forward_rate[3,i+1])
  fwd4[i]=log(forward_rate[4,i]/forward_rate[4,i+1])
}

fwd=data.frame(fwd1,fwd2,fwd3,fwd4)
covariance_of_fwdrates=cov(fwd,fwd)
view(covariance_of_fwdrates)

############ Calculate Calculate the eigenvalues and eigenvectors ################
eigen_yield=eigen(covariance_of_log_returns)
eigen_fwdrate=eigen(covariance_of_fwdrates)
eigen_yield
eigen_fwdrate
################################ END ################################



