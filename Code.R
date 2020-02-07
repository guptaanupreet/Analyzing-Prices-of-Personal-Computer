# Library -----------------------------------------------------------------
library(Ecdat)
library(tidyverse)

# Computers ---------------------------------------------------------------
data(Computers)
?Computers
str(Computers)
dim(Computers)

colSums(is.na(Computers))
hist(Computers$price)

# Checking for normality --------------------------------------------------
install.packages("fitdistrplus")
library(fitdistrplus)

descdist(Computers$price, discrete = FALSE)
normal_dist <- fitdist(Computers$price, distr="norm")
plot(normal_dist)

# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best

# ECDF --------------------------------------------------------------------
set.seed(7031)
f <- ecdf(Computers$price)
plot.ecdf(Computers$price)
Alpha=0.05
n=length(Computers$price)
Eps=sqrt(log(2/Alpha)/(2*n))
grid<-seq(0,5000, length.out = 10000)
lines(grid, pmin(f(grid)+Eps,1),col="red")
lines(grid, pmax(f(grid)-Eps,0),col="red")

f(4000) - f(2000)


# Non-Parametric Bootstrap ------------------------------------------------
set.seed(7031)
(cor.hat <- cor(Computers$price,Computers$screen))

# 0.2960415

theta <- function(x,xdata){ 
  cor(xdata[x,"price"],xdata[x,"screen"]) 
}

#Bootstraping
library(bootstrap)
B=3200
cor.boot<-bootstrap(1:nrow(Computers), B, theta,Computers)

(se.boot=sqrt(var(cor.boot$thetastar)))
# 0.01249974

(normal.ci<-c(cor.hat-2*se.boot, cor.hat+2*se.boot))
#  0.271042 0.321041

(pivatol.ci<-c(2*cor.hat-quantile(cor.boot$thetastar,0.975), 2*cor.hat-quantile(cor.boot$thetastar,0.025)))

# 97.5%      2.5% 
# 0.2710582 0.3209274 

(quantile.ci<-quantile(cor.boot$thetastar, c(0.025, 0.975)))

# 2.5%     97.5% 
# 0.2711555 0.3210247 


# MLE ---------------------------------------------------------------------
x=Computers$price[Computers$premium=="yes"]    
y=Computers$price[Computers$premium=="no"]

n1=length(x)
mu_hat1=mean(x)
sigma_hat1<-sd(x)

n2=length(y)
mu_hat2=mean(y)
sigma_hat2<-sd(y)

mu_hat=mean(x)-mean(y)
mu_hat
# -157.7862

sigma_hat<-sqrt(var(x)/n1+var(y)/n2)  
sigma_hat
# [1] 27.4414

# Parametric Bootstrap ----------------------------------------------------

tau.hat_bootstrap=vector()
n_obs=length(data)
for(i in 1:3200){
  X_i=rnorm(n1,mu_hat1,sigma_hat1)
  Y_i=rnorm(n2,mu_hat2,sigma_hat2)
  tau.hat_bootstrap[i]=mean(X_i)-mean(Y_i)
}

##Or use replicate function
tau_hat_bootstrap<-replicate(3200, mean(rnorm(n1,mu_hat1,sigma_hat1))-mean(rnorm(n2,mu_hat2,sigma_hat2)))

tau.hat_bootstrap_se=sd(tau.hat_bootstrap)
tau.hat_bootstrap_se

#[1]  26.98486

#Confidence Interval
c(mu_hat-2*tau.hat_bootstrap_se,mu_hat+2*tau.hat_bootstrap_se)

#[1] -211.7560 -103.8165






# Wald Test ---------------------------------------------------------------

z.stat<-(mu_hat-0)/sigma_hat
#P(|Z|>|w|)=2*P(Z<-|w|)
p.value=2*(pnorm(-abs(z.stat)))

p.value < 0.05
# TRUE


# Wilcox Test -------------------------------------------------------------
wilcox.test(x, y,conf.int = T,exact=F)

# data:  x and y
# W = 1510700, p-value = 3.091e-07
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -180.99998  -80.00003
# sample estimates:
#   difference in location 
# -129 

# Posterior ---------------------------------------------------------------
#Calculating posterior samples
posterior = rnorm(1000,mean = mu_hat1, sd = sigma_hat1/sqrt(n1)) - rnorm(1000,mean = mu_hat2, sd = sigma_hat2/sqrt(n2))

#To find mean
mean(posterior)

#95% confidence interval
quantile(posterior, c(0.025, 0.975))




