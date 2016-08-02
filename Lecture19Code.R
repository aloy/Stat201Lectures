##Testing whether a coin is fair##
#H0: p = 0.5
#HA: p not equal 0.5 (two sided alternative)
#HA: p > 0.5 (one sided alternative)

n = 100
phat = 58/100
se = sqrt(phat*(1-phat)/n)

z = (phat - 0.5)/se

(1-pnorm(z)) #one-sided (one tails) using Normal approx
sum(dbinom(58:100,size=100,prob=0.5))  #using Binomial exact

(1-pnorm(z)) * 2 #two-sided (both tails) using Normal approx
sum(dbinom(0:42,size = 100,prob=0.5)) + sum(dbinom(58:100,size=100,prob=0.5)) #using Binomial exact

require(mosaic)
dat = c(rep(0,42),rep(1,58))

phatboot = do(1000)*sum(resample(dat))/100

hist(phatboot[,1])
hist(phatboot[,1]-(phat-0.5)) #shift it over to be centered at the Null

d = phat - 0.5 #difference between observed and null
mean((phatboot[,1]-d) >= 0.58 | (phatboot[,1]-d) <= 0.42)#p-value from resampling

