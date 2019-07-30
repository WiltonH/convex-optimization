library(fitdistrplus)
library(MASS)

# data

a <- read.csv("36T.csv")
X <- as.matrix(a$VOL[which(a$VOL>=330)])
X0 <- as.matrix(a$VOL[which(a$VOL<330)])
N=length(X)
1-length(X)/length(a$VOL)

# dist
set.seed(666)
X.v <- as.vector(X)
fit <- fitdist(X.v, "norm")
descdist(X.v, boot = 1000)
x <- round(rnorm(n = N, mean = fit$estimate[1],sd=fit$estimate[2]))
y <- round(rnorm(n = N, mean = 377 ,sd=7.8))

plot(density(X,bw=4), main="HFC1340-350 Distribution")
#polygon(density(X,bw=4),col="#F1EFF1")
lines(density(x,bw=4),col="blue")
lines(density(y,bw=4),col="red")
rug(jitter(X), side=1, col=1)
abline(v=c(330,350,370,390,410),col="gray40",lty=2)
abline(v=c(350),col="black")
abline(v=c(377),col="black",lty=5)

plot(density(a$VOL,bw=10), main="HFC1340-350 Actual Distribution")
text(3,0.003,"β",col="black")
text(335,0.003,"α(μ)",col="black")
text(385,0.003,"μ",col="black")
abline(v=c(350),col="black")
abline(v=c(320),col="black",lty=2)
abline(v=c(35),col="black",lty=2)
abline(v=378,col="black",lty=2)

# var

set.seed(666)
x1 <- round(rnorm(n = N, mean = 376.77,sd=9.93))
x2 <- round(rnorm(n = N, mean = 376.77 ,sd=5))
x3 <- round(rnorm(n = N, mean = 376.77 ,sd=15))
x4 <- round(rnorm(n = N, mean = 350 ,sd=9.93))
plot(density(x2,bw=10), main="HFC1340-350 Distribution vs. sigma & mu",col="blue",xlim=c(300,440))
lines(density(x1,bw=10),col="black")
lines(density(x3,bw=10),col="red")
lines(density(x4,bw=10),col="purple")
rug(jitter(x1), side=1, col=1)
abline(v=c(376.77),col="black",lty=2)
abline(v=c(350),col="black",lty=5)
abline(v=c(330),col="black",lty=1)
abline(h=0.0286095,col="black",lty=2)
text(310,0.005,"α(μ)",col="black")