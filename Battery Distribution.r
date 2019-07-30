set.seed(666)
a <- read.csv("20T.csv")
b <- read.csv("1450.csv")
X <- as.matrix(a$VOL[which(a$VOL>=460)])
Y <- as.matrix(b$VOL[which(b$VOL>=460)])
sd(X)
mu1=510
mu2=530
sigma1=16
sigma2=16
N=length(X)
j=1
phi0<-0.5
w1<-matrix(0,1,N)
w2<-matrix(0,1,N)
pnorm<-function(x,mu,sigma){ 
  pf<-1/(2*pi*sigma)*exp(-(x-mu)^2/(2*sigma^2)) 
  return(pf)
}
a<-NULL
b<-NULL
c<-NULL
d<-NULL
while(j<=100){
    for (i in 1:1000){
    Z=pnorm(X[i,1],mu1,sigma1)*phi0+pnorm(X[i,1],mu2,sigma2)*(1-phi0) 
    w1[1,i]<-pnorm(X[i,1],mu1,sigma1)*phi0/Z 
    w2[1,i]<-pnorm(X[i,1],mu2,sigma2)*(1-phi0)/Z
  }
    new_p1=sum(w1)/N
    new_p2=sum(w2)/N
    New_mu1=w1%*%as.matrix(X)/sum(w1)
    New_mu2=w2%*%as.matrix(X)/sum(w2) 
    New_sigma1=sum((w1)%*%(as.matrix(X)-as.matrix(c(rep(New_mu1,N))))^2) /sum(w1) 
    New_sigma2=sum((w2)%*%(as.matrix(X)-as.matrix(c(rep(New_mu2,N))))^2) /sum(w2)
    oldphi0=phi0 
    phi0=new_p1 
    a<-c(a,mu1)
    b<-c(b,mu2)
    c<-c(c,sigma1)
    d<-c(d,sigma2)
    mu1=New_mu1 
    mu2=New_mu2 
    sigma1=sqrt(New_sigma1) 
    sigma2=sqrt(New_sigma2) 
    j=j+1
  }

cat("mu1",mu1,"sigma1",sigma1,"mu2",mu2,"sigma2",sigma2)
n<-N
mean_s <- c(mu2, mu1)
y <- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.8, 0.2))
x <- round(rnorm(n = N, mean = mean_s[1],sd=sigma2))
tails <- y %in% c("tail")
#x[tails] <- rnorm(sum(tails), mean = mean_s[2],sd=sigma1)
x[tails] <- round(rnorm(sum(tails), mean = 494,sd=6))
x<-as.data.frame(x)
#hist(X,breaks=20)
plot(density(X,bw=3.5),xlim=c(450,570), main="HTC1450-500 Distribution")
polygon(density(X,bw=3.5),col="#F1EFF1",border="black")
lines(density(x$x,bw=3.5),xlim=c(450,570),col="red")
rug(jitter(X), side=1, col=1)
#abline(v=c(460,480,500,520,540,560),col="gray40",lty=2)
#abline(v=c(mean_s[1],494),col="black")
#abline(v=c(mean_s[1]+sigma1,mean_s[1]-sigma1,mean_s[1]+2*sigma1,mean_s[1]-2*sigma1,mean_s[1]+3*sigma1,mean_s[1]-3*sigma1),col="blue")


plot(density(Y,bw=3.5),xlim=c(450,570), main="HTC1450-500 Distribution",col="blue")
polygon(density(X,bw=3.5),border="black")

y <- round(rnorm(n = N, mean = 506,sd=8))
plot(density(Y,bw=3.5),xlim=c(450,560), main="HTC1450-500 Distribution")
polygon(density(Y,bw=3.5),col="#F1EFF1",border="black")
lines(density(y,bw=3.5),xlim=c(450,560),col="red")
rug(jitter(Y), side=1, col=1)
abline(v=c(460,480,500,520,540,560),col="gray40",lty=2)
abline(v=c(500),col="black")
abline(v=c(506.5),col="blue")
mean <- 500
sd <- floor(sd(Y))
abline(v=c(mean-2*sd,mean+2*sd,mean+4*sd,mean-4*sd),col="blue",lty=2)

z <- round(rnorm(n = N, mean = mean(Y),sd=sd(Y)))
y <- round(rnorm(n = N, mean = 506,sd=8))
plot(density(Y,bw=3.5),xlim=c(450,560), main="HTC1450-500 Distribution")
#polygon(density(Y,bw=3.5),col="#F1EFF1",border="black")
lines(density(z,bw=3.5),xlim=c(450,560),col="blue")
lines(density(y,bw=3.5),xlim=c(450,560),col="red")
rug(jitter(Y), side=1, col=1)
abline(v=c(460,480,500,520,540,560),col="gray40",lty=2)
abline(v=c(500),col="black")
#abline(v=c(506.5),col="red")
#abline(v=c(mean(Y)),col="blue")
abline(v=c(480),col="black",lty=2)

b <- read.csv("1450.csv")
plot(density(b$VOL,bw=10), main="HTC1450-500 Actual Distribution")
text(3,0.004,"β",col="black")
text(445,0.004,"α(μ)",col="black")
text(515,0.004,"μ",col="black")
abline(v=485,col="black")
abline(v=mean(Y),col="black",lty=2)
abline(v=35,col="black",lty=2)
abline(v=460,col="black",lty=2)