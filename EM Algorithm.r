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