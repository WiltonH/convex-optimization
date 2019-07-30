prof <- function(mu0,mubar,c0,ca,cp,d,beta,sigma,p){
  mu <- mu0-d+sigma*sqrt(-2*log(sqrt(2*pi)*c0*sigma/(mu0*p)))
  pro <- p*(1-beta-pnorm((mu0-d-mu)/sigma))-(c0/mu0*mu+ca+cp)
  pro
}

muu <- function(mu0,d,sigma,c0,p){
  mu0-d+sigma*sqrt(-2*log(sqrt(2*pi)*c0*sigma/(mu0*p)))
}

profit <- function(mu0,mubar,c0,ca,cp,d,beta,sigma,p,name="HFC1340-350"){
  mu <- mu0-d+sigma*sqrt(-2*log(sqrt(2*pi)*c0*sigma/(mu0*p)))
  pro <- p*(1-beta-pnorm((mu0-d-mu)/sigma))-(c0/mu0*mu+ca+cp)
  pro.bar <- p*(1-beta-pnorm((mu0-d-mubar)/sigma))-(c0/mu0*mubar+ca+cp)
  pro0 <- p*(1-beta-pnorm((mu0-d-mu0)/sigma))-(c0/mu0*mu0+ca+cp)
  r <- pro/p
  r.bar <- pro.bar/p
  r0 <- pro0/p
  alpha <- pnorm((mu0-d-mu)/sigma)
  alpha.bar <- pnorm((mu0-d-mubar)/sigma)
  alpha0 <- pnorm((mu0-d-mu0)/sigma)
  
  cat("最优容量", mu, "最优利润",pro, "最优利润率",r,"最优次品率",alpha, "实际容量",mubar,"实际利润",pro.bar,"实际利润率",r.bar,"实际次品率",alpha.bar, "标称容量", mu0, "标称利润", pro0, "标称利润率", r0, "标称废品率",alpha0)
  
  curve(p*(1-beta-pnorm((mu0-d-x)/sigma))-(c0/mu0*x+ca+cp),from = mu0-d,to=mu0+5*d ,n = 500, ylab="Profit",xlab="mu",ylim=c(pro-0.5,pro+0.2),main=paste(name,"Profit Curve"))
  #text(mu+d,pro+0.1,"Profit Curve",col="black")
  abline(v=mu,col="red",lty=2)
  abline(h=pro,col="red",lty=2)
  abline(v=mu0,col="blue",lty=2)
  abline(h=pro0,col="blue",lty=2)
  abline(v=mubar,col="black",lty=2)
  abline(h=pro.bar,col="black",lty=2)
  #curve(1/sqrt(2*pi)*p*exp(-((mu0-d-x)/sigma)^2/2)-c0*sigma/mu0,from =mu0-d,to=mu0+3*d,n=1000,main="Profit Differentiation Curve", ylab="Profit Differentiation",xlab="mu",add=T)
  #abline(h=0,col="red",lty=2)
  #abline(v=mu,col="red",lty=2)
  
  #curve((1-beta-pnorm((mu0-d-x)/sigma))-(c0/mu0*x+ca+cp)/p,from = mu0-d,to=mu0+5*d ,n = 500, ylab="Profit Ratio",xlab="mu",main=paste(name,"Profit Ratio Curve"))
  
}

profit(mu0=350,mubar=376.77,c0=0.4,ca=0.3,cp=0.3,d=20,beta=0.06,sigma=9.93,p=7.69,name="HFC1340-350")