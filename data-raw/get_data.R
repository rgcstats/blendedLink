# create an example dataset

set.seed(30035)
N <- 500
x1 <- 2*runif(N)-1
x2 <- 2*runif(N)-1
#lp <- -0.5+1.5*x1+1.5*x2
#p <- exp(lp)/(1+exp(lp))
lp <- -1+1.5*x1/2+1.5*x2/2
p <- blendedLink(link1="log",link2="logit",cutover=0.8)$linkinv(lp)
y <- 1*(runif(N)<=p)
loglogit.example <- data.frame(y=y,x1=x1,x2=x2)
save(loglogit.example,file="data/loglogit.example.rdata")
