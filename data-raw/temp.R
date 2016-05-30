t1 <- proc.time()[3]
parboot.pvalue.cutover(cutover=0.4,y~x1+x2 , data=loglogit.example , method="approx",B=3)
pvalue.cutover(cutover=0.4,y~x1+x2 , data=loglogit.example)
t2 <- proc.time()[3]
t2-t1

t1 <- proc.time()[3]
parboot.confint.cutover(y~x1+x2 , data=loglogit.example,B,method="transform")
confint.cutover(y~x1+x2 , data=loglogit.example,B=2,method="transform")
t2 <- proc.time()[3]
t2-t1

t1 <- proc.time()[3]
a <- optim.cutover(y~x1+x2 , data=loglogit.example,tol=0.01)
t2 <- proc.time()[3]
t2-t1

t1 <- proc.time()[3]
a <- optim.cutover(y~x1+x2 , data=loglogit.example,cutover.start=0.8)
t2 <- proc.time()[3]
t2-t1

t1 <- proc.time()[3]
example.loglogit.regression <- glm2(y~x1+x2 , data=loglogit.example,
                                    start=c(-0.8,0.6,0.7),
                                    family = binomial(link=loglogit.linkobject(0.8)))
t2 <- proc.time()[3]
t2-t1
