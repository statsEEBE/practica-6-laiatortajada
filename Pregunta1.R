x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
xbar<- mean(x)
sigma<- sqrt(25)
n<- lenght(x)
#confiança del 90%
#alfa= 0.1
#z(alfa/2)
z005<- qnorm(0.95)
c(xbar-z005*sigma/sqrt(n), xbar+z005*sigma/sqrt(n))

#instalar libreria
install.packages("BSDA")
library(BSDA)   
z.test(x,sigma.x=sigma,conf.level=0.9)

#provem la hipotesis de que les caixes tenen un pes diferent a 500g
#H0: mu=500
#H1: mu!=500
zcri<- qnorm(0.95) #és el mateix que z005
mu0<- 500

#per dues cues, per defecte
zobs<- (xbar-mu0)/(sigma/sqrt(n))
z.test(x,sigma.x=sigma, conf.level=0.9,mu=mu0)
pvalue<- 2*pnorm(-zobs)  

#per cues superior
z.test(x,sigma.x=sigma, conf.level=0.9,mu=mu0,alternative='greater')


#apartat b)
n<-(qnorm(0.975)*sigma)^2
z025<- qnorm(0.975)
c(xbar-z005*sigma/sqrt(n), xbar+z005*sigma/sqrt(n))


#cas 2 IC 99%
xbar<- mean(x)
n<- length(x)
t005<- qt(0.995,n-1)
s<- sd(x)
c(xbar-t005*s/sqrt(n),xbar+t005*s/sqrt(n))
t.test(x,conf.level=0.99) #pel t.test necessitem les observacions 


#hipotesis dos colas
t.test(x,alternative="two.sided", conf.level=0.99, mu=500)

#hipotesis cola superior
t.test(x,alternative="greater", conf.level=0.99, mu=500)
