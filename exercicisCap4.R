library(rethinking)

#Exxercici 4M1
mu<-rnorm(1e4,0,10)
sig<-runif(1e4,0,10)
simulat<-rnorm(1e4,mu,sig)

#Exercici 4M2

m4.2 <- alist(
        height ~ dnorm( mu , sig ) ,
        mu <- dnorm( 0, 10 ) ,
        sigma ~ dunif( 0 , 10 )
    ) 

#Exercici 4M3
#y_i~Normal(mu,sigma)
#mu=a + b*x
#a~normal(0,50)
#b~uniform(0,10)
#sigma~uniform(0,50)

#Exercic 4M4
#y_i~normal(mu,sigma)
#mu=a+b*year
#a~normal(100,70)
#b~normal(2,10)
#sigma~uniform(0,20)

#Exercici 4M5
#y_i~normal(mu,sigma)
#mu=a+b*year
#a~normal(118,10)
#b~normal(2,10)
#sigma~uniform(0,20)
#

#Exercici 4M6
#Si sigma mai es mes gran de 64cm, llavors 64 es 3-Sigma. Paso a normal. mmmm Revisar
#sigma~normal(0,21.3)



#Exercici 4H1

#Fet malament sense centrar els pesos.
library(rethinking)
data(Howell1)
d <- Howell1

m4.H1 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight ,
        a ~ dnorm( 178 , 40 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )
precis( m4.H1 , corr=TRUE )


dens(sim(m4.H1,data=list(weight=c(46.95, 43.72, 64.78, 32.59, 54.63))))


#Fet Be centrant els pesos.
library(rethinking)
data(Howell1)
d <- Howell1
d$weight.centered<-(d$weight-mean(d$weight))/sd(d$weight)


# Error in map(alist(height ~ dnorm(mu, sigma), mu <- a + b * weight, a ~  : 
#                        non-finite finite-difference value [3]
#                    Start values for parameters may be too far from MAP.
#                    Try better priors or use explicit start values.
#                    If you sampled random start values, just trying again may work.
#                    Start values used in this attempt:
#                        a = 272.624768372442
#                    b = 10.8860381965491
#                    sigma = 23.845427040942

#Perque no em surti el error anterior poso el punt d'entrada del map
start <- list(
    mu=mean(d$height),
    sigma=sd(d$height)
)


m4.H1 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight.centered ,
        a ~ dnorm( 178 , 40 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )
precis( m4.H1 , corr=TRUE )

#Ara que tinc el model fet, simulo les alçades per aquestes entrades de pes que també centro,
weight.2pred.centered<-(c(46.95, 43.72, 64.78, 32.59, 54.63)-mean(d$weight))/sd(d$weight)
alt.simulades<-sim(m4.H1,data=list(weight.centered=weight.2pred.centered),n=10000)

#Ara que tinc la simulacio (la sortida te 5 columnes 1  per cada entrada de alçada a simular)
#faig la mitja i el OI i HPDI per cada columna,,,
alt.simulades.mean <- apply(X = alt.simulades, MARGIN = 2, FUN = mean)
alt.simulades.PI <- apply(X = alt.simulades, MARGIN = 2, FUN = PI, prob = .89)
alt.simulades.HPDI <- apply(X = alt.simulades, MARGIN = 2, FUN = HPDI, prob = .89)


#############################
#4H2
#############################

library(rethinking)
library(dplyr)

data(Howell1)
d <- Howell1
d_18<-filter(d,d$age<18)
d_18$weight.centered<-(d_18$weight-mean(d_18$weight))/sd(d_18$weight)
#Perque no em surti el error anterior poso el punt d'entrada del map
start <- list(
    mu=mean(d_18$height),
    sigma=sd(d_18$height)
)


m4.H2 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight,
        a ~ dnorm( 108 , 25 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 75 )
    ) ,
    data=d_18 )
precis( m4.H2 , corr=TRUE )
#per cada 10 kilos augmentem 27,1 cm

plot(height~weight, d_18)
abline(a=coef(m4.H2)['a'],b=coef(m4.H2)['b'])


#per millorar el model fem 2on grau

start <- list(
    mu=mean(d_18$height),
    sigma=sd(d_18$height)
)

d_18$weight2<-d_18$weight^2
m4.H2_2 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b1*weight +b2*weight2,
        a ~ dnorm( 108 , 25 ) ,
        b1 ~ dnorm( 0 , 10 ) ,
        b2 ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 75 )
    ) ,
    data=d_18 )
precis( m4.H2_2 , corr=TRUE )
#per cada 10 kilos augmentem 27,1 cm


#4.H3
##################################

library(rethinking)
data(Howell1)
d <- Howell1
d$weight.centered<-(d$weight-mean(d$weight))/sd(d$weight)


start <- list(
    mu=mean(d$height),
    sigma=sd(d$height)
)


m4.H1 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*log(weight) ,
        a ~ dnorm( 178 , 40 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )
precis( m4.H1 , corr=TRUE )

####copia de GITHUB

## 4H3

library(MASS)
data(Howell1)
d <- Howell1
trials <- 1e5

model <- map(
    alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta*log(weight),
        alpha ~ dnorm(mean = 178, sd = 100),
        beta ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50)
    ),
    data = d
)

# simulate mu then compute mean and hpdi
weight.seq <- seq(from = 1, to = 70, length.out = 100)
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * log(weight)
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight, data = d, col = col.alpha(rangi2, .4))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)