## R code 3.27
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )


#3E1
set.seed(100)


sum(samples>0.2)/1e4
#3E2

sum(samples<0.8)/1e4
#3E3

sum(samples>0.2 & samples<0.8)/1e4

#3E4

quantile(samples,probs = 0.2)

#3E5

quantile(samples,probs = 0.8)

#3E6

PI(samples,prob = 0.5)
HPDI(samples,prob = 0.5)


#3M1

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )


#3M2
library(rethinking)
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

HPDI(samples,0.9)

#3M3


tosses<-rbinom(n = 1e4,prob = samples,size = 15)
eigth<-sum(tosses==8)/length(tosses)
simplehist(tosses)
mean(tosses==8)
#3M4
mean(tosses==6)


#3M5


    p_grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- append(rep(0,500),rep(1,500))
    likelihood <- dbinom( 8 , size=15 , prob=p_grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)
    samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
    
    
    #3M2
    library(rethinking)
    p_grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- append(rep(0,500),rep(1,500))
    likelihood <- dbinom( 3 , size=3 , prob=p_grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)
    samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
    
    HPDI(samples,0.9)
    
    #3M3
    
    
    tosses<-rbinom(n = 1e4,prob = 0.842,size = 15)
    eigth<-sum(tosses==8)/length(tosses)
    simplehist(tosses)
    mean(tosses==8)
    #3M4
    mean(tosses==6)

