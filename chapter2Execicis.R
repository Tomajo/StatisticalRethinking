



compute_posterior <-function(w,n,prior,p_grid){

    
    # compute likelihood at each value in grid
    likelihood <- dbinom( w , size=n , prob=p_grid )
    
    # compute product of likelihood and prior
    unstd.posterior <- likelihood * prior
    
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior / sum(unstd.posterior)
    ## R code 2.4

    
}

#Exercise 2M1

#1 --> W,W,W
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep(1,length(p_grid))


posterior<-compute_posterior(w=3,n=3,prior = prior,p_grid = p_grid)
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )

#2 --> W,W,W,L
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep(1,length(p_grid))


posterior<-compute_posterior(w=3,n=4,prior = prior,p_grid = p_grid)
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )

#3 --> L,W,W,L,W,W,W
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep(1,length(p_grid))


posterior<-compute_posterior(w=5,n=7,prior = prior,p_grid = p_grid)
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )

#2M2
#prior = 0 if p<0.5, and positive constant (0.6) if p>=5
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- append(rep(0,length(p_grid)/2),rep(0.6,length(p_grid)/2))


posterior<-compute_posterior(w=5,n=7,prior = prior,p_grid = p_grid)
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )


#2M3
#imagine 0.5 of chance earth with p=0.7, and a 0.5 chance of mars with 0

# define prior
prior <- c(0.5,0.5)


likelihood<-c(0.3,1)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)

#2M4


# define prior
prior <- c(1/3,1/3,1/3)


likelihood<-c(1,0.5,0)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)

#2M5

# define prior
prior <- c(1/4,1/4,1/4)


likelihood<-c(1,0.5,0,1)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)

#2M6 

#TODO

#counting method
#prior = possibilities
prior<-c(2,1,0)

#probabilities
likelihood<-c(1,2,3)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)

#2M7
prior <- c(1,1,1,1,1,1)


likelihood<-c(2,0,4,0,2,0)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)
posterior[1]+posterior[3]


#2H1

prior <- c(1, 1)

likelihood<- c(0.1,0.2)

unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)


#2H2

posterior[2]

#2H3

prior <- c(1,1)

likelihood<- c(0.1*0.9,0.2*0.8)

unstandarized.posterior.birth<-prior*likelihood
posterior.birth<- unstandarized.posterior.birth/sum(unstandarized.posterior.birth)
posterior.birth[1]



#2H4.1

prior <- c(1,1)

likelihood<- c(0.8,0.35)

unstandarized.posterior.test<-prior*likelihood
posterior.test<- unstandarized.posterior.test/sum(unstandarized.posterior.test)
posterior.test[1]


#2H4.2

 #Ho podem fer de un pas.. pero crec que en aquest cas es millor composar ...
prior <- c(1,1)

likelihood<- c(0.1*0.9*0.8,0.2*0.8*0.35)

unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)
posterior[1]


#calcul compost

unstandardized.posterior<-posterior.birth*posterior.test

posterior<-unstandardized.posterior/sum(unstandardized.posterior)

