



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
# define grid
p_grid <- seq( from=0 , to=1 , length.out=2 )
# define prior
prior <- c(0.5,0.5)


likelihood<-c(0.3,1)
unstandarized.posterior<-prior*likelihood
posterior<- unstandarized.posterior/sum(unstandarized.posterior)

#2M3
#
numerador<- (0.5*0.5)/((0.7*0.5 + 1*0.5))
