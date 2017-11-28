## R code 2.3
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )

# define prior
prior <- append(rep( 0 , 10 ),rep( 1 , 10 ))

# compute likelihood at each value in grid
likelihood <- dbinom( 5 , size=7 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
## R code 2.4
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )



#2M3
#
numerador<- (0.5*0.5)/((0.7*0.5 + 1*0.5))
