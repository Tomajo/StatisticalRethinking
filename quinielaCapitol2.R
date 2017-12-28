



library(rethinking)
p_grid <- seq( from=0 , to=1 , length.out=1000)
prior <- c(10,10,12,10,9,8,11) 
likelihood <- dbinom( 14, size=14 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

plot(posterior)
dens(samples)
HPDI(samples,0.6)


