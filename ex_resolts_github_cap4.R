data(Howell1)
d <- Howell1
d <- d[d$age < 18,]

# scale weight column
d$weight.standardized <- d$weight

# a)
model <- map(
    alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta*weight.standardized,
        alpha ~ dnorm(mean = 100, sd = 100),
        beta ~ dnorm(mean = 0, sd = 10),
        sigma ~ dunif(min = 0, max = 50)
    ),
    data = d
)

precis(model)

# b)
library(MASS)
trials <- 1e5

weight.seq <- seq(from = 1, to = 45, length.out = 50)

# simulate mu then compute mean and hpdi
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * weight
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight.standardized, data = d, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)