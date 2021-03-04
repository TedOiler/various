draw_gamma <- function(alpha,beta) {
  a = alpha
  b = beta
  range = seq(0,10, by=1/10)
  y = dgamma(range, a, b)
  return(y)
  #plot(range, y, type='l', col='blue')
}

draw_exp <- function(lambda){
  l = lambda
  range = seq(1,10, by=1/10)
  y = dexp(range, l)
  return(y)
  #plot(range, y, type='l', col='red')
}

prior <- draw_gamma(alpha=2, beta=2)
posterior <- draw_exp(lambda=2)


a_prior = 11.111
b_prior = 1.111
a_posterior = 46.111
b_posterior = 6.111

prior_average = a_prior/b_prior
prior_variace = a_prior/b_prior**2

posterior_average = a_posterior/b_posterior
posterior_variace = a_posterior/b_posterior**2

prior_min = 0
posterior_min = 0

prior_max = prior_average + 5*sqrt(prior_variace)
posterior_max = posterior_average + 5*sqrt(posterior_variace)

max_value = max(prior_max, posterior_max)

x=seq(0,max_value, by=1)
prior = dgamma(x, shape=a_prior, scale=b_prior)
posterior = dgamma(x, shape=a_posterior, scale=b_posterior)

plot(x,prior, type="l", col="red")
lines(x,posterior , type="l", col="green")
