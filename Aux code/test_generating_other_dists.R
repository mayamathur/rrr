
# for a mean-zero normal mixture distribution
#  with 50% probability on each distribution,
#  calculate the variance needed for each component
#  given the offset (c) from the grand mean (assumed 0)
#  and the desired marginal variance (V)

mu = 0
V = 3
c = 1
n = 10000
prob = 0.5
( v.each = V - c^2 )  # from theory

components = sample(1:2,
                    prob=c(0.5, 0.5),
                    size=n,
                    replace=TRUE)
mus <- c(-c, c)
sds <- sqrt(c(v.each, v.each))

samples <- rnorm(n=n,mean=mus[components],sd=sds[components])
hist(samples)



##################### T-DIST #####################


mu = 0.5
V = 0.25^2
q = 0.7
n = 10000
df = 2*V / (V-1)

Xn = rnorm( 
            mean = mu, 
            sd = sqrt(V),
            n = n )
sum(Xn > q) / length(Xn)

# https://stackoverflow.com/questions/17843497/sampling-from-a-t-distribution-in-r
df = 3
Xt = rt(n, df=df) * sqrt(V * (df-2)/df) + mu
sum(Xt > q) / length(Xt)


##################### UNIFORM MIXTURE #####################

# generate from a uniform mixture with endpoints [-b, -a] and [a, b]
#  but shifted so that the grand mean is mu
runif2 = function(n,
                  mu,
                  V) {
  # calculate lower limit for positive distribution, a
  # arbitrary, but can't have too large or else it;s impossible to find
  #  a valid b
  a = sqrt(V)/2  
  
  # calculate upper endpoint for positive distribution, b
  b = abs( 0.5 * ( sqrt(3) * sqrt( 4*V - a^2 ) - a ) )
           
  #b = 0.6

  # prior to mean shift
  components = sample(1:2,
                      prob=c(0.5, 0.5),
                      size=n,
                      replace=TRUE)
  
  mins = c( -b, a )
  maxes = c( -a, b )

  samples = runif(n=n,
                  min=mins[components],
                  max=maxes[components])

  # mean-shift them
  samples = samples + mu
  
  return( list(x = samples,
               a = a, 
               b = b) )
}

# # sanity check
# mu = 0.5
# V = 0.1^2
# fake = runif2( n = 10000,
#                mu = mu, 
#                V = V)$x
# 
# hist(fake)
# mean(fake)
# var(fake); V


# calculate quantile
qunif2 = function(p, 
                  mu,
                  V) {
  
  # calculate lower limit for positive distribution, a
  a = sqrt(V)/2  
  
  # calculate upper endpoint for positive distribution, b
  b = abs( 0.5 * ( sqrt(3) * sqrt( 4*V - a^2 ) - a ) )
  
  # in this case, easy because the q must be within
  #  the negative distribution
  if (p < 0.5) {
    # total length of support, not counting the gap, 
    #  is (b-a)*2
    # we want a point that is p% of the way through the support
    
    # from the lower endpoint of the negative dist, add the proportion of that interval
    q.shift = -b + p * (b-a)*2
  }
  
  else if (p == 0.5) q.shift = 0
  
  # now we're in the positive part of the distribution
  else if (p > 0.5) {
    # subtract the 0.5 that's used up by the negative dist
    # so we want a point that is (p-0.5)% of the way into the support
    #  of the positive part
    #  and the positive part has support length (b-a)
    #q.shift = a + (p - 0.5) * (b-a)
    
    #browser()
    q.temp = -b + p * (b-a)*2
    q.shift = q.temp + 2*a
  }
  
  q = q.shift + mu
  return(q)
}

# # sanity check
# mu = 0.5
# V = 0.1^2
# fake = runif2( n = 10000,
#                mu = mu,
#                V = V)
# 
# hist(fake$x)
# mean(fake$x)
# var(fake$x); V
# 
# p = 0.1
# ( q = qunif2( p = p, 
#         mu = mu, 
#         V = V) )
# sum(fake$x < q) / length(fake$x); p
# # works :) 

###################### WHAT DO EXISTING EXPO DATA LOOK LIKE? ###################### 

# highly skewed :) 
V = 0.01^2
mu = 0.5
n = 10000
Mi = rexp( n = n, rate = sqrt(1/V) )
# now the mean is sqrt(V) rather than mu
# shift to have the correct mean (in expectation)
Mi = Mi + (mu - sqrt(V))
hist(Mi)
