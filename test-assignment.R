# Exercise to test samples???

source("cachematrix.R")

# create cacheable matrix object
m <- makeCacheMatrix()

# initailize with a an easy to inspect matrix 
m$set(matrix( c(0, 2, 2, 0 ), 2, 2) )

# note use of parens to retrive the matrix part of the object
m$get()
#     [,1] [,2]
#[1,]    0    2
#[2,]    2    0

# DKP testing solve()
solve(m)
# Error in solve.default(m) : 'a' must be a numeric matrix
solve(m$get)
# Error in as.vector(x, mode) : 
#    cannot coerce type 'closure' to vector of type 'any'
solve(m$get())
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0

# test the inverse cacher
cacheSolve( m )
#       [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0

# and again... should be cached now
cacheSolve( m )
#getting cached data <-- NOTE THE MESSAGE
# ...

# test that the inverse works and experiment with how to use the functions
# m$get() returns the matrix and cacheSolve(m) returns the inverse that we can 
# use like regular matrices to do things like multilplications...
#
# product of matrix mult should be identity matrix AND we should get the cached message
m$get() %*% cacheSolve(m)
#getting cached data  <-- Yup... cached!
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1       <-- eye() think it's and identity Matrix...

# let R test identify for us
all.equal( diag(2), m$get() %*% cacheSolve(m) )
# getting cached data <-- hey.. it's still cached
#[1] TRUE             <-- R agrees it's an identity

# save the inverse off and let's see if we can break it...
m1<- cacheSolve(m)

# set m to some new values
m$set( matrix( rnorm(4), 2, 2) )

# does fetching the inverse without any pre-caching work?
all.equal( diag(2), m$get() %*% cacheSolve(m) )
#[1] TRUE             <-- yup, sure does...

# what about testing m x the inverse we squirreled away in m1?
all.equal( diag(2), m$get() %*% m1 )
#[1] "Mean relative difference: xxxx"  <-- Nah... that's a train wreck.  

# try a bigger matrix and see if we can notice the caching effects
m$set( matrix( rnorm( 1000000 ), 1000, 1000 ) )
cacheSolve(m)
cacheSolve(m)
# on my quad i5, seems like the second call is faster...

# and double check it all still works...
all.equal( diag( 1000 ), m$get() %*% cacheSolve(m) )
# getting cached data
# [1] TRUE