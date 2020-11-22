source("cachematrix.R")

##Test 1: Test caching
set.seed(1000)
d <- rnorm(1e+6)
raw1 <- matrix(d, nrow = 1000 )
m <- makeCacheMatrix(raw1)

#Now test the power of caching.
#First without cache
begin <- proc.time()
er2 <- cacheSolve(m)
end <- proc.time()

print("Elapsed without caching")
end - begin

#Then with cache
begin <- proc.time()
er2 <- cacheSolve(m)
end <- proc.time()

print("Elapsed with caching")
end - begin

##Test2: Regression
nums <- c(5,8,13,-3,4,19,21,10,6)
m <- matrix(nums, nrow = 3)

er1 <- c(-0.16210937, 0.08007812, 0.09765625, 
         0.4072266, -0.2373047, -0.1308594, 
         -0.11132812, 0.11523437, 0.04296875) 

res1 <- matrix(m, nrow = 3)
m <- makeCacheMatrix(res1)
er2 <- cacheSolve(m)

epsilon = 0.0001
test_status <- stopifnot(abs(er1-er2) < epsilon)

if (is.null(test_status)) {
    print("Regression: Passed")
} else {
    print("Regression: Some failed")
}
