## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
    	x <<- y
    	m <- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)){
    	message("Getting cached data.")
    	return(m)
    }
    n <- x$get()
    m <- solve(n,...)
    x$setinv(m) 
    m

}

#> x <- rbind(c(3, 2), c(-2, -1)) #The inverse is ((-1 -2)(2 3))
#> m1 <- makeCacheMatrix(x)
#> m1$get()
#[,1] [,2]
#[1,]    3    2
#[2,]   -2   -1
#> cacheSolve(m1) #not in the cache
#[,1] [,2]
#[1,]   -1   -2
#[2,]    2    3
#> cacheSolve(m1) #must be in the cache
#Getting cached data.
#[,1] [,2]
#[1,]   -1   -2
#[2,]    2    3
#
#> y <- rbind(c(2, -9), c(0, 9)) #The inverse is ((1/2 1/2)(0 1/9))
#> m2 <- makeCacheMatrix(y)
#> m2$get()
#[,1] [,2]
#[1,]    2   -9
#[2,]    0    9
#> cacheSolve(m2) #not in the cache
#[,1]      [,2]
#[1,]  0.5 0.5000000
#[2,]  0.0 0.1111111
#> cacheSolve(m1) #must be in the cache
#Getting cached data.
#[,1] [,2]
#[1,]   -1   -2
#[2,]    2    3
#> cacheSolve(m2) #must be in the cache
#Getting cached data.
#[,1]      [,2]
#[1,]  0.5 0.5000000
#[2,]  0.0 0.1111111
