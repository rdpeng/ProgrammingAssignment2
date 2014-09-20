##Assignment 2 
## Reference program creates a vector
#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}
#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}
######
#Assignment is required to create a matrix
makeCacheMatrix <- function(x = matrix()) {
      ##initialize matrix 
      invmat <- NULL
       set <- function(y) {
             x <<- y
             invmat <<- NULL
         }
      ## get matrix passed in function
       get <- function() x
      ##set the inverse of the matrix using solve function
#Computing the inverse of a square matrix can be done with the solve function in R.
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.       
       setinverse <- function(solve) invmat <<- solve
       getinverse <- function() invmat
       list(set=set, 
            get=get, 
            setinverse=setinverse, 
            getinverse=getinverse)
   }
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
            # get inverse of matrix x
            invmat <- x$getinverse()
            ##check if inverse of the matrix exists(is not null)
            if(!is.null(invmat)) {
                      message("getting cached data.") ## message will indicate the value is from cache
                      return(invmat)
                  }
            ##get x and compute the inverse using solve as cache was null
            data <- x$get() ## call get defined in makeCacheMatrix 
            invmat <- solve(data, ...) ## use solve to get inverse of the matrix
            ## set the inverse of the matrix using setinverse defined in makeCacheMatrix
            x$setinverse(invmat)
            invmat
        }

# Test Cases and Results
## working directory c:/R
#>source("c:/R/cachematrix.R") 
#>mat<-makeCacheMatrix()
#>mat$set(matrix(c(1,2,3,4),2,2))
#> mat$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(mat)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(mat)
#getting cached data.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
#
