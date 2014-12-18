# The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse matrix of the matrix
# 4.get the inverse matrix of the matrix
 


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }


# The following cacheSolve function calculates the 
# inverse matrix of the special "matrix"
# created with the above makeCacheMatrix function.
# It will first check to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets 
# the value of the inverse in the cache via the setinverse function.

 cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data.")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
 }
 
#  x = rbind(c(1, -1/4), c(-1/4, 1))
#  x
#        [,1]  [,2]
#  [1,]  1.00 -0.25
#  [2,] -0.25  1.00
#  m = makeCacheMatrix(x)
#  m
# $set
# function (y) 
# {
#    x <<- y
#   inv <<- NULL
# }
# <environment: 0x0000000010c48ac8>

# $get
# function () 
# x
# <environment: 0x0000000010c48ac8>

# $setinverse
# function (inverse) 
# inv <<- inverse
# <environment: 0x0000000010c48ac8>

# $getinverse
# function () 
# inv
# <environment: 0x0000000010c48ac8>

# m$get()
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

# cacheSolve(m)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

#  cacheSolve(m)
# getting cached data.
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
