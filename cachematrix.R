## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL
   }
   get <- function () x
   setinverse <- function (inverse) inv <<- inverse
   getinverse <- function() inv
   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

#The following function returns the inverse of the special vector created in "makeCacheMatrix." To do this,
#it first checks to see if the inverse has already been calculated. If so, then the inverse is not recalculated and
#the previously calculated inverse is returned. If not, the inverse is calculated, set as the inverse value in the cache
#via the "setinverse" function and is then returned in the console. 

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cached data.")
     return(inv)
   }
   data <-x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
