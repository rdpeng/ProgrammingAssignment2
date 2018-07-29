## The purpose of the two functions is to cache the inverse of a matrix. Computing matrix inversion may be a costly computation, therefore caching the inverse may be beneficial.

## makeCacheMatrix creates a list containing a function to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
   	   x <<- y
   	   inv <<- NULL
   }
   get <- function() x
   setinverse <- funtion(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The next function calculates the inverse of the matix. It checks to see if the inverse has already been calculated. If it has, it gets the inverse from the cache and skips the computation. Otherwise it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse funtion.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
   	   message("getting cached data")
   	   return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
