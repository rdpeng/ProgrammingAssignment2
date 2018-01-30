## Put comments here that give an overall description of what your functions do

## makeCacheMatrix creates a special 'matrix' which can cache (i.e. store in memory) its inverse
## Created by Liz Betts

makeCacheMatrix <- function(x = matrix()) {

#clear any previous inverse values from cache before setting value of matrix
i <- NULL
set <- function(y) {
         x <<- y
         i <<- NULL
}
#get value of matrix then set its inverse
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i

#return list with the 4 functions to set matrix value, store matrix value plus same for the inverse
list(set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## CacheSolve returns the inverse of the 'matrix' produced by makeCacheMatrix above
## if inverse already calculated (and matrix not changed) then Cachesolve returns the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #get cached value for inverse
  if (!is.null(i)) { #if there is a value cached already, return this
      message("getting cached data")
      return(i)
  }
  #if cache is empty, calculate the inverse and return it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}

 #TESTING MY CODE (includes outputs from console)

> B <- matrix(c(2,4,12,14),2,2)
> B1 <- makeCacheMatrix(B)
> cacheSolve(B1)
     [,1] [,2]
[1,] -0.7  0.6
[2,]  0.2 -0.1
> B <- makeCacheMatrix(matrix(1:4,2,2))
> B$get() ##check contents of B
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(B)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> B$getinv()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 



