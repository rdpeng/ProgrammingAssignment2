## Overall description of what my functions do

## "Matrix inversion" is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## "following two functions" are used to cache the inverse of a matrix.
## "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse.

## "cacheSolve": This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set function sets the value of the matrix 
## 2.get function gets the value of the matrix 
## 3.setinverse function sets the inverse of the matrix 
## 4.getinverse function gets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ 
  x <<- y
  m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, 
## it first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse of the matrix from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...){
  m<- x$getinverse()  # if the inverse has already been calculated. If so, gets it from the cache and skips the computation.
  if(!is.null(m)){
    message("getting cached data.") 
    return(m)
  }
  data <- x$get()
  m<- solve(data,...) # otherwise, calculates the inverse 
  x$setinverse(m)  #sets the value of the inverse in the cache via the setinv function.
  m
}
