## makeCacheMatrix: (a) Creates a special "matrix" object
##		    (b) Caches the matrix of the inverse
## cacheSolve: (a) Checks if inverse of the matrix is already computed.
##                 If TRUE, it returns the cached inverse of the matrix.
##	       (b) Otherwise it calculates inverse of the matrix,
##                 calls makeCacheMatrix()$setinverse function to cache the inverse, and
##                 returns the calculated inverse.


## This function creates a special "matrix" object
## Also can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  
  set <- function(x){
  
    #Creating special matrix
    m <<- x
    
    #Assigning NULL to inverse of matrix
    im <<- NULL
  }
  
  #Prints the special matrix
  get <- function() m
  
  #Setting inverse of matrix
  setinverse <- function(inv) im <<- inv
  
  #Prints inverse of matrix
  getinverse <- function() im
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  
  #Retrieving inverse of the matrix
  im <- x()$getinverse()
  
  #Checking whether inverse matrix is NULL
  #If inverse of matrix is NOT null, then it returns the cached value
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  
  #Retrieving matrix data
  data <- x()$get()
  
  #Calculating inverse of the matrix
  im <- solve(data, ...)
  
  #Caching the calculated inverse of the matrix
  x()$setinverse(im)
  
  #Prints the inverse of the matrix
  im  
}

