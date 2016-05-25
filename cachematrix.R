## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() function creates a special "matrix" object 
## that can cache its inverse.
## The matrix supplied should always be invertible.
makeCacheMatrix <- function(x = matrix()) {
  Invz<- NULL
  #following function sets the value of the matrix
  # And makes the Invz variable NULL
  set <- function(y) {
    x <<- y
    Invz <<- NULL
  }
  # get() outputs the matrix
  get <- function() x
  
  # setInverse() sets the inverse to the object Invz 
  
  setInverse <- function(inverse) Invz <<- inverse
  
  # getInverse() outputs the current value/inverse of the object Invz
  
    getInverse <- function() Invz
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

## cachesolve() calculates the inverse of the matrix created with the MakeCacheMatrix()

## It first checks to see if the inverse has already been calculated, if thats the case
## it gets the inverse from the cache memory and skips the further calculation 
## if its not, it calculates the inverse of the special matrix using solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Invz <- x$getInverse()
  
  #checks if the inverse has already been calculated
  if(!is.null(Invz)) {
  #outputs that it is fetching the data from the cache
    message("getting cached data")
    return(Invz)
  }
  tmp <- x$get()
  Invz<- solve(tmp, ...)

    #solve()  calculates the inverse of the matrix
  
  x$setInverse(Invz)
  Invz
}
