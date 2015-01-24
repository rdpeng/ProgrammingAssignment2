## Put comments here that give an overall description of what your
## functions do
## These two function create, store and recall a matrix and its inverse in/from cache  
## Write a short comment describing this function
## makeCacheMatrix creates custom matrix type capable of running four functions
## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {
 
  m <- NULL
    set <- function(y){
     x <<- y  
     m <<- NULL 
     }
   get <- function() x
  setInverse <- function(solve) m<<- solve
    getInverse <- function() m #get inverse matrix
   list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve take a custom matrix type created by the makeCacheMatrix function
## and calculates the inverse matrix of it
## but first it checks to see if the calculation has been done before
## if it has been done before it recalls the data from the cache. If it has not been done 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                

   data <- x$get()                     
   m <- solve(data, ...)               
   x$setInverse(m)                     
}
