## Put comments here that give an overall description of what your
## functions do
## These two function create, store and recall a matrix and its inverse in/from cache  
## Write a short comment describing this function
## makeCacheMatrix creates custom matrix type capable of running four functions
## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix
makeCacheMatrix <- function(x = numeric()) {
 
 m <- NULL
set <- function(y){
x <<- y  
 ## since the matrix is assigned a new value, flush the cache
 m <<- NULL 
}
## returns the stored matrix
get <- function() {
 x
}
## cache the given argument 
 setcacheInverse <- function(solve) {
 m <<- solve
 }

## get the cached value
getInverse <- function() {
m
}
## get inverse matrix
## return a list. Each named element of the list is a function
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
## get the cached value
## Return a matrix that is the inverse of 'x'
m <- x$getInverse()                
if(!is.null(m)) {
message("getting cached data")
return(m)
 }
## otherwise get the matrix, caclulate the inverse and store it in
## the cache
 data <- x$get()                     
 m <- solve(data, ...)               
 x$setcacheInverse(m)  
 ## return the inverse
 m
}
