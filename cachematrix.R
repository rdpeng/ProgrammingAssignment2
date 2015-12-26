## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the invert
## get the value of the invert

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    
    set <- function(y){
           x <<- y
           inv <<-NULL
    }


    get <- function() x
    setinverse <- function(solve) inv<<- solve
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)

}


 ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

    invert <-x$getinverse()
    if (!is.null(invert)){
      message("getting cached data")
      return(inv)
      
    }
    dat<- x$get()
    inv <-solve(dat, ...)
    x$setinverse(inv)
    inv
 
}
