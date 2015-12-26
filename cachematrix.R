## This function creates a special "matrix" object that can cache its inverse.

## this function take a square matrix. 
## inside the makecacheMatrix,  thenfunction set 
## set the value of the matrix to the environment
## get the value of the matrix to pull out from the environment
## set the value of the inverse to the environment
## get the value of the inverse to the environment


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

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
## check if inverse alread exit, return it
    invert <-x$getinverse()
    if (!is.null(invert)){
      message("getting cached data")
      return(inv)
      
    }
    # if inverse of the matrix is not exit
    # solve it
    ## set it the catch x$setinverse(inv)
    ## return it.    
    
    dat<- x$get()
    inv <-solve(dat, ...)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x' 
}
