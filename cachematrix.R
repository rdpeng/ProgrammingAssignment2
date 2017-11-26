## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix is a wrapper which creates get and set functions to 
## get & set matrix
## retrieve cached matrix inverse
## cache matrix inverse


makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y){
        x <<- y
        matrix_inverse <<- NULL
    }
    
    get <- function() x
    setMI <- function(MI) matrix_inverse <<- MI
    getMI <- function() matrix_inverse
    
    list(set=set,get=get,setMI=setMI,getMI=getMI)

}


## Returns the inverse of the matrix. Returns the cached matrix matrix if its available if not available 
##calculates and makes a call to setMI to cache the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    matrix_inverse <- x$getMI()
    if(!is.null(matrix_inverse)){
  
        message("getting matrix inverse cached data")
        return(matrix_inverse)
    }
    
    data<-x$get()
    matrix_inverse<-solve(data)
    x$setMI(matrix_inverse)
    matrix_inverse
}
