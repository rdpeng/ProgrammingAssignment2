## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse<-NULL
    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    matrix_inverse <- x$getInverse()
    if(!is.null(matrix_inverse)){
  
        message("getting matrix inverse cached data")
        return(matrix_inverse)
    }
    
    data<-x$getMI()
    matrix_inverse<-solve(data)
    x$setMI(matrix_inverse)
    matrix_inverse
}
