## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse property
        I <- NULL 
        ## set/get the matrix
        set <- function(matrix) {
                x <<- matrix
                I <<- NULL
        }
        get <- function() {x}
        ## set/get the inverse of the matrix
        setI <- function(Inverse) {I <<- Inverse}
        
        getI <- function() {I}
        ## return the list containing functions 
        list(set = set, get = get,
             setI = setI,
             getI = getI)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getI()
        ## return the inverse if it has already been calculated  
         if(!is.null(I)) {
                message("getting cached data")
                return(I)
         }
        ## get the matrix from our object
        data <- x$get()
        ## Calculate the inverse of the data
        I <- solve(data) %*% data
        ## set the inverse to the object
        x$setI(I)
        ## return the matrix
        I
}
