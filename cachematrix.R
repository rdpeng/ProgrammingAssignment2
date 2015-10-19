## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL ## Sets the matrix inverse to NULL
        ## set function changes the matrix that is stored in the main function
        set <- function(y){
                x <<- y
                mat_inv <<-  NULL
        }
        ## get returns the matrix x stored in the main function.
        get<-function() x
        ## set_inv sets the inverse of the matrix, mat_inv, from the input (inv)
        set_inv <-function(inv) mat_inv <<- inv
        ## get_inv gets the inverse of the matrix
        get_inv <- function() mat_inv
        ##Creates a list of the functions that was define above.
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve function calculates the inverse of the special "matrix" created 
## by the "makeCacheMatrix"###

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$get_inv() # equates the mat_inv variable to the function
        if (!is.null(mat_inv)){
                message("This is the Cached Data")
                return(mat_inv)
        } ##Checks if the inverse is available, it will return the cached value
        data<-x$get() #gets the matrix to calculate its inverse
        mat_inv <- solve(data, ...) #calculating inverse using solve function.
        x$set_inv(mat_inv) # we cache the inverse for future
        mat_inv ## the function returns the inverse of the matrix
}


