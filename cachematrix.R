## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse_mat <- NULL
set <- function(y){
        x <<- y
        inverse_mat <<- NULL
        
}
get <- function()x
set_inverse_mat <- function(solve) inverse_mat <<- solve
get_inverse_mat <- function()inverse_mat

list(set = set, get = get, set_inverse_mat = set_inverse_mat, 
     get_inverse_mat = get_inverse_mat)
inverse_mat

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_mat <- x$get_inverse_mat()

        if(!is.null(inverse_mat)){
                message("Getting data from cache")
                return(inverse_mat)
        }


        data <- x$get()
        inverse_mat <- solve(x)
        x$set_inverse_mat(inverse_mat)
        inverse_mat

        }
