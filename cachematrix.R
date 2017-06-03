## Put comments here that give an overall description of what your
## functions do
## cache data for reusing it.
## Write a short comment describing this function
# makeCacheMatrix: return a list of function:
# set the matrix, get the matrix, set the inverse of matrix, get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_matrix <- function(matrix) m <<- matrix
        get_matrix <- function() m
        list(set = set, get = get,
                set_matrix = set_matrix, get_matrix = get_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_matrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inverseMatrix <- solve(data)
        x$set_matrix(inverseMatrix)
        ruturn(inverseMatrix)
}
