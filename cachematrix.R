
#<--The following function first sets the value of the matrix and gets its value. 
#The function set_inverse sets the inverse of the matrix
#The function get_inverse gets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        set_inverse <- function(inverse) matrix_inverse <<- inverse
        get_inverse <- function() matrix_inverse
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)      
}

#-->


##-- The following function inverses the matrix created by the makeCacheMatrix function. If the inverse is already calculated,
# then it'll retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse<-x$get_inverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        mat<-x$get()
        matrix_inverse<-solve(mat,...)
        x$set_inverse(matrix_inverse)
        matrix_inverse
        
}
