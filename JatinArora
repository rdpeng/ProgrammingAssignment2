## Put comments here that give an overall description of what your
## functions do

## Retuns a list containing 4 functions set, get, set_inverse, get_inverse.
## set - sets the value of matrix (Input Matrix for which Inverse is to be cached)
## get - prints input matrix on screen
## set_inverse - value of inverse of a matrix is set using cacheSolve 
## get_inverse - returns value of inversed Matrix 

## Functions will give correct result only for  Invertible Matrix 


makeCacheMatrix <- function(x = matrix()) {
				inv_matrix <- NULL
				set <- function(y) {
							x <<- y 
							inv_matrix <<- NULL 
							}
				get <- function() x 
				set_inverse <- function(inv) inv_matrix <<- inv 
				get_inverse <- function() inv_matrix
				list(set = set, get = get, 
					set_inverse = set_inverse , 
					get_inverse = get_inverse)
}

## cacheSolve function takes list created by makeCacheMatrix as input and returns inverse of matrix from cached memory if available, 
## if not available, this function computes the value of inverse of matrix and stores it in cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv_matrix <- x$get_inverse()
		if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$set_inverse(inv_matrix)
        inv_matrix
}


# ### Sample Run 
###  > x <- matrix(1:4, 2, 2)
###  > inv_check <- makeCacheMatrix(x)
###  > 
###  > inv_check$get()
###       [,1] [,2]
###  [1,]    1    3
###  [2,]    2    4
###  > inv_check$get_inverse()
###  NULL
###  > cacheSolve(inv_check)
###       [,1] [,2]
###  [1,]   -2  1.5
###  [2,]    1 -0.5
###  > cacheSolve(inv_check)
###  getting cached data
###       [,1] [,2]
###  [1,]   -2  1.5
###  [2,]    1 -0.5
###  > inv_check$get_inverse()
###       [,1] [,2]
###  [1,]   -2  1.5
###  [2,]    1 -0.5
