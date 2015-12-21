## 1.makeCacheMatrix function creates a special matrix which can cache its inverse.
## 2.cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- matrix()
        }
        get <- function() x
        set_inv_matrix <- function(inv_mat) inv_mat <<- solve
        get_inv_matrix <- function() inv_mat
        list(set = set, get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}


## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$get_inv_matrix()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inv_matrix(inv_mat)
        inv_mat
}

## output
## x <- c(7, 4, 9, 3)
## y <- matrix(data = x, nrow = 2, ncol = 2)
## z <- makeCacheMatrix(y)
## inv_mat <- cacheSolve(z)
## inv_mat
