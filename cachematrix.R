## Put comments here that give an overall description of what your
## functions do

## this function returns a list of 04 functions, 
## set  and get a matrix, set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
					s <- NULL
                    set <- function(y) {
                        x <<- y
                        s <<- NULL
                      }
                    get <- function() x
                      setsolve <- function(solve) s <<- solve
                      getsolve <- function() s
                    list(set = set, get = get,
                           setsolve = setsolve,
                           getsolve = getsolve)
}


## this function take an object makeCacheMatrix and returns the inverse of a matrix, 
## this function set also the inverse in the makeCacheMatrix object,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            s <- x$getsolve()
            if(!is.null(s)) {
              message("getting cached data")
              return(s)
            }
            data <- x$get()
            if (dim(data)[1]==dim(data)[2]) {
                if (det(data)!= 0) {
                  s <- solve(data, ...)
                } else message("this matrix is not invertible")
            } else message("x is not a square matrix")
            
            x$setsolve(s)
            s
}
