## Put comments here that give an overall description of what your
## functions do
## The first function creates cache of a square matrix, the second function gives us an inverse of the matrix

## Write a short comment describing this function
## The first function creates a square matrix in such a way that when we have to use this function again we dont have to type it out every time; instead the cache allows us t access it and save time.

makeCacheMatrix <- function(x = matrix()) {
              m = NULL
             set <- function(y) {
                     x <<- y
                     m <<- NULL
             }
            get <- function() x
            setmatrix <- function(matrix) m <<- matrix
            getmatrix <- function() m
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)
}


## Write a short comment describing this function
## The second function lets us inverse the square matrix created in the first function and creates its respective cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- mean(data, ...)
         x$setmean(m)
         m
}
