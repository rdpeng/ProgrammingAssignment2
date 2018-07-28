## Programming Assignment 2 - MaorGPA
## Here we have a pair of functions that compute and cache the inverse of a matrix

## The following function creates a special "matrix" object which is actually a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                  # Initialize `m`
    
    set <- function(y) {       # You can use this function in order to assign a new matrix to `x` without calling `makeCacheMatrix`
        x <<- y
        m <<- NULL
    }
    
    get <- function() {return(x)}
    setinverse <- function(inverse) {return(m <<- inverse)}
    getinverse <- function() {return(m)}
    
    return(list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    ))
}

## The following function calculates the inverse of 'x'- the special "matrix" created by the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. If so, it "gets" the inverse from the
## cache and skips computation. Otherwise, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if (!is.null(m)) {           # Check if `m` is already calculated or not
        message("Getting cached data...")
        return(m)
    } else {
        data <- x$get()          # Get the "original matrix" (`x` here is a four functions list, not the matrix!)
        m <- solve(data, ...)    # Calculate the inverse of the matrix
        x$setinverse(m)          # Store the inverse in `setinverse` parent environment (which is `makeCacheMatrix`)
        return(m)                # Return the inverse
    }
}


## Sainty check
m1 <- matrix(c(1 / 2, -1 / 4, -1, 3 / 4), nrow = 2, ncol = 2)
solve(m1)

cache_Matrix_Obj <- makeCacheMatrix(x = m1)

cacheSolve(x = cache_Matrix_Obj)
cacheSolve(x = cache_Matrix_Obj)

m2 <- matrix(c(5 / 8, -1 / 8, -7 / 8, 3 / 8), nrow = 2, ncol = 2)
solve(m2)

cache_Matrix_Obj$set(y = m2)

cacheSolve(x = cache_Matrix_Obj)
cacheSolve(x = cache_Matrix_Obj)

