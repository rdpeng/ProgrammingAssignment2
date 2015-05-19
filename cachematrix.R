##The function makeCacheMatrix creates a special matrix that can cache its inverse
##and the function cacheSolve either computes the inverse of the special matrix 
##returned by makeCacheMatrix, or retrieves it from cache, if already calculated 
##for the same matrix

##defines a function to create the aforementioned special matrix, 
##with an argument of an initialised matrix
makeCacheMatrix <- function(x = matrix()) {
    ##creates and initialises a variable to store the matrix inverse  
    MatInv <- NULL
    ##defines a function to initialise or update the value of the matrix inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ##defines a function to return the matrix inverse
    get <- function() x
    ##defines a function to calculate the inverse of an invertible square matrix
    setInv <- function(Inv) MatInv <<- Inv
    ##defines a function to return the calculated or cached matrix inverse
    getInv <- function() MatInv
    ##returns a list with the above functions compiled
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

##defines a function to return the matrix inverse from cache or fresh calculation, 
##with the returned object from makeCacheMatrix as argument
cacheSolve <- function(x, ...) {
    ##assigns the cached matrix inverse or NULL to MatInv
    MatInv <- x$getInv()
    ##returns the cached matrix inverse if MatInv is already cached
    if(!is.null(MatInv)) {
        message("getting cached data")
        return(MatInv)
    }
    ##calculates, caches and returns the matrix inverse if not already cached
    data <- x$get()
    MatInv <- solve(data, ...)
    x$setInv(MatInv)
    MatInv
}
