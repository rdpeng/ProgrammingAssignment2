## makeCacheMatrix creates a special matrix object that can cache an input matrix and its inverse.
## Then, cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
        mcm <- NULL #sets the value of mcm to NULL (provides a default if cacheSolve has not yet been used)
        set <- function(y) { #set the value of the matrix
                x <<- y #caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
                mcm <<- NULL #sets the value of mcm (the matrix inverse if used cacheSolve) to NULL
        }
        get <- function() x #get the value of the matrix
        setinverse<- function(inverse) mcm <<-inverse #set the value of the inverse of the matrix
        getinverse <- function() mcm #get the value of the inverse of the matrix
        list(set = set, get = get, #creates a list to include the four functions created above
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #I want to compare the matrix to what was there before
        mcm <- x$getinverse() #evaluates if an inverse has already been calculated and gets it
        if (!is.null(mcm)) {  #check to see if cacheSolve has been run before
                message("getting cached inverse matrix")
                return(mcm)
        } else {
                mcm <- solve(x$get()) #compute the value of the inverse of the input matrix
                x$setinverse(mcm) #run the setinverse function on the inverse to cache the inverse
                return(mcm) #return the inverse
        }
}
