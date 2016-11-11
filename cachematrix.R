## Programming Assignment 2:  Lexical Scoping
## As outlined in the given description of this assignment, the
## solution presented here will take advantage of the scoping rules of the 
## R language and how how they can be manipulated to preserve state inside of 
## an R object.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invrse <-NULL
        set <- function(y) {
                x <<- y
                invrse <<- NULL
        }
        get <- function() x
        setinvrse <- function(inverse) invrse <<- inverse
        getinvrse <- function() invrse
        list(set=set, get=get, setinvrse=setinvrse, getinvrse=getinvrse)
}
        
                
## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix` above. If the inverse has already been calculated (and the 
#matrix has not changed), then `cacheSolve` should retrieve the inverse from 
#the cache.


cacheSolve <- function(x, ...) {
        invrse <- x$getinvrse()
        if (!is.null(invrse)) {
                message("getting cached data.")
                return(invrse)
        }
        info <- x$get()
        invrse <- solve(info)
        x$setinvrse(invrse)
        invrse
}

