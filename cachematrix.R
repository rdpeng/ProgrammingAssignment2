##  These two functions work together to cache the solved matrix inverse for
## to save repeated computation. The first function returns a list of four 
## functions, and this output will be the input of the second function.

## The first function makeCasheMatrix() creates a special "matrix" object as
## the default value of input x. The output is four functions for
## cacheSolve() to subset and use.


makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){ ### setting the value of x and I in the global Env
                x <<- y
                I <<- NULL
        }
        get <- function(){x} ### getting the value of x
        setInverse <- function(Inverse){I <<- Inverse} ###Setting the input as Inverse
        getInverse <- function(){I}  ### getting the value of inverse
        list(set = set, get = get,  ### listing these four functions in output for use
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second function computes the inverse of the special "matrix"
## that returned above. But instead of jumping into computing, it would 
# first check if the worked has been done and stored.
# The cashed results would be returned first.

cacheSolve <- function(x, ...) { ## x stores all results of the first function
        I <- x$getInverse()  ## get I by calling getInverse()
        if(!is.null(I)){    ## If I is not null, return the stored value.        
                message("getting cached data")
                return(I)
        }                  ## Else calculate it.   
        data <- x$get()         ### getting the matrix 
        I <- solve(data)        ### solving its inverse
        x$setInverse(I)         ### storing the results for later-on reading
        I
}
