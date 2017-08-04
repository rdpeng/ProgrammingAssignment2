## Overall description: the goal of these functions combined is to get the inverse of a matrix. 
## However, calculating the inverse of a matrix is demanding and may take a lot of time.
## Therefore,it is efficient and useful to not having to calculate the inverse more than once.
## So after calculating the inverse, it can be stored in cache. This code tries to retreive this.
## If the inverse matrix is not in cache, the inverse will be calculated and stored in cache.

## The first function can be considered as a tool, in which all the necessary elements 
## are specified that the second function needs. In this first function A "matrix" object is created 
## that caches its inverse. To make sure the next function can actually access the elements it needs,
## this function uses the advantages of lexical scoping to set certain parts to the 'parent environment'.
## These can be recognized by the '<<-'in the code.

makeCacheMatrix <- function(x = matrix()) { #has a default "matrix()", to prevent having a 'missing' error
          i <- NULL
        set <- function(y) { 
          x <<- y
          i <<- NULL # value of i is cleared (in case cache run before). 
        }
        get <- function() x 
        setinverse <- function(solve) i <<- solve # i needs to be available after the function completes!
        getinverse <- function() i 
        list(set = set, get = get, # functions are put in a list, so the $function can be used by cachemean
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not been changed) 
## then the cacheSolve should retrieve the inverse from cache. If it is not in cache,
## the function computes the inverse of the matrix and stores it in the parent environment.


cacheSolve <- function(x= matrix(), ...) {
            i <- x$getinverse() #the function tries to retrieve the inverse of the object
            if(!is.null(i)) { #is the result null? if not the inverse is cached
              message("getting cached data")
              return(i)
            }
            data <- x$get() #if the result was null, the inverse is calculated and stored in the parent environment
            i <- solve(data, ...)
            x$setinverse(i) 
            i
          
  
}
