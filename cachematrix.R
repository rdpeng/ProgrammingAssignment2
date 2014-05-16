## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix - Parent function of set,get functions for the matrix and its inverse. 
# cacheSolve - Function that computes/retrieve the inverse of the matrix of interest.

## Write a short comment describing this function

############################################################################
#This function set/get matrix and its inverse
makeCacheMatrix <- function(x = matrix()) { 
        
        inverse<-NULL 
        
        set <- function(y) {                    # creates the matrix
                x <<- y
                inverse <<- NULL
        }
        
        get<-function() x                       #retrieves the matrix
        
        setinverse<-function(incominginverse) { #set the inverse
                inverse<<-incominginverse
        }
        getinverse<- function() inverse         #retrieves the inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

############################################################################
## Write a short comment describing this function
# This function computes/retrieves the inverse of the matrix, depending on whether it is available in the cache.
cacheSolve <- function(x, ...) {
        
        inverse<-x$getinverse() #retrieves inverse value from makeCacheMatrix
        
        if(!is.null(inverse)) { #if inverse has a value, return the inverse
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get() #else, retrieves the matrix from makeCacheMatrix, and compute the inverse
        inverse <- solve(data) #calculates the mean
        x$setinverse(inverse)
        inverse
}
