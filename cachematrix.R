## Week3 - Progamming Assignment 2; GitHub user: umas12

##This function makeCacheMatrix creates a special "matrix", which can cache its inverse. Performs 4 functions : 
#set the value of the matrix, get the value of the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {     #takes a matrix as an argument
    inv <- NULL                      #initialized as NULL, will hold the value of the inversed matrix
    setMatrix <- function(y){        #defining the set function to hold the value of the new matrix        
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() {x}        #defining the get function which retruns the value of the function argument
    setInverse <- function(inverse){   #assigns value of inverse
        inv <<- inverse   
    }
    getInverse <- function() {inv}              #gets the value of inverse matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse) #used so as to refer 
                                                                                                         #to the functions with the $ operator
}


##This function cacheSolve computes the inverse of the special matrix retruned by the function makeCacheMatrix above. 
#If the inverse is already calculated (and the matrix has not changed), then the inverse is retrieved by cacheSolve from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()       #calling getInverse from makeCacheMatrix to retrieve inverse
    if(!is.null(inv)) {         #if the inv value was calculated before for the same matrix it would have been cached
        message("getting cached data")
        return(inv)
    }
    data <- x$getMatrix()             #if the inv was NULL, then we compute the inverse of the matrix by getting the matrix from makeCacheMatrix
    inv <- solve(data, ...)
    x$setInverse(inv)           #after computing the inverse it caches the inverse for later.
    inv
}
