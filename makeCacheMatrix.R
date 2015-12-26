#makeCacheMatrix function creates an object that is an envioronment whose purpose is to store the
# inverse of a matrix that the user supplies
#makeCacheMatrix also returns a list with four functions that can be used to access/edit the supplied
# matrix and inverse
#Functions returned in the list are
#       set = user can enter a matrix and the stored matrix will be updated, function takes a matrix variable
#       get = user can return the stored matrix, function takes no variables
#       setinverse = user can enter an inverse matrix (not recommended, instead use the companion
#        function, cacheSolve.R to calculate the inverse matrix)
#       getinverse = user can return the stored inverse matrix, function takes no variables
makeCacheMatrix <- function(p = matrix()) {
        thematrix <- NULL
        set <- function(y) {
                p <<- y
                thematrix <<- NULL
        }     
        get <- function() p
        setinverse <- function(theinverse) thematrix <<- theinverse
        getinverse <- function() thematrix
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


#cacheSolve function takes as input the list of functions output from makeCacheMatrix and does one of 2 things:
#       1. If there is an inverse matrix already calcuated & stored, it returns this inverse
#       2. If there is no inverse matrix already calcuated & stored, it calculates the inverse and returns it
cacheSolve <- function(p, ...) {
        thematrix <- p$getinverse()
        if(!is.null(thematrix)) {
                message("getting cached data")
                return(thematrix)
        }
        data <- p$get()
        thematrix <- solve(data, ...)
        p$setinverse(thematrix)
        thematrix
}