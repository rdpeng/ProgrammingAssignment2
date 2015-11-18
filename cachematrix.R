## function 'makecacheMatrix' creates, sets and returns a matrix
makecacheMatrix <- function(x = matrix()){

    m <- NULL

    set <- function(y){ #Changes the matrix stored
        x <<- y         #Substitutes the matrix x with y (the input)
        m <<- NULL      #Restores to null the value of the solved matrix "m"
    }
    get <- function() x #Return the matrix (x); example use is:
                           #1) assign: a <- makecacheMatrix(matrix), 2) call: a$get()

    setmatrix <- function(solve) m <<- solve #Stores solved matrix
    getmatrix <- function() m #Retrieves the solved matrix
    list(set = set, get = get, #List enables the the assignment of this function to an
         setmatrix = setmatrix,  #object, to provide these functions to that object
         getmatrix = getmatrix)
}

## function 'cacheSolve' returns the inverse of a matrix when called from 'makecachematrix'
cacheSolve <- function(x = matrix(), ...){
    m <- x$getmatrix()

    if(!is.null(m)){ #checking to determine is NULL True or False
        message("getting cached data") #if False, data gets the matrix stored with makecacheMatrix
        return(m) #'m' returned as solved
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
