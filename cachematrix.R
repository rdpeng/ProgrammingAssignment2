## Put comments here that give an overall description of what your
## functions do

#' Henrique: In this assigment we are supposed to generate two functions, one
#' that creates a matrix of data and a second one that receives this matrix 
#' generated and invert it.

#' In order to have an inverse matrix, two conditions need to be achieved:
#' 1-The matrix need to have equal number of rows and columns
#' 2-The determant must be different than 0. 

## Write a short comment describing this function
#' This function will receive a matrix as input and use it to generate a list of
#' 4 elements. 'set' retrieve the value of the matrix and assign it to variable 
#' x that will be recovered by 'get'.
#' 'setinverse' will generate the element that will be recovered and stored in 
#' 'getinverse', this last one will be used in the check of the next function.
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y){
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inverse <- inverse
    getinverse <- function() m_inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
#' This function is easier, it will simple check if the inverse was already 
#' calculated and if it was, return it. If not, it will solve it and return 
#' the value.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if (!is.null(inverse_matrix)){
        print("Inverse calculated, retrieving...")
        inverse_matrix
    }
    normal_matrix <- x$get()
    solve(normal_matrix)
}
