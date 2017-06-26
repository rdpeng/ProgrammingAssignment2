makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
                #creating empty matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
                #creating environment for calculation
        get <- function() x
                #input of the called matrix x
        setinverse <- function(solve) m <<- solve
                #setinverse calculates the inverse of x
        getinverse <- function() m
                #getinverse shows the inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                #the list command prepares the final output
}

#This function `makeCacheMatrix` creates a list 
#containing a #function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

#Works under the assumption that the Input Matrix is 
#a square invertible matrix.
